(ns ^:figwheel-hooks pump-fe.core
  (:require
   [goog.dom :as gdom]
   [axw.ws :as ws]
   [reagent.core :as reagent :refer [atom]]
   [cljsjs.semantic-ui :as sem]
   [com.rpl.specter :as s]
   [clojure.edn :as edn]
   [clojure.string :as string]
   [clojure.pprint :as pprint]
   ))

(declare ws)

(defn multiply [a b] (* a b))

(defonce state (atom {}))
(defonce requests (atom {}))
(defonce timedout (atom #{}))

(defn example []
  (reset! state
          {:pumps [{:pump 1, :minute 0, :ml_per_us 0.000001, :schedule [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]}
                   {:pump 2, :minute 12, :ml_per_us 0, :schedule [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]}
                   {:pump 3, :minute 24, :ml_per_us 0, :schedule [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]}
                   {:pump 4, :minute 36, :ml_per_us 0, :schedule [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]}
                   {:pump 5, :minute 48, :ml_per_us 0, :schedule [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]}]}))

(defn get-app-element []
  (gdom/getElement "app"))

(defn check-timeouts []
  (let [now (js/performance.now)]
    (swap! state (fn [state]
                   (s/transform [:requests s/MAP-VALS
                                 (s/selected? [:msg (s/pred= "pending")])
                                 (s/selected? [:sent (s/pred (fn [sent] (>= (- now sent) 5000)))])]
                                #(merge % {:msg "error" :error "Request timed out"})
                                state)))))

(defonce timeout-checker (js/setInterval #((var check-timeouts)) 1000))

(defn text-field [path]
  [:input {:type "text"
           :value (get-in @state path)
           :on-change #(swap! state assoc-in path (-> % .-target .-value))}])

(defn parse-unit [s default]
  (merge {:unit default :value s}
         (when-let [matches (re-matches #"([\d\.]+)\s*(\w*)" (string/trim s))]
           (try (let [[_ value unit] matches
                      number (edn/read-string value)]
                  (when (number? number)
                    (let [unit (if (= "" unit) default unit)]
                      (merge {:unit unit}
                             (case unit
                               "ml" {:ml number}
                               "us" {:us number}
                               "s" {:us (* 1000000 number)})))))
                (catch :default e nil)))))

(defn unit-field [label default-unit path]
  [:div.field
   [:label label]
   (let [s (get-in @state path)]
     [:div.ui.big.right.labeled.input
      [:input {:type "text"
               :value (:value s)
               :on-change #(swap! state assoc-in path (parse-unit (-> % .-target .-value) default-unit))}]
      [:div.ui.basic.label (or (:unit s) default-unit)]])])

(defn radio-group [items path]
  (let [checked (get-in @state path)
        group-name (name (last path))]
    [:div.inline.fields
     (for [{:keys [value label]} items
           :let [id (str group-name "-" value)]]
       ^{:key id}
       [:div.field
        [:div.ui.radio.checkbox
         [:input {:id id
                  :type "radio"
                  :name group-name
                  :value value
                  :checked (= checked (str value))
                  :on-click #(let [target (.-target %)]
                               (when (.-checked target)
                                 (swap! state assoc-in path (.-value target))))}]
         [:label {:style {:cursor "pointer" :font-size "16px"} :for id} label]]])]))

(defn checkbox-group [items path]
  (let [checked (get-in @state path)]
    [:div.inline.fields
     (for [{:keys [value label]} items
           :let [id (str "chk-" value)]]
       ^{:key id}[:div.field
        [:div.ui.checkbox
         [:input {:id id
                  :type "checkbox"
                  :value value
                  :checked (contains? checked (str value))
                  :on-click #(let [target (.-target %)]
                               (swap! state update-in path (fn [old] ((if (.-checked target) conj disj) (set old) (.-value target)))))}]
         [:label {:style {:cursor "pointer" :font-size "16px"} :for id} label]]])]))

(defn dropdown [items path]
  (reagent/create-class
   {:component-did-mount (fn [this] (.dropdown (js/$ (reagent/dom-node this))))
    :component-did-update (fn [this]
                            (let [jq (js/$ (reagent/dom-node this))
                                  val (get @state path)]
                              (if (some? val)
                                (.dropdown jq "set selected" val)
                                (swap! state assoc path (.dropdown jq "get value")))))
    :reagent-render (fn [items path]
                      [:select.ui.dropdown
                       {:on-change #(swap! state assoc path (-> % .-target .-value))}
                       (for [{:keys [value label]} items]
                         [:option {:value value} label])])}))

(defn pump-menu-items []
  (for [p (:pumps @state) :let [id (:pump p)]] {:value id :label (str "Pump "id)}))

(defn send [data]
  (let [uuid (str (random-uuid))]
    (->> (assoc data :id uuid)
         (clj->js)
         (js/JSON.stringify)
         (ws/send ws))
    uuid))

(defn action-button [path label extractor cb]
  (let [{:keys [msg error]} (get-in @state path)]
    [:<>
     [:button.ui.button {:on-click #(swap! state assoc-in path {:id (send (extractor @state))
                                                                :msg "pending"
                                                                :sent (js/performance.now)
                                                                :cb cb})
                         :class (case msg
                                  "pending" "disabled loading"
                                  "error" "red"
                                  "ok" "green"
                                  "")}
      label]
     (when (= msg "error")
       [:div.ui.left.pointing.red.basic.label error])]))

(defn run-pump-button []
  (action-button
   [:requests :run-pump-button]
   "Run Pump"
   (fn [state] (merge {:msg "run_pump"
                       :pump (:selected-pump state)}
                      (select-keys (:run-for state) [:us :ml])))
   nil))

(defn set-cal-button []
  (action-button
   [:requests :set-cal-button]
   "Set Calibration"
   (fn [state] (merge {:msg "set_cal"
                       :pump (:selected-pump state)}
                      (select-keys (:cal-amount state) [:us :ml])
                      (select-keys (:run-for state) [:us :ml])))
   nil))

(defn save-schedule-button []
  (action-button
   [:requests :save-schedule-button]
   "Save Schedule"
   (fn [state] {:pumps (:selected-pumps state)
                :schedule (map edn/read-string (vals (into (sorted-map) (:schedule-field state))))})
   nil))

(defn load-state-button []
  (action-button
   [:requests :load-state-button]
   "Load State"
   (fn [state] {:msg "get_state"})
   (fn [data]
     (swap! state (fn [state] (-> state
                                  (merge (dissoc data :ack))
                                  (assoc-in [:requests :load-state-button] {:msg "ok"})))))))

(defn load-schedule-button []
  (action-button
   [:requests :load-schedule-button]
   "Load Schedule"
   (fn [state] {:msg "get_state"})
   (fn [data]
     (when (some? (:pumps data))
       (let [schedules (s/select [:pumps s/ALL (s/selected? [:pump #(contains? (:selected-pumps @state) (str %))])
                                  :schedule] data)]
         (cond (empty? schedules)
               (swap! state assoc-in [:requests :load-schedule-button] {:msg "error" :error "No pumps selected"})

               (not (apply = schedules))
               (swap! state assoc-in [:requests :load-schedule-button] {:msg "error" :error "Schedules not equal"})

               (not (= 24 (count (first schedules))))
               (swap! state assoc-in [:requests :load-schedule-button] {:msg "error" :error "Incomplete schedule"})

               :else
               (swap! state (fn [state]
                              (-> state
                                  ( assoc-in [:requests :load-schedule-button] {:msg "ok"} )
                                  ( assoc :schedule-field (apply merge (map-indexed hash-map (map str (first schedules))))))))))))))

(defn schedule-form []
  [:div.ui.stackable.four.column.grid {:style {:margin-bottom "1em" :margin-top "1em"}}
   (for [c-1 (range 8)]
     ^{:key c-1}[:div.column
      [:div.ui.three.column.grid
       (for [n (range 3)
             :let [ix (+ n (* c-1 3))]]
         ^{:key ix}[:div.column
          [:div.inline.field
           [:label (str ix ":")]
           [:div.ui.big.fluid.input
            [text-field [:schedule-field ix]]]]])]])])

(defn main-page []
  [:div.ui.container
   [:div.ui.segment
    [:button.ui.button {:on-click example} "Load Standard"]
    [:button.ui.button {:on-click #(reset! state nil)} "Load empty"]]
   [:div.ui.segment.form
    [load-state-button]
    (when (> (count (:pumps @state)) 0)
      [:<>
       [:h2.ui.dividing.header "Configure Single Pump"]
       [radio-group (pump-menu-items) [:selected-pump]]
       [:h3.ui.dividing.header "Run Pump"]
       [unit-field "Amount:" "s" [:run-for]]
       [run-pump-button]
       [:h3.ui.dividing.header "Calibrate Pump"]
       [unit-field "Amount:" "ml" [:cal-amount]]
       [set-cal-button]
       [:h2.ui.dividing.header "Configure Multiple Pumps"]
       [checkbox-group (pump-menu-items) [:selected-pumps]]
       [load-schedule-button]
       [schedule-form]
       [save-schedule-button]])]])

(defn mount [el]
  (reagent/render-component [main-page] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))

(defn ws-receive [s]
  (let [data (-> s
                 (js/JSON.parse)
                 (js->clj :keywordize-keys true))
        ack (:ack data)
        req-path [:requests s/MAP-VALS (s/selected? [:id (s/pred= ack)])]]
    (doseq [cb (s/select [req-path :cb some?] @state)]
      (cb data))
    (when (some? ack)
      (swap! state (fn [state] (s/setval req-path data state))))))

(defn ws-open [])

(defn ws-close [])

(defonce ws (ws/new "ws://bcws.axw.se/ws"
                    #((var ws-receive) %)
                    #((var ws-open))
                    #((var ws-close))))
