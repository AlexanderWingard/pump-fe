(ns ^:figwheel-hooks pump-fe.core
  (:require
   [goog.dom :as gdom]
   [axw.ws :as ws]
   [reagent.core :as reagent :refer [atom]]
   [cljsjs.semantic-ui :as sem]
   [com.rpl.specter :as s]
   [clojure.edn :as edn]
   [clojure.string :as string]
   [cljs.pprint :as pprint]
   ))

(declare ws)

(defn multiply [a b] (* a b))

(defonce state (atom {:alk-view "kh"}))

(defn example []
  (reset! state
          {:pumps [{:pump 1, :minute 0, :ml_per_us 0.000001, :schedule [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]}
                   {:pump 2, :minute 12, :ml_per_us 0, :schedule [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]}
                   {:pump 3, :minute 24, :ml_per_us 0, :schedule [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]}
                   {:pump 4, :minute 36, :ml_per_us 0, :schedule [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]}
                   {:pump 5, :minute 48, :ml_per_us 0, :schedule [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]}]}))

(defn two-decimals [n]
  (pprint/cl-format nil  "~,2f" n))

(defn get-app-element []
  (gdom/getElement "app"))

(defn check-timeouts []
  (swap! state assoc :last-heard-since (int (- (/ (js/Date.now ) 1000) (:last-heard @state))))
  (let [now (js/performance.now)]
    (swap! state (fn [state]
                   (s/transform [:requests s/MAP-VALS
                                 (s/selected? [:msg (s/pred= "pending")])
                                 (s/selected? [:sent (s/pred (fn [sent] (>= (- now sent) 5000)))])]
                                #(merge % {:msg "error" :error "Request timed out"})
                                state)))))

(defonce timeout-checker (js/setInterval #((var check-timeouts)) 1000))

(defn animate []
  (swap! state (fn [state]
                 (s/transform [:running s/MAP-VALS]
                              (fn [{:keys [start us] :as x}]
                                (let [now (js/performance.now)
                                      duration (- now start)
                                      total (/ us 1000)]
                                  (if (> duration total)
                                    s/NONE
                                    (assoc x :percent (/ duration total)))))
                              state))))
(defonce animation-timer (js/setInterval #((var animate)) 100))

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
                  :checked (contains? checked value)
                  :on-click #(let [target (.-target %)]
                               (swap! state update-in path (fn [old] ((if (.-checked target) conj disj) (set old) (edn/read-string (.-value target))))))}]
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

(defn progress [id]
  (reagent/create-class
   {:component-did-mount (fn [this] (.progress (js/$ (reagent/dom-node this))))
    :component-did-update (fn [this]
                            (let [jq (js/$ (reagent/dom-node this))]
                              (if-let [p (get-in @state [:running id :percent])]
                                (.progress jq #js{:percent (* 100 p)})
                                (.progress jq #js{:percent 0}))))
    :reagent-render
    (fn [id]
      @state
      [:div.ui.indicating.progress
       [:div.bar]])}))

(defn pump-progresses [pumps]
  [:<>
   (for [p pumps]
     ^{:key (:pump p)}
     [:div
      [:label "Pump " (:pump p) " Speed: " (two-decimals(* 1000000(:ml_per_us p))) "ml/s  Dosed: " (two-decimals(:dosed p)) "ml" ]
      [progress (:pump p)]])])

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
                       :pump (:selected-pump state)
                       :nocount (boolean (:ignore-dosed-checkbox state))}
                      (select-keys (:run-for state) [:us :ml])))
   (fn [data]
     (when (= (:msg data) "pump_started")
       (swap! state assoc-in [:requests :run-pump-button] {:msg "ok"})))))

(defn ignore-dosed-checkbox []
  [:div.field
   [:div.ui.checkbox
    [:input {:id :ignore-dosed-checkbox
             :type "checkbox"
             :checked (:ignore-dosed-checkbox @state)
             :on-click #(let [target (.-target %)]
                          (swap! state assoc :ignore-dosed-checkbox (.-checked target)))}]
    [:label {:style {:cursor "pointer" :font-size "16px"} :for :ignore-dosed-checkbox} "Dont Count"]]])

(defn stop-pump-button []
  (action-button
   [:requests :stop-pump-button]
   "Stop Pump"
   (fn [state] (merge {:msg "stop_pump"
                       :pump (:selected-pump state)}))
   (fn [data]
     (when (= (:msg data) "pump_stopped")
       (swap! state assoc-in [:requests :stop-pump-button] {:msg "ok"})))))

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
   (fn [state] {:msg "set_sched"
                :pumps (:selected-pumps state)
                :schedule (map edn/read-string (vals (into (sorted-map) (:schedule-field state))))})
   (fn [data]
     (swap! state assoc :loaded-schedule (map edn/read-string (vals (into (sorted-map) (:schedule-field @state))))))))

(defn disable-button []
  (action-button
   [:requests :disable-button]
   "Disable Pumps"
   (fn [state] {:msg "disable"
                :pumps (:selected-pumps state)
                :disable (edn/read-string (:disable state))})
   (fn [data]
     (send {:msg "get_state"}))))

(defn reset-dosed-button []
  (action-button
   [:requests :reset-dosed-button]
   "Reset Dose Counter"
   (fn [state] {:msg "reset_dosed"
                :pumps (:selected-pumps state)})
   (fn [data]
     (send {:msg "get_state"}))))

(defn save-minutes-button []
  (action-button
   [:requests :save-minutes-button]
   "Save Minutes"
   (fn [state] {:msg "set_spread"
                :minutes (map edn/read-string (vals (into (sorted-map) (:spread state))))})
   nil))

(defn load-state-button []
  (action-button
   [:requests :load-state-button]
   "Load State"
   (fn [state] {:msg "get_state"})
   (fn [data]
     (swap! state assoc-in [:requests :load-state-button] {:msg "ok"}))))

(defn load-alk-button []
  (action-button
   [:requests :load-alk-button]
   "Load Alk"
   (fn [state] {:msg "get_alk"})
   nil))

(defn load-schedule-button []
  (action-button
   [:requests :load-schedule-button]
   "Load Schedule"
   (fn [state] {:msg "get_state"})
   (fn [data]
     (when (some? (:pumps data))
       (let [schedules (s/select [:pumps s/ALL (s/selected? [:pump #(contains? (:selected-pumps @state) %)])
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
                                  ( assoc :loaded-schedule (first schedules))
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

(defn disable-form []
  [:div.inline.field
   [:label "Number of periods:"]
   [:div.ui.big.fluid.input
    [text-field [:disable]]]])

(defn spread-form [items path]
  [:div.ui.stackable.four.column.grid {:style {:margin-bottom "1em" :margin-top "1em"}}
   (for [c-1 (range (Math.ceil (/ (count items) 3)))]
     ^{:key c-1}[:div.column
                 [:div.ui.three.column.grid
                  (for [n (range 3)
                        :let [ix (+ n (* c-1 3))]]
                    (when (< ix 5)
                      ^{:key ix}[:div.column
                                 [:div.inline.field
                                  [:label (str (:label (nth items ix)) ":")]
                                  [:div.ui.big.fluid.input
                                   [text-field (conj path ix)]]]]))]])])

(defn group-alk [data]
  (into {} (for [[k v] (group-by #(select-keys (:time %) [:year :month :day]) data)]
             [k (group-by #(select-keys (:time %) [:hour]) v)])) )

(defn partition-loaded [split]
  (-> (reduce (fn [acc [hr amt]]
                (let [g (first (:groups acc))
                      acc (if (and (some? g) (>= hr g))
                            (do
                              (-> acc
                                  (update :res conj '())
                                  (update :groups rest)))
                            acc)]
                  (update acc :res (fn [l]
                                     (conj (rest l) (conj (first l) amt))))))
              {:groups split :res '()}
              (map-indexed vector (:loaded-schedule @state)))
      (:res)
      (reverse)))

(defn alk-component []
  [:div.ui.segment.form
   [:h2.ui.header "Alkatronic"]
   [radio-group [{:value "kh" :label "kH"}{:value "ph" :label "pH"}] [:alk-view]]
   (when (count (:alk @state))
     (let [g (:alk @state)
           cols (keys(second(second g)))
           key (case (:alk-view @state) "kh" :kh "ph" :ph nil)
           diff (case (:alk-view @state) "kh" :kh-diff "ph" :ph-diff nil)]
       [:table.ui.compact.celled.unstackable.table
        [:thead
         [:tr
          [:th "Date"]
          (for [t cols]
            ^{:key t}[:th (pprint/cl-format nil  "~2,'0d:00" (:hour t))])]]
        [:tbody
         (for [[date row] g]
           ^{:key date}
           [:tr
            [:td [:div [:strong(str (:month date) "-" (:day date))]]
             [:div  (two-decimals (let [nums (map #(get (first(last %)) key) row)]
                                   (/(reduce + nums) (count nums))))]
             [:div (two-decimals (reduce + (map #(get (first(last %)) diff) row)))]
             ]
            (for [c cols]
              ^{:key c}[:td
                        [:div(get (first(get row c)) key)]
                        (when (some? (get (first(get row c)) key))
                          [:div (two-decimals (get (first(get row c)) diff))])])])
         (when (some? (:loaded-schedule @state))
           [:tr
            [:td [:strong "Dose"]]
            (for [h (partition-loaded (rest(map :hour cols)))]
              [:td (reduce + h)])])]]))])



(defn main-page []
  [:div.ui.container
   [:div.ui.segment
    [:button.ui.button {:on-click #(reset! state nil)} "Load empty"]
    [load-state-button]
    [load-alk-button]]
   [alk-component]
   (when (> (count (:pumps @state)) 0)
     [:<>
      [:div.ui.segment.form
       [:h2.ui.header "Pump Info"]
       [:div (:boot @state)]
       [:div (:last-heard-since @state)]
       [pump-progresses (:pumps @state)]]
      [:div.ui.segment.form
       [:h2.ui.header "Configure Single Pump"]
       [radio-group (pump-menu-items) [:selected-pump]]
       [:h3.ui.dividing.header "Run Pump"]
       [unit-field "Amount:" "s" [:run-for]]
       [ignore-dosed-checkbox]
       [run-pump-button]
       [stop-pump-button]
       [:h3.ui.dividing.header "Calibrate Pump"]
       [unit-field "Amount:" "ml" [:cal-amount]]
       [set-cal-button]]
      [:div.ui.segment.form
       [:h2.ui.header "Configure Multiple Pumps"]
       [checkbox-group (pump-menu-items) [:selected-pumps]]
       [:h3.ui.dividing.header "Reset Dosed"]
       [reset-dosed-button]
       [:h3.ui.dividing.header "Disable Pumps"]
       [disable-form]
       [disable-button]
       [:h3.ui.dividing.header "Dosing Schedule"]
       [load-schedule-button]
       [schedule-form]
       [save-schedule-button]
       [:h3.ui.dividing.header "Dosing Minutes"]
       [spread-form (pump-menu-items) [:spread]]
       [save-minutes-button]]
      ])])

(defn mount [el]
  (reagent/render-component [main-page] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))

(defn start-timers-for-runnig [state data]
  (update state :running merge
          (reduce (fn [m {:keys [pump running us]}]
                    (assoc m pump {:start (- (js/performance.now) (/ running 1000)) :us us}))
                  {}
                  (s/select [:pumps s/ALL (s/selected? (s/must :us))] data))))

(defn add-diffs [alk]
  (map (fn [a b]
         (if (some? b)
           (-> a
               (assoc :kh-diff (- (:kh a) (:kh b)))
               (assoc :ph-diff (- (:ph a) (:ph b))))
           a))
       alk (cons nil alk)))


(defn convert-dates [data]
  (map (fn [entry] (update entry :time (fn [ts] (let [d (js/Date. (* ts 1000))]
                                                  ;; (.setMinutes d (- (.getMinutes d) (.getTimezoneOffset d)))
                                                  {:year (.getFullYear d)
                                                   :month (+ 1 (.getMonth d))
                                                   :day (.getDate d)
                                                   :hour (.getHours d)
                                                   :minute (.getMinutes d)
                                                   :seconds (.getSeconds d)}
                                                  )))) data))

(defn react-to [data]
  (swap! state merge {:last-heard-since 0 :last-heard (/ (js/Date.now) 1000)})
  (cond
    (= (:msg data) "pump_started")
    (do
      (swap! state assoc-in [:running (:pump data)] {:start (js/performance.now) :us (:us data)})
      (swap! state (fn [state] (s/setval [:pumps s/ALL (s/selected? [:pump (s/pred= (:pump data))]) :dosed] (:dosed data) state))))

    (= (:msg data) "pump_stopped")
    (do
      (swap! state (fn [state] (s/setval [:running (:pump data)] s/NONE state)))
      (swap! state (fn [state] (s/setval [:pumps s/ALL (s/selected? [:pump (s/pred= (:pump data))]) :dosed] (:dosed data) state))))

    (= (:msg data) "skipped")
    (swap! state (fn [state] (s/setval [:pumps s/ALL (s/selected? [:pump (s/pred= (:pump data))]) :disabled] (:disabled data) state)))

    (some? (:alk data))
    (swap! state assoc :alk (-> (:alk data)
                                (convert-dates)
                                (add-diffs)
                                (group-alk)))

    (some? (:pumps data))
    (swap! state #(-> %
                      (start-timers-for-runnig data)
                      (merge (dissoc data :ack))
                      (assoc :spread (apply merge (map-indexed hash-map (map (fn [x] (str (:minute x))) (:pumps data)))))))))

(defn ws-receive [s]
  (let [data (-> s
                 (js/JSON.parse)
                 (js->clj :keywordize-keys true))
        ack (:ack data)
        req-path [:requests s/MAP-VALS (s/selected? [:id (s/pred= ack)])]]
    (doseq [cb (s/select [req-path :cb some?] @state)]
      (cb data))
    (react-to data)
    (when (some? ack)
      (swap! state (fn [state] (s/setval req-path data state))))))

(defn ws-open []
  (send {:msg "get_alk"})
  (send {:msg "get_state"}))

(defn ws-close [])

(defonce ws (ws/new "ws://bcws.axw.se/ws"
                    #((var ws-receive) %)
                    #((var ws-open))
                    #((var ws-close))))
