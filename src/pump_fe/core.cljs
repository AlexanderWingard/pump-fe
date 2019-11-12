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
  (let [now (js/performance.now)
        path [s/ALL (s/selected? [s/LAST :sent (s/pred (fn [sent] (>= (- now sent) 5000)))])]]
    (swap! timedout #(apply conj % (s/select [path s/LAST :elem] @requests)))
    (swap! requests #(s/setval path s/NONE %))))

(defn send-edn [timeout-id data]
  (let [uuid (str (random-uuid))]
    (swap! timedout disj timeout-id)
    (swap! requests assoc uuid {:sent (js/performance.now) :elem timeout-id})
    (->> (assoc data :id uuid)
         (clj->js)
         (js/JSON.stringify)
         (ws/send ws)))
  (js/setTimeout check-timeouts 5000))

(defn has-pending-requests [id]
  (not-empty(s/select [s/MAP-VALS (s/selected? :elem (s/pred= id))] @requests)))

(defn button [id fun label]
  [:div.ui.button {:id id
                   :class (string/join " " [(when (has-pending-requests id) "disabled loading")
                                            (when (contains? @timedout id) "red")])
                   :on-click fun}
   label])

(defn text-field [path]
  [:input {:type "text"
           :value (get-in @state path)
           :on-change #(swap! state assoc-in path (-> % .-target .-value))}])

(defn parse-unit [path]
  (when-let [matches (re-matches #"([\d\.]+)\s*(\w*)" (string/trim(str(get-in @state path))))]
    (try (let [[_ value unit] matches
               number (edn/read-string value)]
           (when (number? number)
             (case unit
               "ml" {:ml number}
               "us" {:us number}
               "s" {:us (* 1000000 number)}
               "" {:us  (* 1000000 number)}
               nil)))
         (catch :default e nil))))

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

(defn hello-world []
  [:div.ui.container
   [:div.ui.segment.form
    [:h2 "Pump Configuration"]
    [:div.four.wide.field
     [:label "Pump"]
     [dropdown (for [p (:pumps @state) :let [id (:pump p)]] {:value id :label (str "Pump "id)}) :selected-pump]]
    [:div.two.fields
     [:div.field
      [:label "Run Pump"]
      [:div.ui.action.input
       [text-field [:run-for]]
       [button "btn-run-for" #(send-edn "btn-run-for" (merge (parse-unit [:run-for]) {:msg "run_pump" :pump (:selected-pump @state)})) "Run Pump"]]]
     [:div.field
      [:label "Set Calibration"]
      [:div.ui.action.input
       [text-field [:calibration]]
       [button "btn-calibration" #(send-edn "btn-calibration" (merge (parse-unit [:calibration]) (parse-unit [:run-for]) {:msg "set_cal" :pump (:selected-pump @state)})) "Calibrate Pump"]]]]]
   [button "btn-get-state" #(send-edn "btn-get-state" {:msg "get_state"}) "Get State"]
   [button "btn-get-time" #(send-edn "btn-get-time"{:msg "get_time"}) "Get Time"]
   ])

(defn mount [el]
  (reagent/render-component [hello-world] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-app-element)

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(defn ws-receive [s]
  (let [data (-> s
                 (js/JSON.parse)
                 (js->clj :keywordize-keys true))
        ack (:ack data)]
    (when (some? ack)
      (when (= (:msg data) "error")
        (when-let [elem (get-in @requests [ack :elem])]
          (swap! timedout conj elem)))
      (swap! requests dissoc ack))
    (swap! state #(-> % (merge data)))))

(defn ws-open [])

(defn ws-close [])

(defonce ws (ws/new "ws://localhost:8080/ws"
                    #((var ws-receive) %)
                    #((var ws-open))
                    #((var ws-close))))
