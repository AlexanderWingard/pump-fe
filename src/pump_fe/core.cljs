(ns ^:figwheel-hooks pump-fe.core
  (:require
   [goog.dom :as gdom]
   [axw.ws :as ws]
   [reagent.core :as reagent :refer [atom]]
   [cljsjs.semantic-ui :as sem]
   [com.rpl.specter :as s]
   [clojure.string :as string]
   [clojure.pprint :as pprint]
   ))

(declare ws)

(defn multiply [a b] (* a b))

(defonce state (atom {}))
(defonce requests (atom {}))
(defonce timedout (atom #{}))

(defn get-app-element []
  (gdom/getElement "app"))

(defn check-timeouts []
  (let [now (js/performance.now)
        path [s/ALL (s/selected? [s/LAST :sent (s/pred (fn [sent] (>= (- now sent) 5000)))])]]
    (swap! timedout #(apply conj % (s/select [path s/LAST :elem] @requests)))
    (swap! requests #(s/setval path s/NONE %))))

(defn send-edn [data]
  (let [id (str (random-uuid))]
    (js/setTimeout check-timeouts 5000)
    (->> (assoc data :id id)
         (clj->js)
         (js/JSON.stringify)
         (ws/send ws))))

(defn button-click [elem-id data]
  (let [id (str (random-uuid))]
    (swap! timedout disj elem-id)
    (swap! requests assoc id {:sent (js/performance.now) :elem elem-id})
    (->> (assoc data :id id)
         (clj->js)
         (js/JSON.stringify)
         (ws/send ws)))
  (js/setTimeout check-timeouts 5000))

(defn has-pending-requests [id]
  (not-empty(s/select [s/MAP-VALS (s/selected? :elem (s/pred= id))] @requests)))

(defn button [id data label]
  [:div.ui.button {:id id
                   :class (string/join " " [(when (has-pending-requests id) "disabled loading")
                                            (when (contains? @timedout id) "red")])
                   :on-click (fn []
                               (let [uuid (str (random-uuid))]
                                 (swap! timedout disj id)
                                 (swap! requests assoc uuid {:sent (js/performance.now) :elem id})
                                 (->> (assoc data :id uuid)
                                      (clj->js)
                                      (js/JSON.stringify)
                                      (ws/send ws)))
                               (js/setTimeout check-timeouts 5000))}
   label])

(defn hello-world []
  [:div
   [button "btn-get-state" {:msg "get_state"} "Get State"]
   [button "btn-get-time" {:msg "get_time"} "Get Time"]
   (for [p (:pumps @state)]
     [:div "pump"])
   [:pre (with-out-str (pprint/pprint @state))]])

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
      (swap! requests dissoc ack))
    (swap! state #(-> % (merge data)))))

(defn ws-open [])

(defn ws-close [])

(defonce ws (ws/new "ws://localhost:8080/ws"
                    #((var ws-receive) %)
                    #((var ws-open))
                    #((var ws-close))))
