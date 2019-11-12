(ns axw.ws
  (:require [cljs.reader :as reader]
            [goog.net.WebSocket]
            [goog.asserts.AssertionError]
            [goog.events :refer [listen]]
            [goog.net.WebSocket.EventType :refer [MESSAGE OPENED CLOSED]]))

(defrecord ^:private Ws [conn url])

(defn ^:private connect [{:keys [conn url]}]
  (.open conn url))

(defn ^:private handle-on-message [cb event]
  (cb (.-message event)))

(defn ^:private handle-on-close [cb event]
  (cb))

(defn ^:private handle-on-open [cb event]
  (cb))

(defn ^:private ws-uri [resource]
  (let [location (-> js/window .-location)
        host (-> location .-host)
        protocol (-> location .-protocol (case "http:" "ws:" "https:" "wss:"))]
    (str protocol "//" host "/" resource)))

(defn send [{:keys [conn] :as ws} data]
  (try (.send conn data)
       (catch goog.asserts.AssertionError e
         (connect ws))))

(defn new [uri on-message on-open on-close]
  (doto (Ws. (doto (goog.net.WebSocket. true)
               (listen MESSAGE (partial handle-on-message on-message))
               (listen OPENED (partial handle-on-open on-open))
               (listen CLOSED (partial handle-on-close on-close)))
             uri)
    (connect)))
