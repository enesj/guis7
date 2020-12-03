(ns ^:figwheel-hooks enesj.guis7
  (:require
   [goog.dom :as gdom]
   [reagent.core :as r :refer [atom]]
   [reagent.dom :as rdom]
   [reitit.frontend :as rf]
   [reitit.frontend.easy :as rfe]
   [reitit.coercion.spec :as rss]
   [spec-tools.data-spec :as ds]
   [enesj.counter :as counter]
   [enesj.converter :as converter]
   [enesj.flight-booker :as flight-booker]
   [enesj.timer :as timer]
   [enesj.crud :as crud]
   [enesj.circle-drawer :as circle-drawer]
   [enesj.cells :as cells]))


(defonce match (r/atom nil))

(defn current-page []
  [:div {:style {:margin 50}}
   [:div.pure-menu.pure-menu-horizontal
    [:ul.pure-menu-list
     [:li.pure-menu-item [:a.pure-menu-link {:href (rfe/href ::counter)} "Counter"]]
     [:li.pure-menu-item [:a.pure-menu-link {:href (rfe/href ::converter)} "Converter"]]
     [:li.pure-menu-item [:a.pure-menu-link {:href (rfe/href ::flight-booker)} "Flight booker"]]
     [:li.pure-menu-item [:a.pure-menu-link {:href (rfe/href ::timer)} "Timer"]]
     [:li.pure-menu-item [:a.pure-menu-link {:href (rfe/href ::crud)} "CRUD"]]
     [:li.pure-menu-item [:a.pure-menu-link {:href (rfe/href ::circle-drawer)} "Circle drawer"]]
     [:li.pure-menu-item [:a.pure-menu-link {:href (rfe/href ::cells)} "Cells"]]]]
   (if @match
     (let [view (:view (:data @match))]
       [view @match]))])
   ;[:pre {:style {:margin-top 150}} (with-out-str (cljs.pprint/pprint @match))]])

(def routes
  [["/counter"
    {:name ::counter
     :view counter/page}]
   ["/converter"
    {:name ::converter
     :view converter/page}]
   ["/flight-booker"
    {:name ::flight-booker
     :view flight-booker/page}]
   ["/timer"
    {:name ::timer
     :view timer/page}]
   ["/crud"
    {:name ::crud
     :view crud/page}]
   ["/circle-drawer"
    {:name ::circle-drawer
     :view circle-drawer/page}]
   ["/cells"
    {:name ::cells
     :view cells/page}]])


(defn init! []
  (rfe/start!
    (rf/router routes {:data {:coercion rss/coercion}})
    (fn [m] (reset! match m))
    ;; set to false to enable HistoryAPI
    {:use-fragment true})
  (rdom/render [current-page] (.getElementById js/document "app")))

(init!)

