(ns enesj.counter
 (:require
  [reagent.core :as r]))

(defn counting-component []
 (let [click-count (r/atom 0)]
  (fn []
   [:<>
    [:label {:style {:margin "15px 20px 0px 20px"}}
     @click-count]
    [:button {:type "button"
              :on-click #(swap! click-count inc)} "Count"]])))

(defn page []
 [:div {:style {:margin-left 12}}
  [:h2 "Counter"]
  [counting-component]])
