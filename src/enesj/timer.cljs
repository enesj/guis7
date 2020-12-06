(ns enesj.timer
  (:require
    [reagent.core :as r]))

(defn timer-component []
  (let [elapsed-time (r/atom {:time 0
                              :max 300})]
    (js/setInterval #(when (< (:time @elapsed-time) (:max @elapsed-time))
                       (swap! elapsed-time update :time inc))
      100)
    (fn []
        [:<>
         [:label "Elapsed Time:"]
         [:meter {:min   0 :max (:max @elapsed-time)
                  :value (:time @elapsed-time)
                  :style {:width "100%"}}]
         [:label  (/ (:time @elapsed-time) 10)]
         [:input.slider {:type      "range"
                         :min       0
                         :max 1000
                         :value     (:max @elapsed-time)
                         :on-change #(swap! elapsed-time assoc :max (-> % .-target .-value))}]
         [:button {:on-click #(swap! elapsed-time assoc :time 0)}
          " reset"]])))

(defn page []
  [:div {:style {:display "flex" :flex-direction "column" :justify-content "space-between" :width "20%" :height "200px" :margin "10px 0 0 12px"}}
   [:h2 "Timer"]
   [timer-component]])
