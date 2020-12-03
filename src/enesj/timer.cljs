(ns enesj.timer
  (:require
    [reagent.core :as r]))

(defn timer-component []
  (let [elapsed-time (r/atom {:time 0
                              :max 100})]
    (js/setInterval #(when (< (:time @elapsed-time) (:max @elapsed-time))
                       (swap! elapsed-time update :time inc))
      1000)
    (fn []
      (let [state @elapsed-time]
        [:<>
         [:label "Elapsed Time:"]
         [:meter {:min   0 :max (:max @elapsed-time)
                  :value (:time @elapsed-time)
                  :style {:width "100%"}}]
         [:label  (:time @elapsed-time)]
         [:input.slider {:type      "range"
                         :min       0
                         :max 300
                         :value     (:max @elapsed-time)
                         :on-change #(swap! elapsed-time assoc :max (-> % .-target .-value))}]
         [:button {:on-click #(swap! elapsed-time assoc :time 0)}
          " reset"]]))))
         ;[:pre {:style {:margin-top 150}} (with-out-str (cljs.pprint/pprint @elapsed-time))]]))))

(defn page []
  [:div {:style {:display "flex" :flex-direction "column" :justify-content "space-between" :width "20%" :height "200px" :margin "10px 0 0 12px"}}
   [:h2 "Timer"]
   [timer-component]])
