(ns enesj.converter
  (:require
    [reagent.core :as r]))

(defn converter-component []
  (let [temperature (r/atom {:celsius    0
                             :fahrenheit 0})]
     (fn []
      (let [state @temperature]
       [:<>
        [:input {:type      "text" :value (:celsius state)
                             :style {:margin-right 5}
                             :on-change #(do
                                           (swap! temperature assoc :celsius (-> % .-target .-value))
                                           (when-not  (re-find #"[^0123456789-]" (-> % .-target .-value))
                                             (swap! temperature assoc :fahrenheit (+ (* (-> % .-target .-value) (/ 9 5)) 32))))}]
        [:label {:style {:margin-right 5}}
         "Celsius = "]
        [:input {:type      "text" :value (:fahrenheit state)
                             :style {:margin-right 5}
                             :on-change #(do (swap! temperature assoc :fahrenheit (-> % .-target .-value))
                                             (when-not  (re-find #"[^0123456789]" (-> % .-target .-value))
                                               (swap! temperature assoc :celsius (* (- (-> % .-target .-value) 32) (/ 5 9)))))}]
        [:label {:style {:margin-right 5}}
         " Fahrenheit"]]))))
        ;[:pre {:style {:margin-top 150}} (with-out-str (cljs.pprint/pprint state))]]))))

(defn page []
  [:div {:style {:margin-left 12}}
   [:h2 "Temperature Converter"]
   [converter-component]])
