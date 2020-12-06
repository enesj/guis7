(ns enesj.flight-booker
  (:require
    [reagent.core :as r]))

(defn flight-booker-component []
  (let [flight (r/atom {:direction  :one-way
                        :start-date "12.02.2020"
                        :end-date   "12.02.2020"})]
    (fn []
      (let [state @flight
            start-date-invalid (or (js/Number.isNaN (js/Date.parse (:start-date state))) (not (= 10 (count (:start-date state)))))
            end-date-invalid (or (js/Number.isNaN (js/Date.parse (:end-date state))) (not (= 10 (count (:end-date state)))))
            submit-disabled (or start-date-invalid end-date-invalid (> (js/Date.parse (:start-date state)) (js/Date.parse (:end-date state))))]
        [:<>
         [:form.pure-form.pure-form-stacked
          [:select#flight-type {:on-change #(swap! flight assoc :direction (keyword (-> % .-target .-value)))}
           [:option {:value :one-way} "One-way flight"]
           [:option {:value :return} "Return flight"]]
          [:input#start-date {:type      "text" :value (:start-date state)
                                          :style     {:color (if start-date-invalid "red" "black")}
                                          :on-change #(swap! flight assoc :start-date (-> % .-target .-value))}]
          [:input#end-date {:type      "text" :value (:end-date state)
                                        :disabled (= :one-way (:direction state))
                                        :style     {:color (if end-date-invalid "red" "black")}
                                        :on-change #(swap! flight assoc :end-date (-> % .-target .-value))}]
          [:input#submit {:type      "submit" :value "Book"
                          :disabled submit-disabled
                          :on-click #(js/alert (str "You have booked a one-way flight on " (:start-date state)))}]]]))))

(defn page []
  [:div {:style {:margin-left 12}}
   [:h2 "Flight Booker"]
   [flight-booker-component]])

