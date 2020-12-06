(ns enesj.crud
  (:require
    [reagent.core :as r]
    [clojure.string :as str]))

(defn create-item [name surname state-atom]
  (println "ENES " {:name name :surname surname} (:list @state-atom))
  (if
    (seq (filter  #(= % {:name name :surname surname}) (:list @state-atom)))
    (js/alert (str "Person " name " " surname " already exists"))
    (swap! state-atom update-in [:list] #(conj % {:name name :surname surname}))))

(defn delete-item [name surname state-atom]
  (swap! state-atom update-in [:list] #(remove (fn [x] (= x  {:name name :surname surname})) %)))

(defn select-item [full-name state-atom]
  (let [[name surname] full-name]
      (swap!  state-atom assoc :surname surname)
      (swap!  state-atom assoc :name name)
      (swap!  state-atom assoc :selection {:name name :surname surname})))

(defn select-options [state]
  (let [full-list (:list state)
        criteria (:filter state)
        list (if  criteria
               (filter #(str/starts-with? (str/lower-case (:surname %)) (str/lower-case criteria)) full-list)
               full-list)]
    (doall (for [item list]
             (let [{:keys [name surname]} item]
               ^{:key (hash (str name " " surname))} [:option {:value (str name ", " surname) :style {:margin "2px 2px 0px 0px"}} (str name " " surname)])))))

(defn crud-component []
  (let [crud (r/atom {:filter nil :name nil :surname nil :selection nil :list [{:name "Enes" :surname "Jakic"} {:name "Irma" :surname "Jakic"}]})]
    (fn []
      (let [state @crud]
        [:<>
         [:div {:style {:display "flex" :flex-direction "row" :justify-content "space-between" :max-width "40%"}}
          [:label  "Filter prefix"]
          [:input {:type "text"
                   :value (:filter state)
                   :on-change #(swap! crud assoc :filter (-> % .-target .-value))}]]
         [:div {:style {:display "flex" :flex-direction "row" :width "60%" :margin "20px 0px 0px 0px"}}
          [:select {:size      5
                    :style     {:flex "30%"}
                    :on-change #(select-item (str/split (-> % .-target .-value) #", ") crud)}
           (select-options state)]
          [:div {:style {:display "flex" :flex-direction "column" :justify-content "space-between" :flex "10%" :margin "0px 0px 0px 20px"}}
            [:label "Name"]
            [:input {:type "text"
                     :value (:name state)
                     :on-change #(swap! crud assoc :name (-> % .-target .-value))}]
            [:label  "Surname"]
            [:input {:type "text"
                     :value (:surname state)
                     :on-change #(swap! crud assoc :surname (-> % .-target .-value))}]]]
         [:div {:style {:display "flex" :flex-direction "row" :justify-content "space-between" :margin "20px 0px 0px 0px" :width "30%"}}
          [:button  {:type "button"
                     :on-click #(do
                                  (create-item (:name state) (:surname state) crud)
                                  (swap! crud assoc :name nil)
                                  (swap! crud assoc :surname nil))} "Create"]
          [:button   {:type "button"
                      :disabled (not (:selection state))
                      :on-click #(do
                                   (delete-item (:name (:selection state)) (:surname (:selection state)) crud)
                                   (create-item (:name state) (:surname state) crud)
                                   (swap! crud assoc :name nil)
                                   (swap! crud assoc :surname nil)
                                   (swap! crud assoc :selection nil))}
                     "Update"]
          [:button   {:type "button"
                      :disabled (not (:selection state))
                      :on-click #(do
                                   (delete-item (:name state) (:surname state) crud)
                                   (swap! crud assoc :name nil)
                                   (swap! crud assoc :surname nil)
                                   (swap! crud assoc :selection nil))} "Delete"]]]))))

(defn page []
  [:div {:style {:margin-left 12 :max-width "800px"}}
   [:h2 "CRUD"]
   [crud-component]])
