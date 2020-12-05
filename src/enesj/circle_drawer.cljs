(ns enesj.circle-drawer
  (:require
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [cljsjs.react-modal]
    [clojure.string :as str]))


(def abs js/Math.abs)
(def sqrt js/Math.sqrt)
(def pow2 #(js/Math.pow % 2))

(defn get-distance [x1 y1 x2 y2]
  (sqrt (+ (pow2 (abs (- x1 x2)))
          (pow2 (abs (- y1 y2))))))

(defn get-closest [x y circles]
  (when (seq circles)
    (->> (mapv #(vector (get-distance x y (:cx %) (:cy %)) (:id %) (:r %)) circles)
      (reduce (fn [m v]
                (if (< (v 0) (m 0))
                  v
                  m))))))

(defn slider [circles selection]
  [:input.slider {:type      "range"
                  :min       5
                  :max       100
                  :style     {:height 2 :width        150}
                  :value     (:r (first (filter #(= @selection (:id %)) @circles)))
                  :on-change #(reset! circles
                                (set (mapv
                                       (fn [x]
                                         (if (= @selection (:id x)) (assoc x :r (-> % .-target .-value)) x))
                                       @circles)))}])

(defn modal [modal selection circles history svg x y show]
  [(r/adapt-react-class js/ReactModal)
   {:contentLabel   "Adjust Diameter"
    :ariaHideApp    false
    :style          {:content {:left         @x
                               :top          @y
                               :height       (if @show 50 25)
                               :width        160
                               :padding      5
                               :border-style "groove"}}
    :isOpen         @modal
    :onRequestClose #(do
                       (reset! modal false)
                       (reset! show false)
                       (swap! history (fn [x] (-> x
                                                (update :states (fn [y] (conj (vec (take (:pointer x) y))  @circles)))
                                                (assoc :pointer (inc (:pointer x))))))
                       (let [[d id r] (get-closest
                                        (- (.-clientX %) (.-left (-> @svg .getBoundingClientRect)))
                                        (- (.-clientY %) (.-top (-> @svg .getBoundingClientRect)))
                                        @circles)]
                         (if (> r d)
                           (reset! selection id))))}
   (if @show
     [:div "Adjust Diameter"
      (slider circles selection)]
     [:div {:style    {:width 160 :height 20}
            :on-click #(reset! show true)}
      "Adjust Diameter"])])

(js/ReactModal.setAppElement (.getElementById js/document "modal"))

(defn create-circle [x y state-atom selection]
  (let [id (str x ", " y)]
    (swap! state-atom  #(conj % {:cx x :cy y :id id :r 20 :fill "gray" :fill-opacity 0}))
    (reset! selection id)))


(defn draw-circles [state selection]
  (doall (for [circle state]
           (let [id (:id circle)
                 selected-circle (if (= id selection)
                                   (assoc circle :fill-opacity 0.2)
                                   circle)]
             ^{:key (:id circle)} [:circle  selected-circle]))))

(defn circles-component []
  (let [circles (r/atom #{})
        selection (r/atom nil)
        history (r/atom {:pointer 0
                         :states  []})
        slider (r/atom false)
        show-slider (r/atom false)
        svg (r/atom nil)
        x (r/atom 0)
        y (r/atom 0)]
    (fn []
      (let [state @circles
            selected @selection]

        [:<>
         [:div {:style {:display "flex" :flex-direction "row" :justify-content "center"  :width "800px"}}
          [:button  {:type "button" :style {:margin "0px 10px 10px 0px"}
                     :disabled (>= 0 (:pointer @history))
                     :on-click #(do
                                  (swap! history update :pointer dec)
                                  (reset! circles (set (get (:states @history) (dec (:pointer @history))))))} "Undo"]
          [:button  {:type "button" :style {:margin "0px 0px 10px 0px"}
                     :disabled (>= (:pointer @history) (count (:states @history)))
                     :on-click #(do
                                  (swap! history update :pointer inc)
                                  (reset! circles (get (:states @history) (dec (:pointer @history)))))} "Redo"]]
         [:svg {:width "800px" :height "400px" :stroke "black"}
               (draw-circles state selected)
          [:rect {:x             0 :y 0 :width "800px" :height "400px"
                  :fill          "white"
                  :fill-opacity  0
                  :stroke-width  0.2
                  :stroke        "black"
                  :ref (fn [el] (reset! svg el))

                  :on-mouse-down (fn [e]
                                   (reset! x (.-clientX e))
                                   (reset! y (.-clientY e))
                                   (if selected
                                     (reset! slider true)
                                     (do
                                       (create-circle
                                         (- (.-clientX e) (.-left (-> @svg .getBoundingClientRect)))
                                         (- (.-clientY e) (.-top (-> @svg .getBoundingClientRect)))
                                         circles
                                         selection)
                                       (swap! history #(-> %
                                                         (update :states (fn [x] (conj (vec (take (:pointer %) x))  @circles)))
                                                         (assoc :pointer (inc  (:pointer %))))))))
                  :on-mouse-move (fn [e]
                                   (let [[d id r] (get-closest
                                                    (- (.-clientX e) (.-left (-> @svg .getBoundingClientRect)))
                                                    (- (.-clientY e) (.-top (-> @svg .getBoundingClientRect)))
                                                    state)]
                                     (if (> r d)
                                       (reset! selection id)
                                       (reset! selection nil))))}]]
         (modal slider selection circles history svg x y show-slider)
         [:pre {:style {:margin-top 50}} (with-out-str (cljs.pprint/pprint @history))]]))))

(defn page []
  [:div
   [:h2 "Circle Drawer"]
   [circles-component]])
