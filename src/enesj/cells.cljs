(ns enesj.cells
  (:require
    [cljs.spec.alpha :as s]
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [cljsjs.react-modal]
    [clojure.string :as str]))

(def cells (r/atom {}))

(def columns 5)
(def rows 10)

(defn number-to-letter [num]
  (char (+ num 65)))

(defn letter-to-number [letter]
  (when letter (- (.charCodeAt letter 0) 65)))

(defn column-number [ref]
  (letter-to-number (first ref)))

(defn get-row-column [ref]
  (let [column (column-number ref)
        row (js/parseInt (subs ref 1))]
    [row column]))

(defn ref-to-id [ref]
  (let [[row column] (get-row-column ref)]
    (+ (* row columns) column)))

(defn id-to-ref [id]
  (let [column (mod id columns)
        row (quot id columns)
        letter (number-to-letter column)]
    (str letter row)))

(defn expand-range [cell1 cell2]
  (let [[row1 column1] (get-row-column cell1)
        [row2 column2] (get-row-column cell2)]
    (when (and (>= row2 row1)  (>= column2 column1))
      (->> (for [row (range row1 (inc row2))
                 column (range column1 (inc column2))]
              (str (number-to-letter column) row))
        (mapv ref-to-id)))))

(defn sum* [vals]
  (apply + vals))

(defn prod* [vals]
  (apply * vals))

(defn sub* [vals]
  (apply - vals))

(defn div* [vals]
  (apply / vals))

(defn get-fun [fun-part]
  (let [funs {"=sum" (partial sum*)
              "=prod" (partial prod*)
              "=div" (partial div*)
              "=sub" (partial sub*)}]
     (funs fun-part)))


(defn get-ids [arg-part]
  (if (and arg-part (.includes arg-part ":") (= 2 (count (str/split arg-part #":"))))
    (apply expand-range (str/split arg-part #":"))
    (mapv ref-to-id (str/split arg-part #","))))

(defn parse [expr]
  (let [[fun-part arg-part] (str/split expr #"\(|\)")
        fun (get-fun fun-part)
        ids (get-ids arg-part)]
    (when (and fun (seq ids))
      [fun ids])))

(defn get-id-value [id]
  (let [kw-id (keyword (str id))
        output-state (js/parseFloat (:output-state (kw-id (:state @cells))))
        input-state  (:input-state (kw-id (:state @cells)))]
    {:output-state output-state :input-state input-state}))


(defn transform-state [expr]
  (if-let [[fun ids] (parse expr)]
    (let [vals (map #(get-id-value %) ids)
          num-vals (map :output-state vals)]
      (fun num-vals))
    expr))





(defn create-cell [left top x y width height row column]
  (let [id (+ (* row columns) column)
        kw-id (keyword (str id))
        cell-state (kw-id (:state @cells))
        {:keys [input-state output-state]} cell-state
        show-input (= id (:selection @cells))]
    (if show-input
      ^{:key id} [:input {:style {:left (+ x left) :top (+ y top) :width (* width 0.8) :height (* height 0.8)   :row row :column column  :stroke-width  0.2 :position "absolute"}
                          :id id
                          :type "text"
                          :value input-state
                          :on-blur #(when-not (parse ( -> % .-target .-value)) (swap! cells assoc-in  [:state kw-id :input-state] output-state))
                          :on-key-down #(when (= (.-key %) "Enter")
                                          (swap! cells assoc :selection nil)
                                          ( swap! cells assoc-in  [:state kw-id :output-state] ( -> % .-target .-value)))
                          :on-change #(do
                                        (swap! cells assoc-in  [:state kw-id :input-state] ( -> % .-target .-value)))}]
      ^{:key id} [:div {:style {:left (+ x left) :top (+ y top) :width width :height height :border "0.2px solid gray" :position "absolute" :display "flex" :justify-content "center" :align-items "center"}
                        :on-mouse-down #(swap! cells assoc :selection id)} output-state])))

(defn create-cells [left top full-width full-height]
  (let [width (/ full-width columns)
        height (/ full-height rows)]
    (doall (for [row (range rows)
                 column (range columns)]
             (let [x (* column width)
                   y (* row height)]
               (create-cell left top x y width height row column))))))

(defn init-state []
  (reset! cells
    (hash-map
      :selection nil
      :state
      (into {} (doall (for [row (range rows)
                            column (range columns)]
                        (let [id (+ (* row columns) column)]
                          (hash-map
                            (keyword (str id))
                            {:input-state ""
                             :output-state ""}))))))))

(init-state)

(defn cells-component []
  (let [cells (r/atom {})]
     (fn []
       [:<>
        ;[:svg {:width "800px" :height "500px" :stroke "black"}]
        (create-cells 100 150 500 500)])))


(defn page []
  [:div
   [:h2 "Cells"]
   [cells-component]])
