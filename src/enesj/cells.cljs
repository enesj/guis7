(ns enesj.cells
  (:require
    [reagent.core :as r]
    [cljsjs.react-modal]
    [com.stuartsierra.dependency :as dep]
    [clojure.string :as str]))

(defonce cells (r/atom {}))

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
    (keyword (str (+ (* row columns) column)))))

(defn transform-deps [deps-map]
  (reduce-kv
    (fn [m k v]
      (into m (mapv #(vector % k) v)))
    [] deps-map))

(defn get-dependants  [id dependencies]
  (try
    (let [g (loop [d (transform-deps (if dependencies
                                       dependencies
                                       (:dependencies @cells)))
                   g (dep/graph)]
              (if (seq d)
                (recur (next d)  (apply dep/depend g (first d)))
                g))]
      (dep/transitive-dependencies g id))
    (catch js/Object e (js/alert  (str "Error: Cyclic" (.-message e))))))

(defn expand-range [cell1 cell2]
  (let [[row1 column1] (get-row-column cell1)
        [row2 column2] (get-row-column cell2)]
    (when (and (>= row2 row1)  (>= column2 column1))
      (->> (for [row (range row1 (inc row2))
                 column (range column1 (inc column2))]
              (str (number-to-letter column) row))
        (mapv ref-to-id)))))

(defn sum* [vals]
  (apply + (remove string? vals)))

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

(defn is-formula [expr]
  (let [[fun-part arg-part] (str/split expr #"\(|\)")
        fun (get-fun fun-part)]
    (if fun [fun arg-part] expr)))

(defn get-id-value [id]
  (let [input-state  (:input-state (id (:state @cells)))]
    (if (js/Number.isNaN  (js/parseFloat input-state))
        [input-state id]
        [(js/parseFloat input-state) id])))

(defn transform-state [expr kw-id]
  (if (coll? (is-formula expr))
    (let [[fun-part arg-part] (is-formula expr)
          ids (get-ids arg-part)
          vals (map get-id-value ids)]
      (when (get-dependants kw-id (assoc-in (:dependencies @cells) [kw-id] ids))
        (swap! cells assoc-in [:dependencies kw-id] ids)
        (fun-part (map #(apply transform-state %) vals))))
    expr))


(defn input-to-output [input kw-id]
  (swap! cells assoc-in  [:state kw-id :output-state] (transform-state input kw-id)))

(defn create-cell [left top x y width height row column]
  (let [id (+ (* row columns) column)
        kw-id (keyword (str id))
        cell-state (kw-id (:state @cells))
        {:keys [input-state output-state]} cell-state
        show-input (= id (:selection @cells))]

    (if show-input
      ^{:key id} [:input {:style       {:left (+ x left 6) :top (+ y top 3) :width (* width 0.8) :height (* height 0.8)  :row row :column column :stroke-width 0.2 :position "absolute"}
                          :id          id
                          :type        "text"
                          :value       input-state
                          :on-blur     #(when
                                          (= (is-formula (-> % .-target .-value)) (-> % .-target .-value))
                                          (swap! cells assoc-in [:state kw-id :input-state] output-state))
                          :on-key-down #(when (= (.-key %) "Enter")
                                          (swap! cells assoc :selection nil)
                                          (input-to-output (-> % .-target .-value) kw-id)
                                          (let [dependants (get-dependants kw-id nil)]
                                            (doseq [dependant-id dependants]
                                              (input-to-output (-> @cells :state dependant-id :input-state) dependant-id))))
                          :on-change   #(do
                                          (swap! cells assoc-in [:state kw-id :input-state] (-> % .-target .-value)))}]
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


(defn create-row-label [left top x y width height row]
  ^{:key row} [:div {:style       {:left (+ x left) :top (+ y top) :width width :height height :background-color "lightgray" :opacity "1"
                                   :border "0.2px solid gray" :stroke-width 0.2 :position "absolute" :color "black" :display "flex" :justify-content "center" :align-items "center"}}
               row])

(defn create-column-label [left top x y width height column]
  ^{:key column} [:div {:style       {:left (+ x left) :top (+ y top) :width width :height height :background-color "lightgray" :opacity "1"
                                      :border "0.2px solid gray" :stroke-width 0.2 :position "absolute" :color "black" :display "flex" :justify-content "center" :align-items "center"}}
                  (number-to-letter column)])

(defn create-row-labels [left top full-height]
  (let [width 30
        height (/ full-height rows)]
    (doall (for [row (range rows)]
             (let [x 0
                   y (* row height)]
               (create-row-label left top x y width height row))))))

(defn create-column-labels [left top full-width]
  (let [width  (/ full-width columns)
        height 40]
    (doall (for [column (range columns)]
             (let [x (* column width)
                   y 0]
               (create-column-label left top x y width height column))))))

(defn init-state []
  (reset! cells
    (hash-map
      :dependencies {}
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
  (fn []
    [:<>
     (create-row-labels 80 180 500)
     (create-column-labels 110 140 800)
     (create-cells 110 180 800 500)]))

(defn page []
  [:div
   [:h2  "Cells"]
   [cells-component]])


(comment
  sum, div, sub, prod
  "=sum(A1:B1)")
