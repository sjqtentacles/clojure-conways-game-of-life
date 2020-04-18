(ns clojure-conway-game-of-life.core
  (:gen-class)
  (:require [lanterna.screen :as s]
            [lanterna.terminal :as t]
            [clojure.math.combinatorics :as combo]))

(def onsymb "@")
(def offsymb " ")

(defmacro clear-draw-refresh
  [screen & drawings]
  (list 'do 
    (list s/clear screen)
    (cons 'do drawings)
    (list s/redraw screen)))

(defn concat-cells
  [arr]
  (clojure.string/join (flatten arr)))

(defn on?
  [c]
  (= c onsymb))

(defn off?
  [c]
  (= c offsymb))

(defn fetch-cell
  [arr x y]
  (-> arr
    (nth x)
    (nth y)))

(defn flip
  [c]
  (if (= c onsymb) offsymb onsymb))

(defn flip-cell
  "may run into out of bounds errors if row and coll are too big"
  [arr row col]
  (let [cell (fetch-cell arr row col)]
    (assoc arr row (assoc (nth arr row) col (flip cell)))))

(defn flip-cells
  "arr is the initial array, cellsv is a nested vector of cells
  to update. Order doesn't matter. Purely functional"
  [arr cellsv]
  (reduce
    (fn [a [x y]]
      (flip-cell a x y))
    arr
    cellsv))

(defn fetch-adjacent-cell-coords
  [arr x y]
  (let [top-left [(dec x) (dec y)]
        top-mid [(dec x) y]
        top-right [(dec x) (inc y)]
        mid-left [x (dec y)]
        mid-right [x (inc y)]
        bot-left [(inc x) (dec y)]
        bot-mid [(inc x) y]
        bot-right [(inc x) (inc y)]
        adjpoints [top-left top-mid top-right
        mid-left mid-right
        bot-left bot-mid bot-right]]
    (filter
      (fn [[i j]]
        (and 
          (>= i 0)
          (< i (count arr))
          (>= j 0)
          (< j (count (first arr)))))
      adjpoints)))

(defn fetch-adjacent-cells-count
  [arr x y]
  (let [points (fetch-adjacent-cell-coords arr x y)]
    (->> points
      (map (fn [[i j]] 
        (fetch-cell arr i j)))
      (map (fn [k] (if (= k onsymb) 1 0)))
      (apply +))))

(defn update-cell
  [arr x y]
  (let [adj-sum (fetch-adjacent-cells-count arr x y)
        cell-val (fetch-cell arr x y)]
    (cond
      (and (on? cell-val) (< adj-sum 2)) offsymb
      (and (on? cell-val) (> adj-sum 3)) offsymb
      (and (on? cell-val) (and (> adj-sum 1) (< adj-sum 4))) onsymb
      (and (off? cell-val) (= adj-sum 3)) onsymb
      :else cell-val)))

(defn full-update
  [arr]
  (let [w (count arr) h (count (first arr))]
    (partition 
      h
      (mapv 
        (fn [[x y]]
          (update-cell arr x y))
        (combo/cartesian-product 
          (range w) 
          (range h))))))

(defn print-array
  [arr]
  (doseq [line arr]
    (println line)))

(defn do-update-times
  [arr times]
  (let [scr (s/get-screen :swing)]
    (s/in-screen scr
      (loop [a arr t times prev-a []]
        (s/clear scr)
        (doseq [[line i] (map vector a (range (count a)))]
          (s/put-string scr 0 i (concat-cells line) {:fg :green}))
        (s/redraw scr)
        ;;(time (Thread/sleep 100))
        (if 
          (or (= t 0) (= a prev-a)) a 
          (recur (vec (map vec (full-update a))) (dec t) a))))))

(defn read-seed-from-file
  [fname]
  (as-> (slurp fname) s
    (clojure.string/replace s "." offsymb)
    (clojure.string/split-lines s)
    (mapv (fn [x] (clojure.string/split x #"")) s)))

(defn -main
  [& args]
  (let [arr (read-seed-from-file "glider.txt")]
    (do-update-times arr 600)))
