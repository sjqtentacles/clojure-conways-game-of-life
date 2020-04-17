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
          (<= i (dec (count (first arr))))
          (>= j 0)
          (<= j (dec (count arr)))))
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
      (and (on? cell-val) (and (> adj-sum 1) (< adj-sum 4))) cell-val
      (and (off? cell-val) (= adj-sum 3)) onsymb
      :else cell-val)))

(defn full-update
  [arr]
  (let [w (count (first arr))
        h (count arr)]
    (partition 
      w
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
      (loop [a arr t times]
        (s/clear scr)
        (doseq [[line i] (map vector a (range (count a)))]
          (s/put-string scr 0 i (concat-cells line) {:fg :green}))
        (s/redraw scr)
        (time (Thread/sleep 300))
        (if 
          (= t 0) a 
          (recur (vec (map vec (full-update a))) (dec t)))))
        ))

(defn -main
  [& args]
  (def arr
    [
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;1
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;2
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,onsymb,onsymb,onsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;3
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;4
      [offsymb,onsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;5
      [offsymb,offsymb,onsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;6
      [onsymb,onsymb,onsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;7
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;8
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;9
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,onsymb,onsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;10
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,onsymb,onsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;11
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;12
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;13
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;14
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;15
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;16
      [offsymb,onsymb,onsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;17
      [offsymb,onsymb,onsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;18
      [offsymb,offsymb,offsymb,onsymb,onsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;19
      [offsymb,offsymb,offsymb,onsymb,onsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;20
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;21
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;22
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;23
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;24
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;25
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;26
      [offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb,offsymb];;27
    ])
  (do-update-times arr 100))
