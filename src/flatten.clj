(ns flatten
  (:require [clojure.zip :as z]
            [clojure.string :as str]))

(def the-routes
  [["/yo" [["/man" :man] ["/manman" :manman]]]
   ["/foo" [] [[]]
    ["/bar" :bar]
    ["/baz" ["/quux" :quux] [["/qux0" :qux0] [["/qux1" :qux1]]]]
    ["/ba" ["/zz" ["/bazz" :bazz]] ["/baq" :baq]]]])

(def the-routes
  ["/yo"
   ["/hey" :hh]])

(defn flatten-routes-recursive [[path-part & [sec :as rst] :as all]]
  (cond (keyword? sec)      {path-part sec}
        (string? path-part) (update-keys (reduce merge (map flatten-routes-recursive rst))
                                         (partial str path-part))
        :else               (reduce merge (map flatten-routes-recursive all))))


(defn flatten-routes-pre-1:11 [[path-part & [sec :as rst] :as all]]
  (cond (keyword? sec)      {path-part sec}
        (string? path-part) (->> (reduce merge (map flatten-routes-pre-1:11 rst))
                                 (reduce-kv (fn [m k v]
                                              (assoc m (str path-part k) v))
                                            {}))
        :else               (reduce merge (map flatten-routes-pre-1:11 all))))

(defn flatten-routes-iter
  ([x] (flatten-routes-iter {} [] x))
  ([routes path [fst & rst :as returns]]
   (prn (symbol "routes ") routes)
   (prn (symbol "path   ") path)
   (prn (symbol "returns") returns)
   (prn (cond (empty? returns) (symbol "case 1")
              (keyword? fst)   (symbol "case 2")
              (string? fst)    (symbol "case 3")
              (fn? fst)        (symbol "case 4")
              :else            (symbol "case 5")))
   (prn '------------------------------------------)
   (cond (empty? returns) routes
         (keyword? fst)   (recur (assoc routes (apply str path) fst) path rst)
         (string? fst)    (recur routes (conj path fst) rst)
         (fn? fst)        (recur routes (fst path) rst)
         :else            (let [pop-marker (when (string? (first fst)) [pop])]
                            (recur routes path (concat fst pop-marker rst))))))
(defn flatten-routes-iter
  ([x] (flatten-routes-iter {} [] x))
  ([routes path [fst & rst :as returns]]
   (cond (empty? returns) routes
         (keyword? fst)   (recur (assoc routes (str/join path) fst) path rst)
         (string? fst)    (recur routes (conj path fst) rst)
         (fn? fst)        (recur routes (fst path) rst)
         :else            (let [pop-marker (when (string? (first fst)) [pop])]
                            (recur routes path (concat fst pop-marker rst))))))

(defn flatten-routes-iter-compact
  ([x] (flatten-routes-iter-compact {} [] x))
  ([routes path [h & t :as returns]]
   (cond (keyword? h) (recur (assoc routes (apply str path) h) path t)
         (string? h)  (recur routes (conj path h) t)
         (empty? h)   (if (empty? t) routes (recur routes (pop path) t))
         :else        (let [pop-marker (when (string? (first h)) [[]])]
                        (recur routes path (concat h pop-marker t))))))

(defn flatten-routes-zipper [routes]
  (loop [res {} z (z/vector-zip routes)]
    (cond (z/end? z)            res
          (keyword? (z/node z)) (recur (assoc res
                                              (->> z z/path (map first) (filter string?) str/join)
                                              (z/node z))
                                       (z/next z))
          :else                 (recur res (z/next z)))))

(defn flatten-routes-iteration [routes]
  (->> (iteration identity
                  :initk (z/vector-zip routes)
                  :kf    z/next
                  :somef (complement z/end?)
                  :vf    #(when (-> % z/node keyword?)
                            [(->> % z/path (map first) (filter string?) str/join) (z/node %)]))
       (into {})))

(assert
 (= (flatten-routes-recursive the-routes)
    (flatten-routes-pre-1:11 the-routes)
    (flatten-routes-iter the-routes)
    (flatten-routes-zipper the-routes)
    (flatten-routes-iteration the-routes)))
