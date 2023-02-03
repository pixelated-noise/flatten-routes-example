(ns flatten
  (:require [clojure.zip :as z]
            [clojure.string :as str]))

(def the-routes
  [["/yo" [["/man" :man] ["/manman" :manman]]]
   ["/foo" [] [[]]
    ["/bar" :bar]
    ["/baz" ["/quux" :quux] [["/qux0" :qux0] [["/qux1" :qux1]]]]
    ["/ba" ["/zz" ["/bazz" :bazz]] ["/baq" :baq]]]])

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
   (cond (empty? returns) routes
         (keyword? fst)   (recur (assoc routes (str/join path) fst) path rst)
         (string? fst)    (recur routes (conj path fst) rst)
         (fn? fst)        (recur routes (fst path) rst)
         :else            (let [pop-marker (when (string? (first fst)) [pop])]
                            (recur routes path (concat fst pop-marker rst))))))

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

(defn zipper-iteration [zipper vf]
  (iteration identity
             :initk zipper
             :kf    z/next
             :somef (complement z/end?)
             :vf    vf))

(defn flatten-routes-generic [routes]
  (into {}
        (zipper-iteration (z/vector-zip routes)
                          #(when (-> % z/node keyword?)
                             [(->> % z/path (map first) (filter string?) str/join) (z/node %)]))))

(assert
 (= (flatten-routes-recursive the-routes)
    (flatten-routes-pre-1:11 the-routes)
    (flatten-routes-iter the-routes)
    (flatten-routes-zipper the-routes)
    (flatten-routes-iteration the-routes)
    (flatten-routes-generic the-routes)))
