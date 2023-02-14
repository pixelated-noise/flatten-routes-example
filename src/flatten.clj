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
  (cond (keyword? sec) ; form 1
        {path-part sec}

        (string? path-part) ; form 2
        (update-keys (reduce merge (map flatten-routes-recursive rst))
                     (partial str path-part))

        :else ; form 3
        (reduce merge (map flatten-routes-recursive all))))


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
   (cond (empty? returns) routes                                                ; 1
         (keyword? fst)   (recur (assoc routes (str/join path) fst) path rst)   ; 2
         (string? fst)    (recur routes (conj path fst) rst)                    ; 3
         (fn? fst)        (recur routes (fst path) rst)                         ; 4
         :else            (let [pop-marker (when (string? (first fst)) [pop])]  ; 5
                            (recur routes path (concat fst pop-marker rst))))))

(defn flatten-routes-zipper [routes]
  (loop [res {}
         z   (z/vector-zip routes)]
    (cond (z/end? z)
          res

          (keyword? (z/node z))
          (recur (assoc res
                        (->> z z/path (map first) (filter string?) str/join)
                        (z/node z))
                 (z/next z))

          :else
          (recur res (z/next z)))))

(defn flatten-routes-iteration [routes]
  (->> (iteration identity
                  :initk (z/vector-zip routes)
                  :kf    z/next
                  :somef (complement z/end?)
                  :vf    #(when (-> % z/node keyword?)
                            [(->> % z/path (map first) (filter string?) str/join)
                             (z/node %)]))
       (into {})))

(defn zipper-iteration [zipper vf]
  (iteration identity
             :initk zipper
             :kf    z/next
             :somef (complement z/end?)
             :vf    vf))

(defn flatten-routes-generic [routes]
  (into {}
        (zipper-iteration
         (z/vector-zip routes)
         #(when (-> % z/node keyword?)
            [(->> % z/path (map first) (filter string?) str/join) (z/node %)]))))

(defn base-node? [[path-part sec]]
  (and (string? path-part)
       (keyword? sec)))

(defn flatten-routes-tree-seq [routes]
  (->> routes
       ;; walk the tree, collecting ancestor info (parent paths) in meta.
       (tree-seq
        ;; base nodes don't have children
        (complement base-node?)
        ;; prefix and sibling nodes do; extract the children
        (fn [[path-part & rst :as all]]
          (let [parent-path (get (meta all) ::parent-path "")]
            (if (string? path-part)
              ;; prefix node; extend the parent-path
              (map #(with-meta % {::parent-path (str parent-path path-part)})
                   rst)
              ;; else, sibling nodes; maintain the parent-path
              (map #(with-meta % {::parent-path parent-path})
                   all)))))
       ;; tree-seq returns every node in the tree; we want only the base nodes
       (filter base-node?)
       ;; a final pass to get the full path to the node
       (map (fn [[path-part keyword :as node]]
              [(str (::parent-path (meta node)) path-part) keyword]))
       (into {})))

(assert
 (= (flatten-routes-recursive the-routes)
    (flatten-routes-pre-1:11 the-routes)
    (flatten-routes-iter the-routes)
    (flatten-routes-zipper the-routes)
    (flatten-routes-iteration the-routes)
    (flatten-routes-generic the-routes)
    (flatten-routes-tree-seq the-routes)))
