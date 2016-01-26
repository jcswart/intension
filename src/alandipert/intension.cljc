(ns alandipert.intension
  "Functions for converting nested structures into sets of tuples.")

(defn paths
  "Returns the set of paths into a nested map/vector."
  ([root]
   {:pre [(or (map? root)
              (vector? root))]}
   (paths [] root))
  ([parent x]
   (cond (map? x)
         (mapcat (fn [[k v]] (paths (conj parent k) v)) x)
         (vector? x)
         (mapcat #(paths (conj parent %1) %2) (range) x)
         :else [parent])))

(defn make-db
  "Converts a nested structure of vectors/maps into a set of tuples suitable for
   query by Datalog.  Takes an optional configuration map that can contain these options:

     format
         :diff   - Return [[<path>] <val>], ...]          Useful for diffing structures.
         :update - Return [[<path>] <path> <val>], ...]   Useful for processing structures with
                                                         update-in based on query results."
  [coll & [{:keys [format]
            :or   {format nil}}]]
  (mapv (fn [path]
          (let [update      (vec (list* path path))
                val-of-path (get-in coll path)
                diff        (vector path)]
            (conj
             (case format
               :update  update
               :diff    diff
               path)
             val-of-path)))
        (paths coll)))

(declare deep-diff)
(declare make-diff-db)

(defn diff
  "A completely flat structure makes identifying change easy."
  [a b]
  (let [a' (make-diff-db a)
        b' (make-diff-db b)]
    (if (= a' b')
      :same
      (deep-diff a' b'))))

(defn make-diff-db
  "Diffs will benefit from fast lookups."
  [root]
  (->> (make-db root {:format :diff})
       (apply concat)
       (apply hash-map)))

(defn deep-diff
  "We don't have identical items so we find all variance.

  eg: a' = {:foo 1}, b' = {:foo 2 :bar 2} --> [{:path [:bar] :a nil :b 2}
                                               {:path [:foo] :a 1   :b 2}]"
  [a' b']
  (let [paths (set (concat (keys a') (keys b')))
        compare-val (fn [accu path]
                      (let [val-a' (get a' path)
                            val-b' (get b' path)]
                        (if (= val-a' val-b')
                          accu
                          (conj accu {:path path :a val-a' :b val-b'}))))
        differences (reduce compare-val (vector) paths)]
    (sort-by :path differences)))
