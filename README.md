# intension

[](dependency)
```clojure
[alandipert/intension "1.0.0"] ;; latest release
```
[](/dependency)

Clojure makes it easy and efficient to create, update, and access places within
immutable, nested associative structures like maps-of-maps or vectors-of-maps.

However, I haven't found in Clojure satisfying means of *querying* these
structures.  As a result, I have been compelled in the past to implement bespoke
query engines and DSLs on a per-application basis.

This library contains a set of functions for converting associative structures
to in-memory databases suitable for query via Datomic-flavored [Datalog][0]
implementations like [Datomic's own][1] or [DataScript][2].

## Usage

```clojure
(require '[alandipert.intension :refer [make-db]]
         '[datascript.core      :refer [q]])

(def pets
  [{:name "George"
    :species "Parakeet"
    :age 3
    :owners ["Frege" "Peirce"]}
   {:name "Francis"
    :species "Dog"
    :age 8
    :owners ["De Morgan"]}
   {:name "Bob"
    :species "Goldfish"
    :age 1
    :owners ["Peirce"]}])
    
;; Create a set of relations based on paths into the pets map. Every relation is
;; a path followed by the value at that path.

(def pets-db (make-db pets))

;; Find the names of all the pets:

(q '[:find ?name
     :where
     [_ :name ?name]]
   pets-db)
;;=> #{["George"] ["Francis"] ["Bob"]}

;; To find each owner and how many pets each owner owns, we might write Clojure code like this:

(->> (for [p pets, o (:owners p)] {o 1})
     (apply merge-with +))
;;=> {"Peirce" 2, "Frege" 1, "De Morgan" 1}

;; It's pretty short for this example, but I find this kind of code gets hard to
;; follow, particularly with deeper structures. I find queries involving joins
;; and filters especially difficult to express adequately in this way. Here's
;; how I would prefer to do it, with Datalog:

(->> (q '[:find ?owner (count ?pet)
          :where
          [?pet :owners _ ?owner]]
       pets-db)
     (into {}))
;;=> {"Peirce" 2, "Frege" 1, "De Morgan" 1}

;; Create another set of relations, this time prefixing each with the path
;; itself. This is useful for updating structures with update-in based on query
;; results.

(def pets-db2 (make-db pets {:output-style :update}))

;; Find the paths to every pet's age over 2:

(def age-paths
  (->> (q '[:find ?path
            :where
            [?path _ :age ?age]
            [(> ?age 2)]]
          pets-db2)
       (map first)))
;;=> ([0 :age] [1 :age])
     
(reduce #(update-in %1 %2 inc) pets age-paths)
;;=> A vector of maps in which George and Francis are now 4 and 9, respectively.
```

## Usage for diffing

It is hard to identify differences in nested objects. Working with the paths of an entirey flattened object makes it much easier.

```clojure
(diff {:foo 1} {:foo 9000 :bar 9000})

;;=> ({:path [:bar], :a nil, :b 9000} {:path [:foo], :a 1, :b 9000})
```

Where each result of diff is a map that specifies the `:path` to the value. The key `a` represents the value of this path in the first argument to diff and `b` is the value of the second argument at that path.

## Theory of operation

### Maps as sets of pairs

Consider the following map:

```clojure
(def m1
  {:color "red"
   :year 1992
   :sound "ringing"})
```

The same information could be represented as a set of vectors, or ordered pairs:

```clojure
(def m2
  #{[:color "red"]
    [:year 1992]
    [:sound "ringing"]})
```

A set of ordered pairs is equivalent to a map in that the `[key, value]`
associations are distinct within both structures. The map is superior for most
programming purposes in that given a key, the corresponding value can be found
efficiently.

However, the set-of-tuples structure has its own special affordance: it can be
viewed as set of 2-place [relations][3].

The advantage of a relational view of the structure is that it can be
queried directly easily with a popular dialect of Datalog.

For example, the following Datomic-flavored Datalog query finds the values for
`?v` in all of the relations starting with `:color`:

```clojure
[:find ?v :where [:color ?v]]
```

## Differences from specter

[specter][4] is another library that was also created with the goal making it
easier to work with nested structures in Clojure.

Unlike specter, intension supplies no means for updating structures — only
querying them using a (separate) Datalog implementation.  Also, it's only
possible to query maps and vectors currently.

## Notes and ideas for improvement

### Works great with amazonica

[amazonica][5] is a Clojure wrapper around many Amazon Web Services APIs that
returns results as vector/maps. These return values are amenable to intension,
making it possible to query AWS API results with Datalog.

### Shortcuts and conveniences

It might be useful to support "wildcards."  Sometimes you don't know how deep
the path is to some key/value in the structure you're interested in — you may just
know you're interested in the shallowest value for `:name`.

It might be good to create another function, like `make-meta-db`, that returned
a database of convenience relations generated at some extra expense. The meta db
might contain relations for every distinct key/value regardless of depth,
prefixed by path. Meta data could be joined against data generated by `make-db`
on the path value by using `q`'s ability to query multiple data sources.

### Other collections

Only maps and vectors are supported because:

* Compositions of them are maybe the most popular kind of nested structure in the wild
* We could easily generate paths into sequences, by indexing the same way we do
  with vectors, but `get` isn't defined in Clojure for sequences. That means the
  paths we generated couldn't be fed directly to Clojure's existing `get-in`.
  We'd need to supply a variant, which would add scope. We could add access
  protocols, but that would also add scope. Better to use this for awhile longer
  and see what we actually want and need.
* We can generate paths into sets but they're never in JSON - worth the extra stuff?
  
### Input validation

We know some from a query's clauses the expectations the user has about the
structure and size of the tuples in the set. Provided a set of clauses, we could
do some amount of validation on input data. The more places in the clauses are
qualified, the more validation we could do.

[0]: https://en.wikipedia.org/wiki/Datalog
[1]: http://docs.datomic.com/query.html
[2]: https://github.com/tonsky/datascript
[3]: https://en.wikipedia.org/wiki/Relation_(database)
[4]: https://github.com/nathanmarz/specter
[5]: https://github.com/mcohen01/amazonica

## License

```
Copyright (c) Alan Dipert. All rights reserved.

The use and distribution terms for this software are covered by the Eclipse
Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
be found in the file epl-v10.html at the root of this distribution. By using
this software in any fashion, you are agreeing to be bound by the terms of
this license. You must not remove this notice, or any other, from this software.
```
