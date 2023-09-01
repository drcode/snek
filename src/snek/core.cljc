(ns snek.core
  (:refer-clojure :rename
                  {defn      core-defn
                   defmethod core-defmethod
                   update    core-update})
  (:require [clojure.set :as st]
            [clojure.test :as te]
            [clojure.edn :as ed]
            [net.cgrand.macrovich :as mc]
            #?(:clj [clojure.pprint :as pp]
               :cljs [cljs.pprint :as pp])))

(def lazy-depth 10)

(core-defn typ [k]
           (cond (map? k)  :map
                 (set? k)  :set
                 (coll? k) :coll
                 (fn? k)   :fn
                 :else     :other))

(defmulti mismatch
  (fn [x y]
    (mapv typ [x y])))

(def magic-values #{0 :_ "" #{} false})

(core-defn nil-if-empty [coll]
           (when (seq coll)
             coll))

(core-defn optional-key [key]
           (when (keyword? key)
             (when-let [[_ s] (re-matches #"\?(.+)$" (name key))]
               (keyword s))))

(core-defmethod mismatch [:map :map]
                [ref data]
                (nil-if-empty (into {}
                                    (keep (fn [[k v]]
                                            (cond (optional-key k)                                (let [k     (optional-key k)]
                                                                                                    (when (contains? data k)
                                                                                                      (let [inner (mismatch v (data k))]
                                                                                                        (when-not (and (contains? data k) (not inner))
                                                                                                          {k inner}))))

                                                  (and (= (typ k) :other) (not (magic-values k))) (let [inner (mismatch v (data k))]
                                                                                                    (when-not (and (contains? data k) (not inner))
                                                                                                      {k inner}))
                                                  :else                                           (when-let [g (nil-if-empty (into {}
                                                                                                                                   (keep (fn [[k2 v2]]
                                                                                                                                           (when (not (mismatch k k2))
                                                                                                                                             (let [inner (mismatch v v2)]
                                                                                                                                               (when inner
                                                                                                                                                 {k2 inner}))))
                                                                                                                                         data)))]
                                                                                                    {k g})))
                                          ref))))

(core-defmethod mismatch [:set :set]
  [ref data]
  (nil-if-empty (set (keep (fn [k]
                             (when (and (= (typ k) :other) (not (magic-values k)) (not (contains? data k)))
                               k))
                           ref))))

(core-defmethod mismatch [:set :other]
  [ref data]
  ref)

(core-defmethod mismatch [:other :map]
  [ref data]
  ref)

(core-defmethod mismatch [:set :map]
  [ref data]
  ref)

(core-defmethod mismatch [:map :coll]
  [ref data]
  ref)

(core-defmethod mismatch [:other :other]
  [ref data]
  (cond (nil? ref)    nil
        (= ref 0)     (when-not (number? data)
                        ref)
        (= ref "")    (when-not (string? data)
                        ref)
        (= ref :_)    (when-not (keyword? data)
                        ref)
        (= ref '_)    (when-not (symbol? data)
                        ref)
        (= ref false) (when-not (boolean? data)
                        ref)
        :other      (when-not (= ref data)
                        ref)))

(core-defmethod mismatch [:coll :coll]
  [ref data]
  (cond (= (count ref) 1)            (nil-if-empty (keep (partial mismatch (first ref)) (take lazy-depth data))) ;;cap at 100 because of infinite lists
        (= (count ref) (count data)) (nil-if-empty (keep (fn [[ref data]]
                                                           (mismatch ref data))
                                                         (map vector ref data)))
        :else                        (for [item ref]
                                       (cond (map? item) {}
                                             (coll? item) []
                                             :else        item))))

(core-defmethod mismatch [:coll :other]
                [ref data]
                ref)

(core-defmethod mismatch [:other :fn]
                [ref data]
                ref)

(core-defmethod mismatch [:other :coll]
                [ref data]
                ref)

(core-defmethod mismatch [:coll :set]
                [ref data]
                ref)

(core-defmethod mismatch [:map :other]
                [ref data]
                ref)

(core-defmethod mismatch [:coll :map]
  [ref data]
  ref)

(core-defn valid? [ref data]
           (not (mismatch ref data)))

(defmulti generalize
  (fn [x y]
    (mapv typ [x y])))

(core-defmethod generalize [:other :other]
[ref data]
(cond (nil? ref)    nil
      (= ref 0)     (when (number? data)
                      0)
      (= ref "")    (when (string? data)
                      "")
      (= ref :_)    (when (keyword? data)
                      :_)
      (= ref '_)    (when (symbol? data)
                      '_)
      (= ref false) (when (boolean? data)
                      :_)
      :other      (cond (= ref data)                         ref
                          (and (number? ref) (number? data))   0
                          (and (string? ref) (string? data))   ""
                          (and (keyword? ref) (keyword? data)) :_
                          (and (boolean? ref) (boolean? ref))  false
                          :else                                nil)))

(core-defmethod generalize [:coll :coll]
  [ref data]
  (let [ref  (take lazy-depth ref)
        data (take lazy-depth data)]
    (cond (= (count ref) 1)                                             [(reduce generalize (first ref) data)]
          (and (= (count ref) (count data)) (< (count ref) lazy-depth)) (mapv generalize ref data)
          :else                                                         (generalize [(reduce generalize ref)] data))))

(core-defn nil-snek-type? [o]
           (or (nil? o) (not (or (number? o) (string? o) (keyword? o) (coll? o)))))

(core-defn coll-snek-type? [o]
           (and (not (map? o)) (coll? o)))

(core-defmethod generalize [:set :set]
  [ref data]
  (let [ref-keys     ref
        data-keys    data
        all-keys     (st/union ref-keys data-keys)
        safe-keys    (apply disj (st/intersection ref-keys data-keys) magic-values)
        unsafe-keys  (apply disj (st/difference (st/union ref-keys data-keys) safe-keys) magic-values)
        wild-nil     (or (contains? all-keys nil) (some nil-snek-type? unsafe-keys))
        wild-number  (or (contains? all-keys 0) (some number? unsafe-keys))
        wild-string  (or (contains? all-keys "") (some string? unsafe-keys))
        wild-keyword (or (contains? all-keys :_) (some keyword? unsafe-keys))
        wild-boolean (or (contains? all-keys false) (some boolean? unsafe-keys))
        wild-coll    (or (some coll-snek-type? unsafe-keys))
        wild         (fn [pred]
                       (reduce generalize
                               (keep (fn [[k v]]
                                       (when (pred k)
                                         v))
                                     (concat ref data))))]
    (cond-> safe-keys
      wild-nil     (conj nil)
      wild-number  (conj 0)
      wild-string  (conj "")
      wild-keyword (conj :_)
      wild-boolean (conj false)
      wild-coll    (conj (reduce generalize (filter coll-snek-type? all-keys))))))

(core-defmethod generalize [:map :map]
  [ref data]
  (let [ref-keys     (set (keys ref))
        data-keys    (set (keys data))
        all-keys     (st/union ref-keys data-keys)
        safe-keys    (apply disj (st/intersection ref-keys data-keys) magic-values)
        unsafe-keys  (apply disj (st/difference (st/union ref-keys data-keys) safe-keys) magic-values)
        wild-nil     (or (contains? all-keys nil) (some nil-snek-type? unsafe-keys))
        wild-number  (or (contains? all-keys 0) (some number? unsafe-keys))
        wild-string  (or (contains? all-keys "") (some string? unsafe-keys))
        wild-keyword (or (contains? all-keys :_) (some keyword? unsafe-keys))
        wild-boolean (or (contains? all-keys false) (some boolean? unsafe-keys))
        wild-coll    (or (some coll-snek-type? unsafe-keys))
        wild         (fn [pred]
                       (reduce generalize
                               (keep (fn [[k v]]
                                       (when (pred k)
                                         v))
                                     (concat ref data))))]
    (cond-> (into {}
                  (for [key safe-keys]
                    [key (generalize (ref key) (data key))]))
      wild-nil     (assoc nil (wild nil-snek-type?))
      wild-number  (assoc 0 (wild number?))
      wild-string  (assoc "" (wild string?))
      wild-keyword (assoc :_ (wild keyword?))
      wild-boolean (assoc false (wild boolean?))
      wild-coll    (assoc (reduce generalize (filter coll-snek-type? all-keys)) (wild coll-snek-type?)))))

(core-defmethod generalize [:coll :other]
  [ref data]
  nil)

(core-defmethod generalize [:other :coll]
  [ref data]
  nil)

(core-defmethod generalize [:other :map]
                [ref data]
                nil)

(defmulti query
  (fn [x y]
    (mapv typ [x y])))

(core-defmethod query [:other :map]
                [ref data]
                (if ref
                  (data ref)
                  data))

(core-defmethod query [:other :coll]
                [ref data]
                (if ref
                  (data ref)
                  data))

(core-defmethod query [:other :set]
                [ref data]
                (if ref
                  (data ref)
                  data))

(core-defmethod query [:coll :map]
                [ref data]
                (vec (for [item ref]
                       (query item data))))

(core-defmethod query [:map :map]
                [ref data]
                (doseq [[k v] ref]
                  (when (and (not (or (nil? k) (magic-values k))) (not (contains? data k)))
                    (throw (ex-info (str "Missing query key " (pr-str k) " in data") {}))))
                (let [result (into {}
                                   (keep (fn [[k v]]
                                           (reduce (fn [acc [kref vref :as item]]
                                                     (if (valid? kref k)
                                                       (let [[k v] (or acc [k v])]
                                                         [k (query vref v)])
                                                       acc))
                                                   nil
                                                   ref))
                                         data))]
                  (if (and (= (count ref) 1) (not (or (nil? (first (keys ref))) (magic-values (first (keys ref)))))) ;can lift the query result higher
                    (result (first (keys ref)))
                    result)))

(core-defmethod query [:set :set]
                [ref data]
                (set (filter (fn [item]
                               (some #(valid? % item) ref))
                             data)))

(core-defmethod query [:other :other]
                [ref data]
                (when (valid? ref data)
                  data))

(core-defmethod query [:coll :coll]
                [ref data]
                (vec (if (= (count ref) 1)
                       (for [item data]
                         (query (first ref) item))
                       (map query ref data))))

(core-defmethod query [:map :other]
                [ref data]
                nil)

(defmulti modify
  (fn [x y]
    (mapv typ [x y])))

(core-defmethod modify [:map :map]
                [upd coll]
                (reduce (fn [acc [k v :as item]]
                          (cond (or (nil? k) (magic-values k)) (into {}
                                                                     (for [[k2 v2 :as item2] acc]
                                                                       (if (valid? k k2)
                                                                         [k2 (modify v v2)]
                                                                         item2)))
                                (or (map? k) (fn? k))          (into {}
                                                                     (for [[k2 v2 :as item2] acc]
                                                                       [(modify k k2)
                                                                        (if v
                                                                          (modify v v2)
                                                                          v2)]))
                                :else                          (core-update acc k (partial modify v))))
                        coll
                        upd))


(core-defmethod modify [:coll :map]
                [upd coll]
                (cond (= (count upd) 1) (into {}
                                              (map (partial modify (first upd)) coll))
                      (= (count upd) 2) (into {}
                                              (for [[k v] coll]
                                                [(modify (first upd) k) (modify (second upd) v)]))))

(core-defmethod modify [:map :coll]
                [upd coll]
                (reduce (fn [acc [k v :as item]]
                          (core-update acc
                                       k
                                       (fn [g]
                                         (modify v g))))
                        coll
                        upd))

(core-defmethod modify [:fn :other]
                [upd coll]
                (upd coll))

(core-defmethod modify [:coll :coll]
                [upd coll]
                (cond-> (cond (= (count upd) 1)            (map (partial modify (first upd)) coll)
                              (= (count coll) (count upd)) (map modify upd coll)
                              :else                        upd)
                  (vector? coll) vec))

(core-defmethod modify [:coll :set]
                [upd coll]
                (set (cond (= (count upd) 1)            (map (partial modify (first upd)) coll)
                           (= (count coll) (count upd)) (map modify upd coll)
                           :else                        upd)))

(core-defmethod modify [:other :other]
                [upd coll]
                (if (nil? upd)
                  coll
                  upd))

(core-defmethod modify [:fn :coll]
                [upd coll]
                (upd coll))

(core-defmethod modify [:fn :map]
                [upd coll]
                (upd coll))

(core-defmethod modify [:fn :set]
                [upd coll]
                (upd coll))

(core-defmethod modify [:coll :other]
                [upd coll]
                upd)

(core-defmethod modify [:map :other]
                [upd coll]
                (if coll
                  (throw (ex-info (str "Can't update map " upd " to atomic value " coll) {}))
                  (modify {} upd)))

(core-defmethod modify [:other :map]
                [upd coll]
                (cond (keyword? upd) (coll upd)
                      (nil? upd)     coll
                      :else          (throw (ex-info (str "Can't update arbitrary value " upd " against map " coll) {}))))

(defmulti instance typ)

(core-defmethod instance :map
                [ref]
                (into {}
                      (keep (fn [[k v]]
                              (when (and (not (magic-values k)) (not (optional-key k)) (not (coll? k)))
                                [k (instance v)]))
                            ref)))

(core-defmethod instance :coll
                [ref]
                (if (= (count ref) 1)
                  []
                  (mapv instance ref)))

(core-defmethod instance :set
                [ref]
                #{})

(core-defmethod instance :other
                [ref]
                ref)

(core-defn parse [template s]
           (let [k (try (ed/read-string (str "[" s "]"))
                        (catch #?(:clj Exception :cljs js/Object) e
                          nil))]
             (when (valid? template k)
               k)))

(def snek-type (atom nil))
(def snek-debug (atom false))

(def snek-inferences (atom {}))
(def snek-declarations (atom {}))

(defmacro defsnek [& body]
  (if (seq body) 
    (let [body     (vec body)
          num      (count body)
          args     (subvec body 0 (- num 2))
          result   (last body)
          arrow    (body (- num 2))
          args-sym (gensym)]
      (assert (or (= arrow '->) (= arrow '-d>)))
      `(let [~args-sym [~@args]]
         (do (assert (not @snek-type))
             (reset! snek-debug ~(= arrow '-d>))
             (reset! snek-type [~args-sym ~(if (= result '*)
                                             `(first ~args-sym)
                                             result)]))))
    `(do (reset! snek-type nil)
         (reset! snek-inferences {}))))

(core-defn add-inference [nam args result]
           (swap! snek-inferences
                  (fn [snek-inferences]
                    (assoc snek-inferences
                           nam
                           (if (contains? snek-inferences nam)
                             (let [[args-exp result-exp] (snek-inferences nam)]
                               [(generalize args-exp args) (generalize result-exp result)])
                             [(generalize args args) (generalize result result)]))))) ;;generalizing items against themselves will "disinfect" the map key case where the data is something like {0 :a 1 :b} which can't self-validate.

(core-defn check-inferences [snek-inferences snek-declarations]
           (filter (fn [[k v]]
                     (not= v (snek-declarations k)))
                   snek-inferences))

(core-defn raw-defsnek [[args result :as inf]]
           (concat ['defsnek]
                   args
                   ['-> result]))

(core-defn inference-report []
           (doseq [[k v] @snek-inferences]
             (when (not= v (@snek-declarations k))
               (println k ":" (raw-defsnek v)))))

#?(:clj (core-defmethod te/assert-expr 'inferences-valid? [msg form]
                        `(let [failure# (when-let [[k#] (check-inferences @snek-inferences @snek-declarations)]
                                          k#)
                               result# (not failure#)]
                           (te/do-report 
                            {:type (if result# :pass :fail)
                             :message (str "Inferred snek for " (first failure#) " should match declaration")
                             :expected (raw-defsnek (second failure#))
                             :actual (raw-defsnek (@snek-declarations (first failure#)))})
                           result#)))

(core-defn restructure
           "Takes an argument list and returns a string equivalent to the vector of all items in the argument list"
           [args gensym]
           (let [n    (count args)
                 args (vec (for [arg args]
                             (cond (map? arg)    (cond-> arg
                                                   (not (:as arg)) (assoc :as (gensym)))
                                   (vector? arg) (let [z (count arg)]
                                                   (if (and (>= z 2) (= (arg (- z 2)) :as))
                                                     arg
                                                     (conj arg :as (gensym))))
                                   :else         arg)))]
             (if (and (>= n 2) (= (args (- n 2)) '&))
               (let [[a b] (restructure (subvec args 0 (- n 2)) gensym)]
                 `[~args
                   (concat ~b ~(last args))])
               `[~args
                 (list ~@(for [arg args]
                           (cond (map? arg)    (:as arg)
                                 (vector? arg) (last arg)
                                 :else         arg)))])))

(core-defn defn-helper [raw-fun-sym declaration nam nam-human args body]
           (let [[args arg-list] (restructure args gensym)
                 raw-fun         (symbol raw-fun-sym)]
             `(if (and @snek-type ~*assert*) ;;requires compiler-option "elide-asserts" to work in clojurescript
                (let [[args-exp# result-exp#] @snek-type
                      pp-fun#                 (mc/case :clj  clojure.pprint/pprint
                                                       :cljs cljs.pprint/pprint)
                      err-fn#                 (fn [typ# a# b# delta#]
                                                (throw (ex-info (str "Snek " typ# " error in " ~nam-human ".\n\nExpected:\n" (with-out-str (pp-fun# a#)) "\nActual:\n" (with-out-str (pp-fun# b#)) "\nDelta:\n" (with-out-str (pp-fun# delta#))) {})))]
                  (swap! snek-declarations assoc '~nam [args-exp# result-exp#])
                  (declare ~nam)
                  (core-defn ~raw-fun ~args
                             ~@body)
                  (~@declaration
                   ~args
                   (let [args# ~arg-list]
                     (do (when ~(identity @snek-debug)
                           (println (pr-str (cons '~nam (butlast (query (conj args-exp# ::xtra) (conj (vec args#) ::xtra)))))))
                         (when-let [delta# (mismatch (conj args-exp# ::xtra) (conj (vec args#) ::xtra))]
                           (err-fn# "argument" args-exp# args# delta#))
                         (let [result# (apply ~raw-fun args#)]
                           (when-let [delta# (mismatch result-exp# result#)]
                             (err-fn# "result" result-exp# result# delta#))
                           #_(add-inference '~nam (vec args#) result#)
                           (when ~(identity @snek-debug)
                             (println '~nam-human "results =" (pr-str (query result-exp# result#))))
                           result#))))
                  (reset! snek-type nil))
                (~@declaration ~args ~@body))))

(defmacro defn [nam args & body] ;; todo: support docstrings
  (defn-helper (str (name nam) "__") ['clojure.core/defn nam] nam (name nam) args body))

(defmacro defmethod [nam pattern args & body] ;; todo: support docstrings
  (defn-helper (str (name nam) "__" (name (gensym))) ['clojure.core/defmethod nam pattern] nam (str (name nam) " " pattern) args body))

(core-defn snek [b]
           (defsnek))
;; @snek-inferences
