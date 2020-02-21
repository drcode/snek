(ns snek.core
  (:refer-clojure
   :rename {defn   core-defn
            update core-update})
  (:require [clojure.set :as st]
            [clojure.test :as te]))

(def lazy-depth 10)

(core-defn typ [k]
           (cond (map? k)  :map
                 (set? k)  :set
                 (coll? k) :coll
                 (fn? k)   :fn
                 :else     :default))

(defmulti mismatch
  (fn [x y]
    (mapv typ [x y])))

(def magic-values #{0 :_ "" #{} false})

(core-defn nil-if-empty [coll]
           (when (seq coll)
             coll))

(defmethod mismatch [:map :map]
  [ref data]
  (nil-if-empty (into {}
                      (keep (fn [[k v]]
                              (if (and (= (typ k) :default) (not (magic-values k)))
                                (let [inner (mismatch v (data k))]
                                  (when-not (and (contains? data k) (not inner))
                                    {k inner}))
                                (when-let [g (nil-if-empty (into {}
                                                                 (keep (fn [[k2 v2]]
                                                                         (when (not (mismatch k k2))
                                                                           (let [inner (mismatch v v2)]
                                                                             (when inner
                                                                               {k2 inner}))))
                                                                       data)))]
                                  {k g})))
                            ref))))

(defmethod mismatch [:set :set]
  [ref data]
  (nil-if-empty (set (keep (fn [k]
                             (when (and (= (typ k) :default) (not (magic-values k)) (not (contains? data k)))
                               k))
                           ref))))

(defmethod mismatch [:set :default]
  [ref data]
  ref)

(defmethod mismatch [:default :map]
  [ref data]
  ref)

(defmethod mismatch [:set :map]
  [ref data]
  ref)

(defmethod mismatch [:map :coll]
  [ref data]
  ref)

(defmethod mismatch [:default :default]
  [ref data]
  (cond (nil? ref)    nil
        (= ref 0)     (when-not (number? data)
                        ref)
        (= ref "")    (when-not (string? data)
                        ref)
        (= ref :_)    (when-not (keyword? data)
                        ref)
        (= ref false) (when-not (boolean? data)
                        ref)
        :default      (when-not (= ref data)
                        ref)))

(defmethod mismatch [:coll :coll]
  [ref data]
  (cond (= (count ref) 1)            (nil-if-empty (keep (partial mismatch (first ref)) (take lazy-depth data))) ;;cap at 100 because of infinite lists
        (= (count ref) (count data)) (nil-if-empty (keep (fn [[ref data]]
                                                           (mismatch ref data))
                                                         (map vector ref data)))
        :else                        (for [item ref]
                                       (cond (map? item) {}
                                             (coll? item) []
                                             :else        item))))

(defmethod mismatch [:coll :default]
  [ref data]
  ref)

(defmethod mismatch [:default :coll]
  [ref data]
  ref)

(defmethod mismatch [:map :default]
  [ref data]
  ref)

(defmethod mismatch [:coll :map]
  [ref data]
  ref)

(core-defn valid? [ref data]
           (not (mismatch ref data)))

(defmulti generalize
  (fn [x y]
    (mapv typ [x y])))

(defmethod generalize [:default :default]
[ref data]
(cond (nil? ref)    nil
      (= ref 0)     (when (number? data)
                      0)
      (= ref "")    (when (string? data)
                      "")
      (= ref :_)    (when (keyword? data)
                      :_)
      (= ref false) (when (boolean? data)
                      :_)
      :default      (cond (= ref data)                         ref
                          (and (number? ref) (number? data))   0
                          (and (string? ref) (string? data))   ""
                          (and (keyword? ref) (keyword? data)) :_
                          (and (boolean? ref) (boolean? ref))  false
                          :else                                nil)))

(defmethod generalize [:coll :coll]
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

(defmethod generalize [:set :set]
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

(defmethod generalize [:map :map]
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

(defmethod generalize [:coll :default]
  [ref data]
  nil)

(defmethod generalize [:default :coll]
  [ref data]
  nil)

(defmethod generalize [:default :map]
  [ref data]
  nil)

(defmulti query
  (fn [x y]
    (mapv typ [x y])))

(defmethod query [:default :map]
  [ref data]
  (if ref
    (data ref)
    data))

(defmethod query [:default :coll]
  [ref data]
  (if ref
    (data ref)
    data))

(defmethod query [:default :set]
  [ref data]
  (if ref
    (data ref)
    data))

(defmethod query [:coll :map]
  [ref data]
  (vec (for [item ref]
         (query item data))))

(defmethod query [:map :map]
  [ref data]
  (into {}
        (keep (fn [[k v]]
                (reduce (fn [acc [kref vref :as item]]
                          (if (valid? kref k)
                            (let [[k v] (or acc [k v])]
                              [k (query vref v)])
                            acc))
                        nil
                        ref))
              data)))

(defmethod query [:set :set]
  [ref data]
  (set (filter (fn [item]
                 (some #(valid? % item) ref))
               data)))

(defmethod query [:default :default]
  [ref data]
  (when (valid? ref data)
    data))

(defmethod query [:coll :coll]
  [ref data]
  (vec (if (= (count ref) 1)
         (for [item data]
           (query (first ref) item))
         (map query ref data))))

(defmulti update
  (fn [x y]
    (mapv typ [x y])))

(defmethod update [:map :map]
  [upd coll]
  (reduce (fn [acc [k v :as item]]
            (if (or (nil? k) (magic-values k))
              (into {}
                    (for [[k2 v2 :as item2] acc]
                      (if (valid? k k2)
                        [k2 (update v v2)]
                        item2)))
              (core-update acc k (partial update v))))
          coll
          upd))

(defmethod update [:coll :map]
  [upd coll]
  (cond (= (count upd) 1) (into {}
                                (map (partial update (first upd)) coll))
        (= (count upd) 2) (into {}
                                (for [[k v] coll]
                                  [(update (first upd) k) (update (second upd) v)]))))

(defmethod update [:map :coll]
  [upd coll]
  (reduce (fn [acc [k v :as item]]
            (core-update acc
                         k
                         (fn [g]
                           (update v g))))
          coll
          upd))

(defmethod update [:fn :default]
  [upd coll]
  (upd coll))

(defmethod update [:coll :coll]
  [upd coll]
  (cond-> (cond (= (count upd) 1)            (map (partial update (first upd)) coll)
                (= (count coll) (count upd)) (map update upd coll)
                :else                        upd)
    (vector? coll) vec))

(defmethod update [:default :default]
  [upd coll]
  upd)

(defmethod update [:fn :coll]
  [upd coll]
  (upd coll))

(defmethod update [:fn :map]
  [upd coll]
  (upd coll))

(defmethod update [:coll :default]
  [upd coll]
  upd)

(defmethod update [:map :default]
  [upd coll]
  (if coll
    (throw (ex-info (str "Can't update map " upd " to atomic value " coll) {}))
    (update {} upd)))

(defmethod update [:default :map]
  [upd coll]
  (if (keyword? upd)
    (coll upd)
    (throw (ex-info (str "Can't update arbitrary value " upd " against map " coll) {}))))

(def snek-type (atom nil))
(def snek-debug (atom false))

(def snek-inferences (atom {}))
(def snek-declarations (atom {}))

(defmacro defsnek [& body]
  (if (seq body)
    (let [body   (vec body)
          num    (count body)
          args   (subvec body 0 (- num 2))
          result (last body)
          arrow  (body (- num 2))]
      (assert (or (= arrow '->) (= arrow '-d>)))
      `(do (assert (not @snek-type))
           (reset! snek-debug ~(= arrow '-d>))
           (reset! snek-type [[~@args] ~result])))
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

(defmethod te/assert-expr 'inferences-valid? [msg form]
  `(let [failure# (when-let [[k#] (check-inferences @snek-inferences @snek-declarations)]
                    k#)
         result# (not failure#)]
     (te/do-report 
      {:type (if result# :pass :fail)
       :message (str "Inferred snek for " (first failure#) " should match declaration")
       :expected (raw-defsnek (second failure#))
       :actual (raw-defsnek (@snek-declarations (first failure#)))})
     result#))

(defmacro defn [nam args & body] ;; todo: support docstrings
  (let [raw-fun (symbol (str (name nam) "__"))]
    `(if @snek-type
       (let [[args-exp# result-exp#] @snek-type]
         (swap! snek-declarations assoc '~nam [args-exp# result-exp#])
         (declare ~nam)
         (core-defn ~raw-fun ~args
                    ~@body)
         (core-defn ~nam [& args#]
                    (do (when ~(identity @snek-debug)
                          (println (pr-str (cons '~nam (butlast (query (conj args-exp# ::xtra) (conj (vec args#) ::xtra)))))))
                        (when-let [delta# (mismatch (conj args-exp# ::xtra) (conj (vec args#) ::xtra))]
                          (throw (ex-info (str "Snek argument error in " '~nam ": Expected " args-exp# " but got " args# ", delta " (pr-str delta#)) {})))
                        (let [result# (apply ~raw-fun args#)]
                          (when-let [delta# (mismatch result-exp# result#)]
                            (throw (ex-info (str "Snek result error in " '~nam ": Expected " result-exp# " but got " result# ", delta " (pr-str delta#)) {})))
                          #_(add-inference '~nam (vec args#) result#)
                          (when ~(identity @snek-debug)
                            (println '~nam "results =" (pr-str (query result-exp# result#))))
                          result#)))
         (reset! snek-type nil))
       (core-defn ~nam ~args
                  ~@body))))

(core-defn snek [b]
           (defsnek))
;; @snek-inferences
