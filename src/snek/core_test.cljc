(ns snek.core-test
  (:refer-clojure :rename {defn   core-defn
                           update core-update})
  (:require [snek.core
             :as sn
             :refer [defsnek
                     defn
                     valid?
                     typ
                     generalize
                     query
                     mismatch
                     update
                     instance
                     optional-key
                     restructure
                     parse]]
            [fbc-utils.test :as te]))

(defmacro error-name-DELETE [& body]
  `(try ~@body
        (catch Exception e#
          (str e#))))

(te/test (do (defsnek)
             (defsnek 0 0 -> 0)
             (defn myadd [a b]
               (+ a b))
             (myadd 1 2))
         3
         (valid? [0 0] [0 :foo])
         false
         (valid? :_ :a)
         true
         (mismatch {:_ 0} {:a 4})
         nil
         (valid? {:_ 0} {:a 4})
         true
         (valid? {:_ 0}
                 {"b" :foo
                  :a  4})
         true
         (valid? {:_ 0}
                 {"b" :foo
                  :a  4})
         true
         (valid? {:_ 0}
                 {"b" :foo
                  :a  :derp})
         false
         (do (defsnek)
             (defsnek 0 :foo -> 0)
             (defn myadd2 [a b]
               (+ a b))
             (error-name-DELETE (myadd2 1 2)))
         "clojure.lang.ExceptionInfo: Snek argument error in myadd2: Expected [0 :foo] but got (1 2), delta (:foo) {}"
         (do (defsnek)
             (defsnek 0 0 -> 0)
             (defn myadd3 [a b]
               :derp)
             (error-name-DELETE (myadd3 1 2)))
         "clojure.lang.ExceptionInfo: Snek result error in myadd3: Expected 0 but got :derp, delta 0 {}"
         (valid? {:foo 0} {:health 5})
         false
         (valid? {:health 0} {:health 5})
         true
         (typ {})
         :map
         (do (defsnek)
             (defsnek -> 0)
             (defn derp []
               5)
             (derp))
         5
         (valid? [0] [1 2 3])
         true
         (valid? nil 1)
         true
         (valid? [nil] [1 2])
         true
         (valid? [[nil]] [[1 2] [3 4]])
         true
         (valid? [0] (repeat 1)) ;;gotta halt with infinite lists
         true
         (generalize 3 4)
         0
         (generalize [0 0] [0 :foo])
         [0 nil]
         (generalize [4] [1 2 3])
         [0]
         (generalize {:foo 1 :bar 6} {:foo 2 :bar 6})
         {:foo 0 :bar 6}
         (generalize [0 0] [1 2 3])
         [0]
         (generalize {:foo 1} {:bar 2})
         {:_ 0}
         (generalize {1 5 :foo 3} {2 3 :foo :bar :baz :qux})
         {:foo nil 0 0 :_ nil}
         (generalize {:foo 3} {:bar 7})
         {:_ 0}
         (generalize {:baz 1 :foo 3} {:baz 1 :bar 7})
         {:baz 1 :_ 0}
         (generalize {[1 2] :foo [3 3] :bar} {[1 2] :foo [4 4] :baz})
         {[1 2] :foo [0 0] :_}
         (generalize {[3 3] :bar} {[4 4] :baz})
         {[0 0] :_}
         (generalize nil [1 2])
         nil
         (generalize (repeat :foo) (repeat 5))
         [nil]
         (generalize (repeat 1) (repeat 2))
         [0]
         (valid? {0 :a 1 :b} {0 :a 1 :b}) ;;malformed reference because 0 is ambiguous
         false
         (generalize {0 :_} {0 :a 1 :b})
         {0 :_}
         (generalize {2 :c} {0 :a 1 :b})
         {0 :_}
         (generalize {2 "foo" :z 99} {0 :a 1 :b})
         {0 nil :_ 99}
         (generalize {2 :foo :z 99} {0 :a 1 :b})
         {0 :_ :_ 99}
         (generalize {0 :a 1 :b :z 88} {2 :foo :z 99})
         {0 :_ :z 0}
         (generalize {0 :a 1 :b} {0 :a :d :e})
         {0 :_ :_ :e}
         (generalize {0 :b 1 :c} {0 :b 1 :c})
         {0 :_ 1 :c}
         (generalize {0 {:foo 5} 1 {:foo 6}} {0 {:foo 7} 1 {:foo 8}})
         {0 {:foo 0} 1 {:foo 0}}
         (valid? {0 {:foo 0} 1 {:foo 0}} {0 {:foo 0} 1 {:foo 0}})
         true
         (generalize {0 {:pos [9 9], :health 100, :db/id 0}, 1 {:pos [9 7], :health 100, :db/id 1}, 2 {:pos [8 7], :health 100, :db/id 2}, 3 {:pos [8 9], :health 100, :db/id 3}} {0 {:pos [9 9], :health 100, :db/id 0}, 1 {:pos [9 7], :health 100, :db/id 1}, 2 {:pos [8 7], :health 100, :db/id 2}, 3 {:pos [8 9], :health 100, :db/id 3}})
         {0 {:pos [0 0], :db/id 0, :health 100}, ;;note that pos is [0 0] now
          1 {:pos [9 7], :db/id 1, :health 100},
          3 {:pos [8 9], :db/id 3, :health 100},
          2 {:pos [8 7], :db/id 2, :health 100}}
         (valid? nil {0 1})
         true
         (valid? {0 :foo} {})
         true
         (generalize {} {3 :foo})
         {0 :foo}
         (generalize {:a 1} {:a 1 :b :foo})
         {:a 1 :_ nil}
         (valid? #{} #{:a 5})
         true
         (valid? [false] [true false 5])
         false
         (valid? [false] [true false])
         true
         (generalize true false)
         false
         (generalize #{3} #{3 7})
         #{0 3}
         (query [4 :a] {4 :foo :a 1})
         [:foo 1]
         (query {:a nil} {:a 5 :b 3})
         {:a 5}
         (query nil :foo)
         :foo
         (mismatch {:a 5 0 {:foo 5}} {:a 4 3 {:foo 5} 7 {:foo 3}})
         {:a 5 0 {7 {:foo 5}}}
         (mismatch {:a 4 0 {:foo 5}} {:a 4})
         nil
         (mismatch [:_] [])
         nil
         (query {nil nil} {:a 3 :b :foo :c #{:yup}})
         {:a 3 :b :foo :c #{:yup}}
         (query {:_ nil} {:a 3 :b :foo :c #{:yup}})
         {:a 3 :b :foo :c #{:yup}}
         (query {:_ nil :c :z} {:a 3 :b :foo :c {:z 3}})
         {:a 3 :b :foo :c 3}
         (query {:_ nil :c [:x :y]} {:a 3 :b :foo :c {:z 3 :x 4 :y 2}})
         {:a 3 :b :foo :c [4 2]}
         (query {:_ nil :c {:x nil :y nil}} {:a 3 :b :foo :c {:z 3 :x 4 :y 2}})
         {:a 3 :b :foo :c {:x 4 :y 2}}
         (mismatch [{:derp 4} 0] [{:size 4} 1 :attack])
         [{} 0]
         (update {:foo inc} {:foo 1})
         {:foo 2}
         (update {:foo {:bar inc}} {:foo {:bar 1}})
         {:foo {:bar 2}}
         (update [inc] [1 2 3])
         [2 3 4]
         (update {:bar 2} {:foo 1})
         {:foo 1
          :bar 2}
         (update {:bar (fn [k]
                         (if k
                           (inc k)
                           3))
                  :foo inc}
                 {:foo 1})
         {:foo 2
          :bar 3}
         (update {:foo [inc]} {:foo [1 2]})
         {:foo [2 3]}
         (update {nil inc}
                 {:foo 2
                  :bar 5})
         {:foo 3
          :bar 6}
         (update {:foo inc}
                 {:foo 1})
         {:foo 2}
         (update {:foo [1 1 1]}
                 {})
         {:foo [1 1 1]}
         (update {:foo [1 1 1]}
                 {:foo [0 0 0]})
         {:foo [1 1 1]}
         (update {:foo [[inc]]}
                 {:foo [[1 2 3] [5 6 7]]})
         {:foo [[2 3 4] [6 7 8]]}
         (update [:foo] [{:foo 5} {:foo 3}])
         [5 3]
         (update :foo {:foo 3})
         3
         (update {1 :derp} [:a :b :c])
         [:a :derp :c]
         (update {:db/id [1 0 0]}
                 {:db/id [0 0 0]})
         {:db/id [1 0 0]}
         (update {0 inc} {0 4 1 10})
         {0 5
          1 11}
         (query {:size 0} {:size 8})
         {:size 8}
         (query 0 8)
         8
         (query [{:a nil}] [{:a 3 :b 6} {:a 7}])
         [{:a 3} {:a 7}]
         (query [{:b nil} {:a nil}] [{:a 3 :b 6} {:a 7}])
         [{:b 6} {:a 7}]
         (query #{1} #{1 2 4})
         #{1}
         (query #{0} #{1 2 4})
         #{1 2 4}
         (instance [3])
         []
         (instance {:foo 34 :bar [3] :baz [2 3] :qux {0 :zup}})
         {:foo 34 :bar [] :baz [2 3] :qux {}}
         (valid? {:foo nil} {})
         false
         (error-name-DELETE (query {:foo nil} {:derp 45}))
         "clojure.lang.ExceptionInfo: Missing query key :foo in data {}"
         (optional-key :?foo)
         :foo
         (optional-key :foo)
         nil
         (optional-key "foo")
         nil
         (mismatch {:a 0 :?b 0} {:a 4 :b 6})
         nil
         (mismatch {:a 0 :?b 0} {:a 4})
         nil
         (mismatch {:a 0 :?b 0} {:a 4 :b :foo})
         {:b 0}
         (restructure '[a b c] gensym)
         '[[a b c]
           (clojure.core/list a b c)]
         (restructure '[a b & c] gensym)
         '[[a b & c]
           (clojure.core/concat (clojure.core/list a b) c)]
         (restructure '[a {:keys [foo]}] (fn []
                                           'gensym))
         '[[a {:keys [foo] :as gensym}]
           (clojure.core/list a gensym)]
         (restructure '[[x y]]
                      (fn []
                        'gensym))
         '[[[x y :as gensym]] (clojure.core/list gensym)]
         (parse [0 :_] "33 :foo")
         [33 :foo]
         (valid? '_ 'foo)
         true
         (parse '[_] "barf")
         '[barf]
         (parse [0 :_] "33 foo")
         nil
         (parse [] "")
         []
         (parse [] "0")
         nil
         (parse [0] "")
         []
         (valid? {:?foo 0} {:foo "bar"})
         false
         (valid? {:foo 0} {:foo 4 :bar 2})
         true)

