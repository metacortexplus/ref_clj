;; Expression -- code that can be exluated for a result.
;; Form -- valid expression that can be evaluated.


;; Symbols -- same as variables, when evaluated returns thing it refers to.
;; Use namespaces to orginize.
(def foo "bar") ;; not a direct bind, creates var object, uses namespace
                                ;; (*namespace*/foo, deafult namspace is "user"),
                                ;; global
(let [foo "bar"] *expressions*) ;; local binding, uses vector form (symbol & value)
                                                              ;; special form, binding form
                                                              ;; evaluates to last expression
(let [a "somestring"]
    [b "anotherstring"] [a, b]) ;; -> ["somestring" "anotherstring"]

(declare ^:dynamic *a*) ; dynamic variables that can have their value changed within a particular scope
(binding [*a* "Hello"]) ;

(def global-val (atom nil)) ; atom, mutable type
(deref global-val) ; -> nil
@global-val ; shorthand for deref
(reset! global-val 42) ; 
(swap! global-val inc) ; -> 43, accept function as second argument
;; ! in name used for functions that operate on mutable data

(def names (ref []) ;
(dosync							; open transaction
    ref-set
    alter) ;

;;;;;;;;;;;;;;;;;;;; Destructuring

(let [[symb1 symb2] [exp1 exp2]]) ;; destructuring, symb1 = exp1; symb2 = exp2
(let [symb1 [symb2]] [exp1 [exp2]]) ;; nested
(let [[symb1 [symb2] :as symb3] [exp1 [exp2]]]) ;; binding data structure;
                                                                                                ;; symb3 = [symb1[symb2]]
(let [{symb1 :kw1 :as symb3} {:kw1 "abc"}]) ;; same for map; symb3 = {:kw1 "abc"}

(let [{symb1 :kw1 symb2 :kw2} ;; bind to value of the keys in the map
            {:kw1 "abc" :kw2 "xyz"}]) ;; symb1 = "abc", symb2 = "xyz"

(let [{symb1 :kw1 symb2 :kw2 :or {symb2 value}} ;; :or for default if :kw3 missing in map 
            {:kw1 "abc"}]) 

(let [{:keys [symb1 symb2]} {:kw1 "abc", :kw2 "xyz"}]) ; :keys directive, most common way
(defn register [{:keys [id pass repeat-pass] :as user}]) ; extract keys while preserving the original map



;;;;;;;;;;;;;;;;;;;; Namespaces

(ns some.namespace) ;; creates ns and switch to it
                    ;; if switched to another namespace, use full name for vars:
                    ;; other.ns/symbol
*ns* ;; check namespace
(require 'namespace) ;; load the lib, accessed by fully quialified ns
                     ;; as clojure.set/union
(require '[namespace] :as *ns.alias*) ;;
(ns *namespace*
    (:require [*other.ns* :as *ns.alias*])) ;; more common
(ns *namespace*
    (:require [*other.ns* :refer :all]
            [*other.ns* :refer :all])) ; import all symbols, conflict prone
                                                                              ; (use) is the same
(ns my.ns
  (:use other.ns) ; import everything from other.ns to my.ns

(ns my.ns
  (:use [other.ns :only [some-func]])) ; import only some-func to my.ns

;;;;;;;;;;;;;;;;;;;; Functions

(declare ...) ; declare function; used for mutualy recursive functions

(defn func-name 
    "Documenting this func and `arg`"
    [*parameters* & opts] *body*) ;; & opts (not keyword) for optional args
                                                                                            ;; same as (def fn-name (fn [] *body*))
(defn *name*
    ([] *body*)
    ([a] *body*)
    ([a b] *body*)
    ([...] *body*)) ;; variadic defn

(func-name) ;; calling a function
(fn [] *parameters*) ;; anon func, put inside () to call
(# *body %* *param*) ;; short form for anon func with 1 param (%)
(# *body %1 %2* *params*) ;; same with more then 1 parameters 


(defmulti area :shape) ; multimethod
(defmethod area :rectangle [{:keys [l w]}]
    (* l w))

(defprotocol) ; protocol (interface/trait)
(deftype) ;  create type that implements protocol
(extend-protocol) ;

(partial ) ;; partial application

(defn somefunc [param]
 ((comp func1 func2) param) ;; composing functions

(apply ... ...) ;;

(do (...)) ;; common use for side effects

(dotimes [*counter* *times*]
    (...)) ;;

(map ...)
(pmap ...) ; paralell map




;;;;;;;;;;;;;;;;;;;; Simple values

1 ; 64-bit integers
1N ; arbitrary pressision

01 ; octal
0x1 ; hexadecimal

1.0 ; double-precision 64-bit floats
1.0M ; arbitrary pressision float
1/3 ; ratio

"word" ; string
:kw ; keyword
\a ; char
\uNNNN ; unicode
\oNNN ; octal unicode

\newline \spec \tab

true ; bool, logical true
false ; bool, logical false

nil ; value absence, only logical false beside false

;; Vars provide mutable storage locations. These can be bound and rebound
;; on a per-thread basis.

;;The special symbolic values:
##Inf
##-Inf
##NaN

#"[0-9]+" ; regular expression; compiled to java.util.regex.Pattern objects

;; Operators

+ ; plus
+' ; supports arbitrary precision
- ;

;; Math

(quot 8 3) ; quotient; division ; -> 2

;; COLLECTIONS
;; immutable, persistent (structural sharing)
;; implement clojure.lang.IPersistent.Collection interface
;; ISeq interface

;; List
;; traverse optimised

'(1 "hello" :some-keyword) ; list, ordered, optional comas to separate
(list 1 2 3 4 5) ; create list with function
(cons 5 '()) ; add 5 to empty list
(cons 5 nil) ; same




;; Vector
;; IFn protocol (callable as functions)

[:kw 42 "word"] ;; fast index access
[nth [vector] index] ;; access element of vector


;; Maps (key-value pairs)
;; can be sorted

{:kw1 "foo", :kw2 "bar"} ;; comas are OK with maps
(get {:kw1 "foo", :kw2 "bar"} :kw1 "not found") ;;
(:kw1 {:kw1 "foo", :kw2 "bar"}) ;; more idiomatic way to get value, :kw1 as function
(keys {:kw1 "foo", :kw2 "bar"}) ;; -> (:kw1 :kw2)
(vals {:kw1 "foo", :kw2 "bar"}) ;; -> ("foo" "bar")
(assoc {:kw1 "foo", :kw2 "bar"} :kw1 "new-value") ;; change
(dissoc {:kw1 "foo", :kw2 "bar"} :kw1) ;; remove
(merge {} {}) ;; merge maps

;; maps func

(hash-map) ;;
(zipmap) ;;


;; Records (class-like structure)
;; allows create instences of type-name with defined fields;
;; two factory functions (positional and map) created automatically

(defrecord *type-name* [ *field*
                                                    ...
])

(def *instance-name*
    (->*record* *attr-value* ...)) ;; positional factory

(def *instance-name*
    (map->*record* {:*attr* *value*
                                    ...})) ;; map factory, more flexible



;; Sets
;; unique elements, fast search, any values
;; clojure.set

#{} ;; duplicates aren't allowed at creation, error
(clojure.set/union #{} #{}) ;;
(clojure.set/difference #{} #{}) ;;
(clojure.set/intersection #{} #{}) ;;
(set *collection*) ;; converting to set
(set {:a 1, :b 2, :c 3}) ;; -> #{[:c3] [:b 2] [:a 1]} converted to set of vectors

(get #{:a :b :c} :a) ; -> :a
(:a #{:a :b :c}) ; same, -> :a
(#{:a :b :c} :a) ; using set as a function (predicate), -> :a

(contains? #{:a :b :c} :a) ; -> true

;; Collection functions

(first '(:kw1 :kw2 :kw3 :kw4)) ;
(rest '(:kw1 :kw2 :kw3 :kw4)) ;
(last) ;
(nth *collection* *index*) ; access element of collection by index (slow with list)
(first (rest (rest (rest (rest '(:rabbit :pocket-watch :marmalade :door)))))) ;; -> nil

(count *collection*) ;; returns size of collection
(conj *collection* *elements(s)*) ;; adds (in the most natural way for that data structure)
                                  ;; vector at the end, list at the beginning 
                                  ;; one or more elements to the collection
                                  ;;
;; conj adds to the end of vectors
(conj [:toast :butter] :jam)
;; -> [:toast :butter :jam]
(conj #{:a :b} :c) ;; add element
(disj #{:a :b} :b) ;; remove element, -> #{:a}

(seq ) ;; turns collection into a sequence (walkable list abstraction for collection ds)
(seq []) ;; -> nil, idiomatic way to check if collection is not empty

(empty? []) ;; -> true
(empty? {}) ;; -> true
(empty? '()) ;; -> true

;; Check against elements
(every? *predicate* *collection*) ;;
(not-any? *predicate* *collection*) ;;
(some *predicate* *collection*) ;; returns true on first true case
(some #{4 5} [1 2 3 4 5]) ;; -> 4; using set as predicate
(some #{nil} [nil nil nil]) ;; -> NIL!
(some #{false} [false false false]) ;; -> NIL!

;; LOGIC

(true? ) ;;
(false? ) ;;
(nil? 1) ;; -> false

(not false) ;; negation, -> true
(not nil) ;; -> true
(not 0) ;; -> false
(not "string") ;; -> false

(= :abc 123) ;; -> false
(not= :abc 123) ;; -> true
(= '(:a, :b) [:a, :b]) ;; -> true


;; CONTROL

(if *true* *this* *else-this*) ;;
(let [*symbol* *expression*] ;; use if-let for this
    (if *symbol-eval-true*
        *this*
        *else-this*))
(if-let [*symbol* *expression*] ;;
    *this-if-symbol-eval-true*
    *else-this*)
(when *predicate* *expression*) ;;
(when-let [*symbol* *expression*]
    *expression-if-symbol-eval-true*) ;; if false -> nil

(cond
    (*test* *expression1* *expression2*) *if-test-true*) ;; similar to if-elseif in other lang
    (*test* *expression1* *expression2*) *if-test-true*) ;; return first true clause
    (*test* *expression1* *expression2*) *if-test-true*) ;; order is important
    :else *this*) ;; if no matches and no deafult (not a special word) -> nil 

(let [*symbol* *expression*] ;; shortcut for cond (within let)
    (case *symbol-value*
        *matches-this* *return-this*
        *matches-this* *return-this*
        *default*)) ;; if no default -> ERROR

;;;;;;;;;;;;;;;;; MACRO
;; execute before compile time


(defmacro) ;
;; ~ -- unquoting; indicates that we’d like to replace the name
;; with the value it refers to
;; ~@ -- unquote splicing; used with sequence
(macroexpand-l `()) ; expanding macro

;;;;;;;;;;;;;;;;;;;;; JAVA

(ns my.ns
    (:import java.io.File))

(ns my.ns
    (:import [java.io File FileInputStream ...])) ; import multiple classes from the same package
(new File ".") ; create instance of a class
(File. ".") ; shorthand

;; / used for calling a static method or variable in a class

(.. (File. ".") getAbsoluPath getBytes) ; chain multiple methods call


;;;;;;;;;;;;;;;;;;;;;;; READER CONDITIONALS
;; allow mixing code that targets multiple platforms
;; in the same file (*.cljc)