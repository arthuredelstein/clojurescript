(ns webrepl
  (:require [cljs.core]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.reader :as reader]))

(def ^:dynamic *debug* false)
(def ^:dynamic *e nil)

(defn prompt [] (str *ns-sym* "=> "))

(def append-dom)

(defn dom [o]
  (if (coll? o)
    (let [[tag attrs & body] o]
      (if (keyword? tag)
        (let [elem (.createElement js/document (name tag))]
          (when (map? attrs)
            (doseq [[k v] attrs]
              (when v (.setAttribute elem (name k) v))))
          [(append-dom elem (if (map? attrs) body (cons attrs body)))])
        (mapcat dom o)))
    (when o
      [(.createTextNode js/document (str o))])))

(defn append-dom [parent v]
  (doseq [i (dom v)]
    (.appendChild parent i))
  parent)

(defn repl-print [log text cls]
  (doseq [line (.split (str text) #"\n")]
    (append-dom log
      [:div {:class (str "cg "
                         (when cls
                           (str " " cls)))}
       line]))
  (set! (.-scrollTop log) (.-scrollHeight log)))

(defn- read-next-form [text]
  (binding [*ns-sym* *ns-sym*]
    (reader/read-string text)))

(defn postexpr [log text]
  (append-dom log
    [:table
     [:tbody
      [:tr
       [:td {:class "cg"} (prompt)]
       [:td (.replace text #"\n$" "")]]]]))

(defn ep [text]
  (try
    (let [env (assoc (ana/empty-env) :context :expr)
          form (read-next-form text)
          _ (when *debug* (println "READ:" (pr-str form)))
          body (ana/analyze env form)
          _ (when *debug* (println "ANALYZED:" (pr-str (:form body))))
          res (comp/emit-str body)
          _ (when *debug* (println "EMITTED:" (pr-str res)))
          value (js/eval res)]
      (set! *3 *2)
      (set! *2 *1)
      (set! *1 value)
      (binding [*out* *rtn*] (print (pr-str value))))
    (catch js/Error e
      (binding [*out* *err*] (print  (.-stack e)))
      (set! *e e))))

(defn pep [log text]
 (postexpr log text)
 (ep text))

(set! (.-onload js/window) (fn []
  ;; Bootstrap an empty version of the cljs.user namespace
  (swap! comp/*emitted-provides* conj (symbol "cljs.user"))
  (.provide js/goog "cljs.user")
  (set! cljs.core/*ns-sym* (symbol "cljs.user"))

  (let [log (.getElementById js/document "log")
        input (.getElementById js/document "input")
        status1 (.getElementById js/document "status1")
        status2 (.getElementById js/document "status2")]

    ;; Setup the print function
    ;; Setup the print function
    (set! *out* #(repl-print log % nil))
    (set! *rtn* #(repl-print log % "rtn"))
    (set! *err* #(repl-print log % "err"))
    (set! *print-fn* #(*out* %1))

    (println ";; ClojureScript")
    (append-dom log [:div {:class "cg"}
      ";;   - "
      [:a {:href "http://github.com/kanaka/clojurescript"}
       "http://github.com/kanaka/clojurescript"]])
    (println ";;   - A port of the ClojureScript compiler to ClojureScript")
    (pep log "(+ 1 2)")
    (pep log "(defn sqr [x] (* x x))")
    (pep log "(sqr 8)")
    (pep log "(defmacro unless [pred a b] `(if (not ~pred) ~a ~b))")
    (pep log "(unless false :yep :nope)")

    (set! (.-onkeypress input)
          (fn [ev]
            (when (== (.-keyCode (or ev event)) 13)
              (try
                (let [form (reader/read-string (.-value input))]
                  (do
                    (pep log (.-value input))
                    (js/setTimeout #(set! (.-value input) "") 0)
                    (set! (.-visibility (.-style status1)) "visible")
                    (set! (.-visibility (.-style status2)) "hidden")
                    (set! (.-innerText (.getElementById js/document "ns")) (prompt))))
                (catch js/Error e
                  (if (re-find #"EOF while reading" (.-message e))
                    (do
                      (set! (.-visibility (.-style status1)) "hidden")
                      (set! (.-visibility (.-style status2)) "visible"))
                    (repl-print log e "err")))))))

    (.focus input))))
