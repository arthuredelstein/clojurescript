(ns webrepl
  (:require [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.reader :as reader]))

(def ^:dynamic *debug* false)
(def ^:dynamic *e nil)

(defn prompt [] (str *ns-sym* "=> "))

(defn- read-next-form [text]
  (binding [*ns-sym* *ns-sym*]
    (reader/read-string text)))

(defn evaluate-code [text]
  (try
    (let [env (assoc (ana/empty-env) :context :expr)
          form (read-next-form (str "(do\n" text "\n)"))
          _ (when *debug* (println "READ:" (pr-str form)))
          body (ana/analyze env form)
          _ (when *debug* (println "ANALYZED:" (pr-str (:form body))))
          res (comp/emit-str body)
          _ (when *debug* (println "EMITTED:" (pr-str res)))
          value (js/eval res)]
      (set! *3 *2)
      (set! *2 *1)
      (set! *1 value)
      {:value value :js res})
    (catch js/Error e
      (set! *e e)
      {:error (.-stack e)})))

(defn- build-msg
  [title msg klass]
  {:msg (str title msg)
   :className klass})

(defn handle-input [input]
    (let [evaluated (evaluate-code input)]
      (if-let [err (and evaluated (:error evaluated))]
        (build-msg "Compilation error: " err "jqconsole-message-error")
        (try
          (build-msg "" (pr-str (:value evaluated)) "jqconsole-output")
          (catch js/Error e
            (build-msg "Error: " e "jqconsole-message-error"))))))

(defn complete-form? [text]
  (try
    (reader/read-string text)
    true
    (catch js/Error e
           (not (re-find #"EOF while reading" (.-message e))))))

(defn respond-to-input [input]
  (let [msg (handle-input input)]
    ;(js/alert (pr-str msg))
    (.Write js/jqconsole (:msg msg) (:className msg))))

(defn start-prompt []
  (let [prompt-label (str "\n" (prompt))
        continue-label (str (apply str (repeat (- (count prompt-label) 5) " "))
                            "...")]
    (.SetPromptLabel js/jqconsole prompt-label continue-label)
    (.Prompt js/jqconsole "true"
             (fn [input]
               (respond-to-input input)  
               (start-prompt))
             #(if (complete-form? %)
                false
                0))))

(defn store-file-text [key text]
  (-> js/window .-localStorage (.setItem key text)))

(defn load-file-text [key]
  (-> js/window .-localStorage (.getItem key)))

(defn evaluate-file [editor]
  (let [text (.getValue editor)]
    (.AbortPrompt js/jqconsole)
    (.Write js/jqconsole "\n\nEvaluating file...\n" "jqconsole-output")
    (respond-to-input text)
    (store-file-text "scratch" text))i
  (start-prompt))

(defn- map->js [m]
  (let [out (js-obj)]
    (doseq [[k v] m]
      (aset out (name k) v))
    out))

(defn setup-editor []
  (doto
    (.fromTextArea js/CodeMirror
                   (.getElementById js/document "editor")
                   (map->js {:mode "clojure"
                             :lineNumbers true
                             :matchBrackets true
                             :extraKeys (map->js {"Cmd-E" evaluate-file})}))
    (.setValue (load-file-text "scratch"))))

(.ready (js/jQuery js/document)
  (fn []
    ;; Bootstrap an empty version of the cljs.user namespace
    (swap! cljs.compiler/*emitted-provides* conj (symbol "cljs.user"))
    (.provide js/goog "cljs.user")
    (set! cljs.core/*ns-sym* (symbol "cljs.user"))
    
    ;; setup the REPL console
    (set! js/jqconsole
          (.jqconsole (js/jQuery "#console")
                      "ClojureScript-in-ClojureScript Web REPL"
                      "\n>>> "
                      ""))
    (.SetIndentWidth js/jqconsole 1)
    (set! *print-fn* #(.Write js/jqconsole %))
    (start-prompt)
    
    (def e (setup-editor))

    ;; print,evaluate,print some example forms
    ;(pep "(+ 1 2)")
    ;(pep "(let [sqr #(* % %)] (sqr 8))")
    ;(pep "(defmacro unless [pred a b] `(if (not ~pred) ~a ~b))")
    ;(pep "(unless false :yep :nope)")
    ))


