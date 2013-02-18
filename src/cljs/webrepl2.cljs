(ns webrepl
  (:require [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.reader :as reader]))

(def ^:dynamic *debug* false)

;; jquery

(def $ js/$)

;; handling user code inputs

(defn evaluate-next-form
  "Evaluates next clojure form in reader. Returns a map, containing
   either resulting value and emitted javascript, or an error
   object, or {:finished true}."
  [rdr]
  (try
    (let [form (reader/read rdr false ::finished-reading)
          _ (when *debug* (println "READ:" (pr-str form)))]
      (if (= form ::finished-reading)
        {:finished true}
        (let [env (assoc (ana/empty-env) :context :expr)
              body (ana/analyze env form)
              _ (when *debug* (println "ANALYZED:" (pr-str (:form body))))
              res (comp/emit-str body)
              _ (when *debug* (println "EMITTED:" (pr-str res)))
              value (js/eval res)]
          {:value value :js res})))
    (catch js/Error e
      {:error e :line-number (reader/get-line-number rdr)})))

(defn evaluate-code
  "Evaluates some text from REPL input. If multiple forms are
   present, evaluates in sequence until one throws an error
   or the last form is reached. The result from the last
   evaluated form is returned. *1, *2, *3, and *e are updated
   appropriately." 
  [text]
  (let [rdr (reader/indexing-push-back-reader text)]
    (loop [last-output nil]
      (let [output (evaluate-next-form rdr)]
        (if-not (:finished output)
          (if-let [err (:error output)]
            (do (set! *e err)
                output)
            (recur output))
          (do (set! *3 *2)
              (set! *2 *1)
              (set! *1 (:value last-output))
              last-output))))))

(defn print-error [{:keys [error line-number]}]
  (print error "at line" line-number))

(defn handle-input [input]
  (let [evaluated (evaluate-code input)]
    (if (:error evaluated)
      (binding [*out* *err*] (print-error evaluated))
      (try
        (binding [*out* *rtn*] (print (pr-str (:value evaluated))))
        (catch js/Error e
          (binding [*out* *err*] (println err)))))))

(defn complete-form? [text]
  (try
    (reader/read-string text)
    true
    (catch js/Error e
           (not (re-find #"EOF while reading" (.-message e))))))

;; console

(defn prompt [] (str *ns-sym* "=> "))

(defn start-prompt 
  ([initial-text]
    (let [prompt-label (str "\n" (prompt))
          continue-label (str (apply str (repeat (- (count prompt-label) 4) " "))
                              "...")]
      (.SetPromptLabel js/jqconsole prompt-label continue-label)
      (.Prompt js/jqconsole "true"
               (fn [input]
                 (handle-input input)  
                 (start-prompt))
               #(if (complete-form? %)
                  false
                  0))
      (when-not (empty? initial-text)
        (.SetPromptText js/jqconsole initial-text))))
  ([] (start-prompt nil)))

(defn cancel-input [message]
  (let [prompt-text (.GetPromptText js/jqconsole false)]
    (doto js/jqconsole
      .ClearPromptText
      .AbortPrompt
      (.Write message "jqconsole-output"))
    prompt-text))
      
(defn setup-console
  "Setup the REPL console."
  [evaluate-file-callback]
  (set! js/jqconsole
        (.jqconsole ($ "#console")
                    "ClojureScript-in-ClojureScript Web REPL"
                    "\n>>> "
                    ""))
  (.SetIndentWidth js/jqconsole 1)
  ;; Setup the print function
  (set! *out* #(.Write js/jqconsole %))
  (set! *rtn* #(.Write js/jqconsole % "jqconsole-output"))
  (set! *err* #(.Write js/jqconsole % "jqconsole-message-error"))
  (set! *print-fn* #(*out* %1))
  ;; key binding
  (.RegisterShortcut js/jqconsole "E" evaluate-file-callback)
  (start-prompt))

;; file storage

(defn store-item [key text]
  (-> js/window .-localStorage (.setItem key text)))

(defn load-item [key]
  (-> js/window .-localStorage (.getItem key)))

;; editor

(defn evaluate-file [editor]
  (let [text (.getValue editor)
        prompt-text (cancel-input "Evaluating file...\n")]
    (handle-input text)
    (store-item "scratch" text)
    (start-prompt prompt-text)))

(defn- map->js [m]
  (let [out (js-obj)]
    (doseq [[k v] m]
      (aset out (name k) v))
    out))

(defn str-contains? [s x]
  (not= (.indexOf s x) -1))

(defn mac? []
  (str-contains? (str (.-platform js/navigator)) "Mac"))

(def command-prefix (if (mac?) "Cmd" "Ctrl"))

(defn setup-editor []
  (let [eval-cmd (str command-prefix "-E")]
    (.html ($ "#tiny-note")
           (str "Press " eval-cmd
                " to evaluate file in REPL."))
    (doto
      (.fromTextArea js/CodeMirror
                     (.getElementById js/document "editor")
                     (map->js {:mode "clojure"
                               :lineNumbers true
                               :matchBrackets true
                               :extraKeys (map->js {eval-cmd
                                                    evaluate-file})}))
    (.setValue (or (load-item "scratch") ";; Develop your clojurescript program here")))))

;; show/hide editor
  
(def editor-visible (atom false))

(def editor-visible-key "__editor_visible")

(defn update-editor-visibility [show?]
  (let [container ($ "#editor-container")]
    (if show?
      (.slideDown container 100)
      (.slideUp container 100))))

(defn store-editor-visibility [show?]
  (store-item editor-visible-key (str show?)))

(defn update-link [show?]
  (.html ($ "#toggle-editor")
         (str (if show? "Hide" "Show")
              " file editor")))

(defn add-updating-watch [reference fun]
  (add-watch reference fun (fn [_ _ _ value] (fun value))))

(defn setup-editor-toggling []
  (add-updating-watch editor-visible update-editor-visibility)
  (add-updating-watch editor-visible store-editor-visibility)
  (add-updating-watch editor-visible update-link)
  (.click ($ "#toggle-editor") #(swap! editor-visible not))
  (reset! editor-visible (= "true" (load-item editor-visible-key))))

;; startup

(.ready ($ js/document)
  (fn []
    ;; Bootstrap an empty version of the cljs.user namespace
    (swap! comp/*emitted-provides* conj (symbol "cljs.user"))
    (.provide js/goog "cljs.user")
    (set! cljs.core/*ns-sym* (symbol "cljs.user"))

     (setup-editor-toggling)
   
     ;; setup the editor and console
    (let [editor (setup-editor)]
      (setup-console (fn [] (evaluate-file editor)))
      (def editor editor))



    ;; print,evaluate,print some example forms
    ;(pep "(+ 1 2)")
    ;(pep "(let [sqr #(* % %)] (sqr 8))")
    ;(pep "(defmacro unless [pred a b] `(if (not ~pred) ~a ~b))")
    ;(pep "(unless false :yep :nope)")
    ))


