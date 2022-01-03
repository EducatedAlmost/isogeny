#!/usr/bin/env bb

(ns ae.isogeny
  (:require [clojure.tools.cli :refer [parse-opts]]
            [babashka.classpath :refer [add-classpath]]
            [clojure.pprint :as pp]))

(def help-explanation
  "USAGE: isogeny -t TEMPLATE -c CONTEXT -o OUTPUT")

(def cli-options
  [["-t" "--template TEMPLATE" "Template" :default *in*]
   ["-c" "--context CONTEXT" "EDN context" :default *in*]
   ["-C" "--context-string CONTEXT_STRING" "EDN context string"]
   ["-d" "--context-default CONTEXT_DEFAULT" "Default EDN context"]
   ["-o" "--output OUTPUT" "Output to file"]
   ["-a" "--add-tags ADD-TAGS" "Provide a file of additional tag definitions"]
   ["-s" "--strict" "STRICT" "Missing values throw exceptions"]
   ["-v" "--verbose" "Verbose output"]
   ["-h" "--help"]])

;; Add the {% env %} tag to read environment variables.
(selmer.parser/add-tag!
 :env (fn [args _] (System/getenv (first args))))

(defn throw-on-missing
  "If --strict is used, this sets Selmer to throw on missing values."
  []
  (selmer.util/set-missing-value-formatter!
   (fn [tag context=map]
     (-> "Missing value: "
         (str (or (:tag-value tag) (:tag-name tag)))
         Exception.
         throw))))

(defn read-context
  "Read the context, falling back on the default context if it can't be found."
  [options]
  (let [context-path (if (= "-" (options :context)) *in* (options :context))
        context-default (options :context-default)]
    (try (-> context-path slurp read-string)
         (catch java.io.FileNotFoundException e
           (if (options :verbose) (println "Using default context: " context-default))
           (-> context-default slurp read-string)))))

(defn ->context
  "Create the context to be used."
  [options]
  (let [context (read-context options)
        context-string (some-> options :context-string read-string)]
    (eval (merge context context-string))))

(defn ->template
  "Read the template, either from file or STDIN."
  [options]
  (-> (or (options :template) *in*) slurp))

(defn add-tags!
  "Add additional tags for the rendering of the template."
  [options]
  (try (some-> options :add-tags slurp read-string eval)
       (catch clojure.lang.ExceptionInfo e
         (println "Exception thrown loading additional tags:")
         (throw e))))

(defn render
  "Render the template in the context."
  [template context]
  (try (selmer.parser/render template context)
       (catch clojure.lang.ExceptionInfo e
         (println "Selmer rendering exception thrown:")
         (throw e))))

(defn write
  "Write the output either to file or STDOUT."
  [out options]
  (if-let [output (options :output)]
    (spit output out)
    (print out)))

(defn -main []
  (let [{{help :help verbose? :verbose strict? :strict :as options} :options
         args :arguments} (parse-opts *command-line-args* cli-options)]
    (if verbose? (println "Options: " (with-out-str (pp/pprint options))))
    (if strict? (throw-on-missing))
    (if help (pp/pprint cli-options)
        (let [context (->context options)
              template (->template options)]
          (add-tags! options)
          (if verbose? (println "Known variables: " (selmer.parser/known-variables template) "\n"))
          (println "Output:")
          (-> template
              (render context)
              (write options))))))

(-main)
