#!/usr/bin/env bb
;; -*- mode: clojure -*-

(ns ae.isogeny
  (:require [babashka.fs :as fs]
            [clojure.pprint :as pp]
            [clojure.tools.cli :as cli]
            [clojure.tools.logging :as log]
            [selmer.parser :as s.parser]
            [selmer.util :as s.util]))

(def usage
  "USAGE: isogeny -t TEMPLATE -c CONTEXT -o OUTPUT
TEMPLATE: Selmer template
CONTEXT: EDN context map
OUTPUT: output location")

(def cli-options
  [["-t" "--template TEMPLATE" "Template" :default *in* :default-desc "*in*"]
   ["-T" "--multi-template" "Render multiple templates provided as args"]
   ["-c" "--context CONTEXT" "EDN context" :default *in* :default-desc "*in*"]
   ["-C" "--context-string CONTEXT_STRING" "EDN context string"]
   ["-d" "--context-default CONTEXT_DEFAULT" "Default EDN context"]
   ["-o" "--output OUTPUT" "Output to file"]
   ["-a" "--add-tags ADD_TAGS" "Provide a file of additional tag definitions"]
   ["-s" "--strict" "Missing values throw exceptions" :id :strict?]
   ["-v" "--verbose" "Verbose output" :id :verbose?]
   ["-V" "--version" "Version"]
   ["-h" "--help" "Display help" :id :help?]])

(def *verbose?* (atom false))

;; Add the {% env %} tag to read environment variables.
(selmer.parser/add-tag!
 :env (fn [args _] (System/getenv (first args))))

(defn throw-on-missing
  "Causes an exception to be thrown when a missing value is encountered."
  []
  (when @*verbose?* (log/info "Throwing on missing values."))
  (s.util/set-missing-value-formatter!
   (fn [tag _]
     (-> "Missing value: "
         (str (or (:tag-value tag) (:tag-name tag)))
         Exception.
         throw))))

(defn add-tags! "Add additional tags to use when rendering the template."
  [add-tags]
  (when @*verbose?* (log/info "Loading tags from: " add-tags))
  (try (load-file add-tags)
       (catch Exception e
         (log/error "ERROR - Exception thrown loading additional tags:")
         (throw e))))

(defn read-context "Read the context, from a default file if it fails."
  [context context-default]
  (try (-> context slurp read-string)
       (catch Exception e
         (when @*verbose?* (log/info "Falling back on default context: " context-default))
         (try (-> context-default slurp read-string)
              (catch Exception e2
                (log/error "Neither the context nor its default can be found:" (.getMessage e))
                (throw e2))))))

(defn ->context "Create the context to be used to render."
  [context context-default context-string]
  (when @*verbose?* (log/info "Attempting to read context: " context))
  (let [context (if (= "-" context) *in* context)
        context-override (some-> context-string read-string)]
    (-> (read-context context context-default)
        (merge context-override)
        eval)))

(defn ->template
  "Read the template, either from file or STDIN."
  [template]
  (let [tmpl (or (if (= template "-") *in* nil) template *in*)]
    (when @*verbose?* (log/info "Reading template: " (if (= tmpl *in*) "STDIN" tmpl)))
    (try (slurp tmpl)
         (catch Exception e
           (log/error "Exception reading template: " template)
           (throw e)))))

(defn render "Render the template using the context."
  [template context]
  (try (s.parser/render template context)
       (catch Exception e
         (log/error "Rendering exception thrown:")
         (throw e))))

(defn write "Write content to output or STDOUT."
  [content output]
  (when @*verbose?* (log/info "Writing output to: " (or output "STDOUT"))
        (when-not output (log/info "Output: \n----------------")))
  (try (if output (spit output content) (print content))
       (catch Exception e
         (log/error "ERROR - Cannot write to: " output)
         (throw e))))

(defn render-one [ctxt template output]
  (let [tmpl (->template template)]
    (when @*verbose?* (log/info "Known variables: " (s.parser/known-variables tmpl)))
    (-> tmpl (render ctxt) (write output))))

(defn render-many
  [ctxt templates]
  (->> templates
       (map (fn [t] [t (fs/strip-ext t)]))
       (map #(render-one ctxt (first %) (second %)))))

(defn -main [args]
  (let [{{:keys [template multi-template context context-string context-default
                 output add-tags strict? verbose? version help?]
          :as options} :options
         :keys [arguments summary errors]} (cli/parse-opts args cli-options)]
    (when errors (log/error "Error parsing options and args:" errors))
    (when help? (println usage) (println summary) (System/exit 0))
    (when version (println "Isogeny 2.0") (System/exit 0))
    (when strict? (throw-on-missing))
    (when verbose? (swap! *verbose?* (constantly true)) (log/info "Options: " options))
    (when add-tags (add-tags! add-tags))
    (let [ctxt (->context context context-default context-string)]
      (if multi-template
        (render-many ctxt arguments)
        (render-one ctxt template output)))))

(-main *command-line-args*)
