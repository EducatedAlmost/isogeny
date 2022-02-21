#!/usr/bin/env bb
;; -*- mode: clojure -*-

(ns ae.isogeny
  (:gen-class)
  (:require
   [babashka.fs :as fs]
   [cheshire.core :as json]
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [clojure.tools.cli :as cli]
   [clojure.tools.logging :as log]
   [selmer.parser :as s.parser]
   [selmer.util :as s.util]))

(def verbose? (atom false))
(def safe? (atom false))
(def dry-run? (atom false))

(def usage
  "USAGE: isogeny render -t TEMPLATE -c CONTEXT -o OUTPUT
       isogeny prepare CONFIG_FILE
TEMPLATE: Selmer template
CONTEXT: EDN context map
OUTPUT: output location")

(def general-opts
  [["-h" "--help" "Display usage information." :id ::+help?]
   ["-V" "--version" "Print the version of Isogeny." :id ::+version?]
   ["-v" "--verbose" "Print execution details." :id ::+verbose?]
   [nil, "--strict" "Throw an exception for missing values." :id ::+strict?]
   [nil, "--safe" "Do not alter existing files." :id ::+safe?]
   [nil, "--dry-run" "Run without writing anything." :id ::+dry-run?]])

(def render-opts
  [["-t" "--template TEMPLATE" "Templates to be rendered."
    :id ::template-files :multi true :default [] :update-fn conj]
   ["-c" "--context CONTEXT" "EDN context used to render template." :id ::context-file :default "-"]
   ["-d" "--context-default CONTEXT" "Default EDN context to fall back on." :id ::context-default-file]
   ["-C" "--context-override STRING" "Provide context override." :id ::context-override]
   ["-j" "--json" "Parse JSON context." :id ::json?]
   ["-a" "--add-tags TAGS_FILE" "File containing additional tag definitions." :id ::add-tags]
   [nil, "--deep-merge" "Deep-merge the context override." :id ::deep-merge?]
   ["-o" "--output OUTPUT" "Locations for output."
    :id ::outputs :multi true :default [] :update-fn conj]
   [nil "--standard" "Assume outputs to be template minus extension." :id ::standard?]])

(def cli-opts
  (concat general-opts render-opts))

;; Add the {% env %} tag to read environment variables.
(selmer.parser/add-tag!
 :env (fn [args _] (System/getenv (first args))))

(defn pp-str [x]
  (with-out-str (pp/pprint x)))

(defn exit
  ([] (System/exit 0))
  ([n] (System/exit n)))

(defn help []
  (let [general-opts-summary (->> general-opts (cli/parse-opts []) :summary)
        render-opts-summary (->> render-opts (cli/parse-opts []) :summary)]
    (println usage "\n")
    (println "General options:") (println general-opts-summary) (println)
    (println "Render options:") (println render-opts-summary) (println)))

(defn version []
  (println "Isogeny 3.0"))

(defn throw-on-missing
  "Causes an exception to be thrown when a missing value is encountered."
  []
  (when @verbose? (log/info "Throwing on missing values."))
  (s.util/set-missing-value-formatter!
   (fn [tag _]
     (-> "Missing value: "
         (str (or (:tag-value tag) (:tag-name tag)))
         Exception.
         throw))))

(defn add-tags! "Add additional tags to use when rendering the template."
  [add-tags]
  (when @verbose? (log/info "Loading tags from:" add-tags))
  (try (load-file add-tags)
       (catch Exception e
         (log/error "Exception thrown loading additional tags:")
         (throw e))))

(defn setup [{::keys [+help? +version? +verbose? +strict? +safe? +dry-run?] :as options}]
  (when +help? (help) (exit))
  (when +version? (version) (exit))
  (when +verbose? (swap! verbose? (constantly true)) (log/info "Options:" (pp-str options)))
  (when +strict? (throw-on-missing))
  (when +safe? (swap! safe? (constantly true)) (when @verbose? (log/info "Enabling safe mode.")))
  (when +dry-run? (swap! dry-run? (constantly true)) (when @verbose? (log/info "Enabling dry-run mode."))))

(defn try-slurp
  ([file] (try-slurp file nil))
  ([file {::keys [throw?]}]
   (try (when @verbose? (log/info "Reading:" file))
        (slurp file)
        (catch Exception e
          (when @verbose? (log/warn "Exception reading file:" (.getMessage e)))
          (when throw? (throw e))))))

(defn -deep-merge
  "Like merge, but merges maps recursively."
  [& maps]
  (if (every? map? maps)
    (apply merge-with -deep-merge maps)
    (last maps)))

(defn deep-merge
  "Like -deep-merge but ignores nil arguments."
  [& maps]
  (apply -deep-merge (filter some? maps)))

(defn ->context
  ([context] (->context context nil nil nil))
  ([context context-default] (->context context context-default nil nil))
  ([context context-default context-override]
   (->context context context-default context-override nil))
  ([context context-default context-override {::keys [json? deep-merge?]}]
   (let [parse-fn (if json? #(json/parse-string % true) read-string)
         merge-fn (if deep-merge? deep-merge merge)
         override (when (some? context-override) (parse-fn context-override))]
     (-> context
         (or context-default context-override
             (throw (new Exception "Neither context, nor default, nor override can be read.")))
         parse-fn
         (merge-fn override)
         eval))))

(defn ->input [input]
  (if (= "-" input)
    *in* input))

(defn ->outputs [outputs]
  (when @verbose? (log/info "Outputs:" outputs))
  (if (empty? outputs) (repeat nil)
      (map (fn [output]
             (condp = output
               "-" *out*
               nil *out*
               output))
           outputs)))

(defn ->standard [template-files]
  (map fs/strip-ext template-files))

(defn render [templates context context-default options]
  (let [{::keys [template-files context-override outputs standard?]} options
        merged-context (->context context context-default context-override options)
        fixed-outputs (if standard? (->standard template-files) (->outputs outputs))]
    (->> templates
         (map #(s.parser/render % merged-context))
         (map (fn [f c] {:file f :content c}) fixed-outputs))))

(defn write! [outputs]
  (when @verbose? (log/info "Writing:" (count outputs) "files" (map :file outputs)))
  (doall (map (fn [{::keys [file content]}]
                (when @verbose? (log/info "Writing to file:" file))
                (if @dry-run?
                  (log/warn "DRY-RUN, not writing to:" file)
                  (if (and @safe? (fs/exists? file))
                    (log/warn "Running safely so not writing to:" file)
                    (spit (or file *out*) content))))
              outputs)))

(defn render! [_ {::keys [template-files context-file context-default-file add-tags] :as options}]
  (when @verbose? (log/info "Rendering:" (pp-str options)))
  (let [templates (doall (map #(-> % ->input (try-slurp {:throw? true})) template-files))
        context (try-slurp (->input context-file) {:throw? false})
        context-default (try-slurp context-default-file {:throw? false})]
    (when add-tags (add-tags! add-tags))
    (-> templates
        (render context context-default options)
        write!)))

(defn prepare
  ([f] (prepare f true))
  ([{::keys [file content]} resolve-host?]
   (let [template (str file ".template")
         host (if-not resolve-host? "HOST"
                      (or (.. java.net.InetAddress getLocalHost getHostName) "HOST"))
         context (str/join "." [file host "edn"])]
     (when @verbose? (log/info "File:" file "\nTemplate:" template "\nContext:" context))
     [{:file template :content content}
      {:file context :content "{}"}])))

(defn prepare! [configs _]
  (when @verbose? (log/info "Preparing:" configs))
  (->> configs
       (map (fn [f] {:file f :content (try-slurp f {:throw? true})}))
       (map prepare)
       (apply concat)
       write!))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [{[subcommand & subargs :as arguments] :arguments
         :keys [options errors] :as x} (cli/parse-opts args cli-opts)]
    (when errors (log/error "Errors parsing CLI arguments:" errors) (exit 1))
    (setup options)
    (when @verbose? (log/info "Arguments:" arguments))
    (case subcommand
      "render" (render! subargs options)
      "prepare" (prepare! subargs options)
      (render! args options))))
