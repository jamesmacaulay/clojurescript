(ns chrome-console
  (:require [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.reader :as reader]))

(defn compile-next-form
  "hacked from cljs.repl/evaluate-next-form"
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
              _ (when *debug* (println "EMITTED:" (pr-str res)))]
          {:js res})))
    (catch js/Error e
      {:error e :line-number (reader/get-line-number rdr)})))

(defn compile-code
  "hacked from cljs.repl/evaluate-code"
  [text]
  (let [rdr (reader/indexing-push-back-reader text)]
    (loop [emitted-js ""]
      (let [output (compile-next-form rdr)
            emitted-js (str emitted-js "\n" (:js output))]
        (if-not (:finished output)
          (if-let [err (:error output)]
            (throw err)
            (recur emitted-js))
          emitted-js)))))
