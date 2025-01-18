(ns rolfrander.puzzle.config
  (:require [clojure.tools.logging :as log]
            [clojure.java.io :as io]))

(defn get-config
  "Reads /config. 
   
   Example config-file:
   
   ```
   {
     :session \"session-cookie\"
   }```"
  []
  (with-open [c (java.io.PushbackReader.
                 (io/reader "config"))]
    (read c)))

(defn get-data
  "Download and cache puzzle input from advent of code. 
   Needs a session cookie stored in `/config` under key `:session`"
  [year day]
  (let [url (format "https://adventofcode.com/%d/day/%d/input" year day)
        local-file (format "resources/advent%d/day%02d.txt" year day)]
    (if (.exists (io/file local-file))
      (do (log/debug "loading input from" local-file)
          (slurp local-file))
      (let [response (http/get url {:cookies {"session" {:value (:session (get-config))}}})]
        (log/debug "loading input from http")
        (if (not= (:status response) 200)
          (throw (RuntimeException. (str "error getting data: " (:reason-phrase response))))
          (let [body (:body response)]
            (with-open [w (io/writer local-file)]
              (.write w body))
            body))))))
