(ns rolfrander.puzzle.vm
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [rolfrander.puzzle.coll :refer [safe-parse-number]]))

(defn interpreter
  "General interpreter logic.
   
   Input is assumed to be a string with lines formatted as `mnemonic parameters...`, with parameters separated by space.
   The CPU-function should have the following signature: `instruction-pointer state mnemonic parameters` and return a new state. The state
   should not include instruction-pointer, that is covered by the interpreter. If the instruction is a jump, instead return
   `{:jmp relative-position}`."
  [input cpu-function start-state]
  (let [parse-line (fn [line] (map safe-parse-number (str/split line #" +")))
        program (cond (vector? input) input
                      (coll? input) (vec input)
                      :else (mapv parse-line (str/split-lines input)))
        last-ip (dec (count program))]
    (loop [state start-state
           ip 0
           cnt 0]
      (if (> ip last-ip)
        {:state state :ip ip :count cnt}
        (let [[mnemonic & params] (get program ip)] 
          (log/debug "instruction: %s %s, state: %s\n" mnemonic params state)
          (let [next-state (cpu-function ip state mnemonic params)]
            (cond (= next-state :hlt)
                  {:state state :ip ip :count cnt}

                  (contains? next-state :jmp)
                  (recur state (+ ip (:jmp next-state)) (inc cnt))

                  :else
                  (recur next-state (inc ip) (inc cnt)))))))))