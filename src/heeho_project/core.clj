(ns heeho-project.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn re-write [x]
  (let [y (str (:name (meta x)))]
       (-> y
           (str/replace #"~" "tilde")
           (str/replace #"`" "grave")
           (str/replace #"!" "exclamation")
           (str/replace #"@" "at")
           (str/replace #"\$" "dollar")
           (str/replace #"%" "percent")
           (str/replace #"\^" "caret")
           (str/replace #"&" "and")
           (str/replace #"\*" "asterisk")
           (str/replace #"\(" "parentheses")
           (str/replace #"\)" "parentheses")
           (str/replace #"-" "dash")
           (str/replace #"_" "underscore")
           (str/replace #"\+" "plus")
           (str/replace #"\=" "equals")
           (str/replace #"\[" "bracket")
           (str/replace #"\]" "bracket")
           (str/replace #"\{" "bracket")
           (str/replace #"\}" "bracket")
           (str/replace #"\|" "verticalbar")
           (str/replace #"\\" "backslash")
           (str/replace #":" "colon")
           (str/replace #";" "semicolon")
           (str/replace #"\"" "doublequote")
           (str/replace #"\'" "singlequote")
           (str/replace #"\<" "lessthan")
           (str/replace #"\>" "greaterthan")
           (str/replace #"\," "comma")
           (str/replace #"\." "period")
           (str/replace #"\?" "question")
           (str/replace #"\/" "slash"))))

(defn remove-consonants-count [x]
  (-> x
      (str/replace #"q" "")
      (str/replace #"w" "")
      (str/replace #"r" "")
      (str/replace #"t" "")
      (str/replace #"p" "")
      (str/replace #"s" "")
      (str/replace #"d" "")
      (str/replace #"f" "")
      (str/replace #"g" "")
      (str/replace #"h" "")
      (str/replace #"j" "")
      (str/replace #"k" "")
      (str/replace #"l" "")
      (str/replace #"z" "")
      (str/replace #"x" "")
      (str/replace #"c" "")
      (str/replace #"v" "")
      (str/replace #"b" "")
      (str/replace #"n" "")
      (str/replace #"m" "")
      (str/replace #"y" "e")
      (str/replace #"u" "e")
      (str/replace #"i" "e")
      (str/replace #"o" "e")
      (str/replace #"a" "e")))

(defn return-hee-ho [result x]
  (loop [finalheeho x]
     (cond
       (not (some #(= finalheeho %) result)) finalheeho
       :else (recur (str finalheeho "o")))))

(defn hee-ho-time [result x]
  (return-hee-ho result (str "h" x "ho")))

(defn read-eval-all [x]
  (loop [rem-args x]
    (cond
      (empty? rem-args) x
      :else (recur (do (eval (read-string (first rem-args))) (rest rem-args))))))

(defn force-args [x]
    (loop [rem-arglist (:arglists (meta x))
           resultstr ""]
      (cond
        (empty? rem-arglist) resultstr
        :else (recur (rest rem-arglist) (str resultstr "(" (first rem-arglist) "(" (str/replace (str/replace (str x) #"\#" "") #"\'" "") " " (clojure.string/replace (clojure.string/replace (clojure.string/replace (str (first rem-arglist)) #"\[" "") #"\]" "") #"\&" "") "))")))))

(defn create-hee-ho-defn [x y]
   (loop [rem-args x
          rem-vals y
          result []]
     (cond
       (empty? rem-args) result
       (nil? (:arglists (meta (first rem-vals)))) (recur rem-args (rest rem-vals) result)
       (or ((meta (first rem-vals)) :macro) ((meta (first rem-vals)) :special-form)) (recur rem-args (rest rem-vals) result)
       :else (recur (rest rem-args) (rest rem-vals) (conj result (str "(defn " (first rem-args) " ";[& args] (" (str/replace (str/replace (str (first rem-vals)) #"\#" "") #"\'" "") " args))"))))))
                (force-args (first rem-vals)) ")"))))))

(defn conversion [x]
  (do (println "Would you really like to define every function in this library as he hoo? (Y/N)")
    (if (= (read-line) "Y")
      (loop [rem-args x
            result []]
        (cond
          (empty? rem-args) (read-eval-all (create-hee-ho-defn result x))
          (nil? (:arglists (meta (first rem-args)))) (recur (rest rem-args) result)
          (or ((meta (first rem-args)) :macro) ((meta (first rem-args)) :special-form)) (recur (rest rem-args) result)
          :else (recur (rest rem-args) (conj result (hee-ho-time result (remove-consonants-count (re-write (first rem-args))))))))
       (println "hee ho has not been generated"))))
