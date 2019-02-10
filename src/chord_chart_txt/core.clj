
(ns chord-chart-txt.core
  (:import (java.io BufferedReader FileReader))
  (:use hiccup.core)
  (:gen-class))


(defn inner-css-reducer [acc item]
     (if (= (last acc) \space)
       (str acc item \;"\n")
       (str acc (name item) \:\space)))

(defn add-css-separations [key-val-map]
  "[:width '100px'] --> '{width:100px;}"
  (reduce inner-css-reducer "" key-val-map))

(defn style 
  "(syle :table [:width '100px' :height '200px'])
    --> 'table {width: 100px; height:200px;}' "
  [style-table]
  (reduce (fn [final-blocks block]
            (if (coll? block)
              (str final-blocks \space\{ (add-css-separations block) \}"\n")
              (str final-blocks (name block))))
          ""
          style-table))

; this is harder to represent because of the \* in the name
(def special-style "html * {font-family: Arial !important;}")

(def constant-style
  (str (style [
        :html* [:font-family "Arial !important"]
        :h1 [:text-align "center"]
        :span [:width "100%"
               :height "0px"
               :display "inline-block"]])
       special-style))

(def large-style
  (style
   [:table [:width "100%"
            :padding-left "60px"
            :padding-right "60px"]
    :th [:border "1px solid black"
         :font-size "25px"
         :padding-left "60px"
         :padding-right "60px"]
    :div [:text-align "justify"
          :height "30px"]
    :td [:padding-top "10px"
         :font-size "20px"
         :padding-bottom "60px"]]))

(def small-style
  (style
   [:table [:width "100%"
            :padding-left "10px"
            :padding-right "10px"]
    :th [:border "1px solid black"
         :font-size "16px"
         :padding-left "10px"
         :padding-right "10px"]
    :div [:text-align "justify"
          :height "18px"]
    :td [:padding-top "10px"
         :font-size "14px"
         :padding-bottom "40px"]]))

(defn table [table-innards]
  [:table table-innards])

(defn into-rows [tag cells]
  "Put cells into table rows with either :td or :th tag"
  (reduce #(into %1 [[tag %2]]) [:tr] cells))

(defn not-in-table? [line]
  (or (= (first line) \#) (= (count line) 0)))

(defn lyric? [line]
  (= (first line) \"))

(defn trim-consec-whitespace [line]
  (clojure.string/replace (clojure.string/trim line) #"\s{2,}" " "))

(defn split-lyrics [line]
  "Split on measure line '|'
   e.g. 'winter | wind' --> ('winter' 'wind')"
  (clojure.string/split line #"\|"))

(defn split-chords [line]
  "Separate by space and then replaces '_' with ' '
   e.g. 'A B_C' --> ('A' 'B C')"
  (map #(clojure.string/replace % "_" " ") (clojure.string/split line #" ")))

(defn add-chord-tags [chords]
  "Adds formatting tags to each chord, putting them in a div
   and adding an empty span
   e.g. ('A' 'B C') --> [:tr [:th [:div [:span] 'A']] [:th [:div [:span] 'B C']]]"
  (reduce #(into %1 [[:th [:div %2  [:span]]]]) [:tr] chords))

(defn parse-table-line [acc line]
  "Puts lyrics into table data (td) elements and chords into table header (th) elements"
  (let [line (trim-consec-whitespace line)]
    (cond
      (lyric? line) (into acc [(into-rows :td (split-lyrics line))])
      (not-in-table? line) acc
      :else (into acc  [(add-chord-tags (split-chords line))])))) ; Assume it's chords

(defn process-file [file-name reducer initial-value]
  (with-open [rdr (BufferedReader. (FileReader. file-name))]
    (reduce reducer initial-value (line-seq rdr))))

(defn title-reducer [acc line]
  (if (= (first line) \#)
    (reduced (do
               (into acc [(remove #{\#} line)])))
    acc))

(defn make-title [file-name]
  (process-file file-name title-reducer [:h1]))

(defn make-table [file-name]
  (process-file file-name parse-table-line [:table]))

(defn get-style! [flags]
  (if (= (first flags) "--small")
    [:style (str constant-style small-style)]
    [:style (str constant-style large-style)]))

(defn flag? [possible-flag]
  "flags start with the following characters: --"
  (and (= (first possible-flag) \-) (= (second possible-flag) \-)))

(defn convert-chart! [in-file out-file & flags]
    (spit out-file
        (html [:html
                [:body
                 (make-title in-file)
                 [:style (get-style! flags)]
                 (make-table in-file)]])))

(defn -main
  [& args]
  (let [files (filter (complement flag?) args)
        flags (filter flag? args)]
    (cond
        (= (first args) "--help") (println "input a path to a plaintext file with alternating verses and chords\nIf it's a song with long verses, use the --small option")
        (not= (count files) 2) (println "usage: input-file output-file")
        :else
        (convert-chart! (first files) (second files) (first flags)))))

