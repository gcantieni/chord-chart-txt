(ns chord-chart-txt.core
  (:import (java.io BufferedReader FileReader))
  (:use hiccup.core)
  (:gen-class))

(def style [:style
  "
  table {
    width:100%;
    padding-left:60px;
    padding-right:60px;
  }
  th {
    border: 1px solid black;
    font-size: 30px;
    padding-left:60px;
    padding-right:60px;
  }
  html *
  {
   font-family: Arial !important;
  }
  h1 {
    text-align: center;
  }
  td {
    padding-top: 10px;
    font-size: 23px;
    padding-bottom:60px;
  }

  div {
    text-align: justify;
    height: 30px;
  }
  span{
  width:100%;
  height:0px;
  display:inline-block;
  }"])

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

(defn add-header [content]
  "Add default header to content"
  [:html
   [:body
    [:h2 "River"]
    [:style "th { border: 1px solid black; }"]
     content]])

(defn -main
  [& args]
  (cond
    (= (first args) "-h") (println "input a path to a plaintext file with alternating verses and chords")
    (not= (count args) 2) (println "usage: input-file output-file")
    :else (let [in-file (first args)
                out-file (second args)]
             (spit out-file
                   (html [:html
                          [:body
                           (make-title in-file)
                           style
                           (make-table in-file)]])))))

