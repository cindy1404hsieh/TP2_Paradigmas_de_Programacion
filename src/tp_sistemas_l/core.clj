(ns tp-sistemas-l.core
  (:require [clojure.java.io :as io]))

(defn -main
  [& args]
  (let [nombre-archivo (first args)]
    (with-open [archivo (io/reader nombre-archivo)]
      (doseq [linea (line-seq archivo)]
        (println linea)))))
