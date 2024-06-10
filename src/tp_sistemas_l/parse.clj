(ns tp-sistemas-l.parse
  (:require [clojure.string :as str]))

(defn parsear-sistema-L
  "dado un conjunto de lineas de texto,
  parsea y devuelve un mapa que representa un sistema L
  con angulo, axioma y reglas."
  [lineas]
  (let [angulo (Double/parseDouble (first lineas))
        axioma (second lineas)
        reglas (into {} (map (fn [linea]
                               (let [[pred suc] (str/split linea #" ")]
                                 [(first pred) suc]))
                             (drop 2 lineas)))]
    {:angulo angulo :axioma axioma :reglas reglas}))


(defn leer-archivo
  "lee todas las líneas de un archivo y devuelve como una secuencia"
  [nombre-archivo]
  (with-open [reader (clojure.java.io/reader nombre-archivo)]
    (doall (line-seq reader))))