(ns tp-sistemas-l.core
  (:require [clojure.java.io :as io]))

(defn parsear-sistema-L
  "dado un conjunto de lineas de texto,
  parsea y devuelve un mapa que representa un sistema L
  con angulo, axioma y reglas."
  [lineas]
  (let [angulo (Double/parseDouble (first lineas))
        axioma (second lineas)
        reglas (map #(clojure.string/split % #" ") (drop 2 lineas))]
    {:angulo angulo :axioma axioma :reglas (into {} reglas)}))

(defn leer-archivo
  "lee todas las líneas de un archivo y devuelve como una secuencia"
  [nombre-archivo]
  (with-open [reader (clojure.java.io/reader nombre-archivo)]
    (doall (line-seq reader))))
(defn -main
  "toma argumentos desde la linea de comandos: archivo de entrada, numero de iteraciones y archivo de salida
  lee el archivo de entrada y parsea el sistema L
  evoluciona el axioma y genera comandos."
  [& args]
  (let [archivo-entrada (first args)
        iteraciones (Integer/parseInt (second args))
        archivo-salida (nth args 2)
        lineas (leer-archivo archivo-entrada)
        sistema (parsear-sistema-L lineas)]
    (println lineas)
    (println sistema)))
