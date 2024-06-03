(ns tp-sistemas-l.core
  (:require [clojure.string :as str]))
(defn aplicar-reglas
  "dada una cadena y un mapa de reglas,
  aplica las reglas a cada caracter de la cadena
  y devuelve la cadena resultante"
  [cadena reglas]
  (apply str (map #(get reglas % (str %)) cadena)))
(defn expandir
  "expande un axioma segun un conjunto de reglas
  para un numero dado de iteraciones"
  [axioma reglas iteraciones]
  (loop [n iteraciones
         resultado axioma]
    (if (zero? n)
      resultado
      (recur (dec n) (aplicar-reglas resultado reglas)))))
(defn generar-comandos
  "convierte una cadena de caracteres en una secuencia de comandos especificos
  segun las reglas de sistema L."
  [cadena angulo]
  (map #(case %
          \F [:adelante 1]
          \G [:adelante 1]
          \f [:pluma-arriba]
          \g [:pluma-abajo]
          \+ [:derecha angulo]
          \- [:izquierda angulo]
          \| [:invertir]
          \[ [:apilar]
          \] [:desapilar]
          nil) cadena))
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
(defn -main
  "toma argumentos desde la linea de comandos: archivo de entrada, numero de iteraciones y archivo de salida
  lee el archivo de entrada y parsea el sistema L
  evoluciona el axioma y genera comandos."
  [& args]
  (let [archivo-entrada (first args)
        iteraciones (Integer/parseInt (second args))
        archivo-salida (nth args 2)
        lineas (leer-archivo archivo-entrada)
        sistema (parsear-sistema-L lineas)
        comandos (generar-comandos (expandir (:axioma sistema) (:reglas sistema) iteraciones) (:angulo sistema))
        ]
    (println comandos)))
