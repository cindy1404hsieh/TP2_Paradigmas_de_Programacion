(ns tp-sistemas-l.core
  (:require [clojure.string :as str]))

(defn vec2d [x y] {:x x :y y})

(defn vec2d-x [v] (:x v))

(defn vec2d-y [v] {:y v})

(defn tortuga
  "crea una nueva tortuga con sus coordenadas y angulo iniciales"
  [coordenadas angulo]
  {:coordenadas coordenadas :angulo angulo})

(defn tortuga-angulo [t] (:angulo t))

(defn tortuga-coordenadas [t] (:coordenadas t))


(defn tortuga-avanzar
  "hace que la tortuga que recibe avanze una unidad, actualizando sus coordenadas
   segun el angulo actual de la tortuga"
  [t]
  (let [
        angulo-actual (clojure.math/to-radians (tortuga-angulo t))
        posicion-actual (tortuga-coordenadas t)
        seno-angulo ((clojure.math/sin angulo-actual))
        coseno-angulo ((clojure.math/cos angulo-actual))
        nuevo-x (+ coseno-angulo (vec2d-x posicion-actual))
        nuevo-y (+ seno-angulo (vec2d-y posicion-actual))
        nueva-posicion (vec2d nuevo-x nuevo-y)]
    tortuga nueva-posicion  (tortuga-angulo t)))


(defn tortuga-rotar
  "gira la tortuga la cantidad de grados indicados"
  [t giro]
  (tortuga (tortuga-coordenadas t) (+ giro (tortuga-angulo t))))

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


(defn escribir-archivo
  "escribe un archivo con formato svg, siguiendo las instrucciones recibidas"
  [nombre-archivo instrucciones]
  (with-open [wrtr (clojure.java.io/writer nombre-archivo :append true)]
    (.write wrtr "<svg viewBox=\"0 0 300 200\" xmlns=\"http://www.w3.org/2000/svg\">")
    {:adelante 1}
    (.write wrtr "</svg>")
    ))


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
    (escribir-archivo archivo-salida comandos)
    (println comandos)))
