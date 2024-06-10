(ns tp-sistemas-l.core
  (:require [clojure.string :as str])
  (:require [clojure.math])
  (:import (java.util Locale)))

(defn vec2d [x y] {:x x :y y})

(defn vec2d-x [v] (:x v))

(defn vec2d-y [v] (:y v))

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
  (let [angulo-actual (clojure.math/to-radians (tortuga-angulo t))
        posicion-actual (tortuga-coordenadas t)
        seno-angulo (clojure.math/sin angulo-actual)
        coseno-angulo (clojure.math/cos angulo-actual)
        nuevo-x (+ (* 5 coseno-angulo) (vec2d-x posicion-actual))
        nuevo-y (+ (* 5 seno-angulo) (vec2d-y posicion-actual))
        nueva-posicion (vec2d nuevo-x nuevo-y)]
    (tortuga nueva-posicion  (tortuga-angulo t))))


(defn tortuga-rotar
  "gira la tortuga la cantidad de grados indicados"
  [t giro]
  (tortuga (tortuga-coordenadas t) (+ giro (tortuga-angulo t))))

(defn tortuga-comparar
  "recibe dos tortugas y devuelve true si ambas tienen la misma posicion"
  [a b]
  (let [coords-a (tortuga-coordenadas a)
        coords-b (tortuga-coordenadas b)]
    (= coords-a coords-b)))

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
  (filter (comp not nil?)
          (map #(case %
                     \F [:adelante]
                     \G [:adelante]
                     \f [:pluma-arriba]
                     \g [:pluma-arriba]
                     \+ [:derecha angulo]
                     \- [:izquierda angulo]
                     \| [:invertir]
                     \[ [:apilar]
                     \] [:desapilar]
                     nil) cadena)) )


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


(defn linea-formateada
  "dada una tortuga con la posicion de inicio y otra con el fin, obtiene una cadena con el formato deseado de svg"
  [t-inicio t-fin]
  (let [s "<line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" stroke-width=\"1\" stroke=\"black\" />"
        coords-inicio (vec (vals (tortuga-coordenadas t-inicio)))
        coords-final (vec (vals (tortuga-coordenadas t-fin)))]
    (Locale/setDefault Locale/US)
    (apply #(format s %1 %2 %3 %4) (concat coords-inicio coords-final))))


(defn aplicar-accion
  "recibe la tortuga actual, el checkpoint, la pila, y las acciones
   que afectaran el estado de alguna de estas"
  [t c p v]
  (let [accion (first v)
        angulo (second v)
        estados {:tortuga t :checkpoint c :pila p :escribir-archivo false :pluma-arriba false}
        girar (fn [a d] (assoc d :tortuga (tortuga-rotar t a) :checkpoint t :escribir-archivo true))
        acciones {:adelante #(update % :tortuga tortuga-avanzar),
                  :pluma-arriba #(assoc %
                                   :tortuga (tortuga-avanzar t)
                                   :checkpoint (tortuga-avanzar t)
                                   :escribir-archivo (not (tortuga-comparar t c))
                                   :pluma-arriba true),
                  :derecha #((partial girar angulo) %),
                  :izquierda #((partial girar (- angulo)) %),
                  :invertir #((partial girar 180) %),
                  :apilar (fn [d] (update d :pila #(conj % t))),
                  :desapilar #(assoc %
                                :tortuga (first p)
                                :checkpoint (first p)
                                :pila (pop p)
                                :escribir-archivo  true)}]
    ((accion acciones) estados)))


(defn ejecutar-instrucciones
  "aplica las acciones de la secuencia de instrucciones
   tiene como precondicion que el archivo de salida este abierto"
  [wrtr instrucciones]
  (loop [tortuga-actual (tortuga (vec2d 0.0 0.0) 90)
         tortuga-checkpoint tortuga-actual                  ;llamo checkpoint a la ultima posicion que se escribio en el archivo
         pila-tortugas (list)
         secuencia instrucciones]
    (if ((comp not empty?) secuencia)
      (let [siguiente-instruccion (first secuencia)
            estados (aplicar-accion tortuga-actual tortuga-checkpoint pila-tortugas siguiente-instruccion)
            linea (linea-formateada
                    tortuga-checkpoint
                    (if (:pluma-arriba estados) tortuga-actual (:tortuga estados)))]
        (if (:escribir-archivo estados) (.write wrtr linea))
        (if (empty? (rest secuencia))                         ;ultima iteracion, debo escribir una linea
          (.write wrtr (linea-formateada (:checkpoint estados) (:tortuga estados))))
        (recur (:tortuga estados) (:checkpoint estados) (:pila estados) (rest secuencia))))))


(defn escribir-archivo
  "escribe un archivo con formato svg, siguiendo las instrucciones recibidas"
  [nombre-archivo instrucciones]
  (with-open [wrtr (clojure.java.io/writer nombre-archivo)]
    (.write wrtr "<svg viewBox=\"-50 -150 500 700\" xmlns=\"http://www.w3.org/2000/svg\">")
    (ejecutar-instrucciones wrtr instrucciones)
    (.write wrtr "</svg>")))


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
        comandos (generar-comandos (expandir (:axioma sistema) (:reglas sistema) iteraciones) (:angulo sistema))]
    (println comandos)
    (escribir-archivo archivo-salida comandos)))
