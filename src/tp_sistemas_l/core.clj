(ns tp-sistemas-l.core
  (:require [clojure.math])
  (:use [tp-sistemas-l.parse])
  (:use [tp-sistemas-l.tortuga])
  (:import (java.util Locale)))

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


(defn linea-formateada
  "dada una tortuga con la posicion de inicio y otra con el fin, obtiene una cadena con el formato deseado de svg"
  [t-inicio t-fin]
  (let [s "<line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" stroke-width=\"1\" stroke=\"black\" />"
        coords-inicio (vec (vals (tortuga-coordenadas t-inicio)))
        coords-final (vec (vals (tortuga-coordenadas t-fin)))]
    (Locale/setDefault Locale/US)
    (apply #(format s %1 %2 %3 %4) (concat coords-inicio coords-final))))

(defn accion-apilar
  [pila estados]
  (assoc estados
    :tortuga (first pila)
    :checkpoint (first pila)
    :pila (pop pila)
    :escribir-archivo  true
    :pluma-arriba true))

(defn accion-pluma-arriba
  [t c estados]
  (assoc estados
    :tortuga (tortuga-avanzar t)
    :checkpoint (tortuga-avanzar t)
    :escribir-archivo (not (tortuga-comparar t c))
    :pluma-arriba true))

(defn accion-girar
  [t angulo estados]
  (assoc estados
    :tortuga (tortuga-rotar t angulo)
    :checkpoint t
    :escribir-archivo true))

(defn aplicar-accion
  "recibe la tortuga actual, el checkpoint, la pila, y las acciones
   que afectaran el estado de alguna de estas"
  [t c p v]
  (let [accion (first v)
        angulo (second v)
        estados {:tortuga t :checkpoint c :pila p :escribir-archivo false :pluma-arriba false}
        acciones {:adelante #(update % :tortuga tortuga-avanzar),
                  :pluma-arriba #((partial accion-pluma-arriba t c) %),
                  :derecha #((partial accion-girar t angulo) %),
                  :izquierda #((partial accion-girar t (- angulo)) %),
                  :invertir #((partial accion-girar t 180) %),
                  :apilar (fn [d] (update d :pila #(conj % t))),
                  :desapilar #((partial accion-apilar p) %)}]
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
    (escribir-archivo archivo-salida comandos)))
