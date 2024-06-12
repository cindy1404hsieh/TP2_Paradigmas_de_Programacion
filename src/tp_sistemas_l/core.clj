(ns tp-sistemas-l.core
  (:require
    [clojure.math]
    [tp-sistemas-l.parse :refer :all]
    [tp-sistemas-l.reglas :refer :all]
    [tp-sistemas-l.salida :refer :all]
    [tp-sistemas-l.tortuga :refer :all]))

(defn obtener-medidas
  "recibe un vector con las medidas actuales, y lo actualiza en base a las coordenadas de la tortuga actual"
  [v t]
  (let [x-tortuga (vec2d-x (tortuga-coordenadas t))
        y-tortuga (vec2d-y (tortuga-coordenadas t))
        x-min (min (v 0) x-tortuga)
        y-min (min (v 1) y-tortuga)
        x-max (max (v 2) x-tortuga)
        y-max (max (v 3) y-tortuga)]
    [x-min y-min x-max y-max]))

(defn ejecutar-instrucciones
  "aplica las acciones de la secuencia de instrucciones"
  [nombre-archivo instrucciones]
  (loop [t-actual (tortuga (vec2d 0.0 0.0) 270)
         t-checkpoint t-actual
         pila-tortugas (list)
         secuencia instrucciones
         lineas ""
         medidas [0.0 0.0 0.0 0.0]]
    (when-not (empty? secuencia)
      (let [siguiente-instruccion (first secuencia)
            estados (aplicar-accion t-actual t-checkpoint pila-tortugas siguiente-instruccion)
            t-siguiente (:tortuga estados)
            tc-siguiente (:checkpoint estados)
            linea (linea-formateada t-checkpoint (if (:pluma-arriba estados) t-actual t-siguiente))
            sig-linea (if (:escribir-archivo estados) (str lineas linea) lineas)]
        (when (empty? (rest secuencia))
          (escribir-archivo nombre-archivo (str sig-linea (linea-formateada tc-siguiente t-siguiente)) (obtener-medidas medidas t-siguiente)))
        (recur
          t-siguiente
          tc-siguiente
          (:pila estados)
          (rest secuencia)
          sig-linea
          (obtener-medidas medidas tc-siguiente))))))

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
    (ejecutar-instrucciones archivo-salida comandos)))
