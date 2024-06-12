(ns tp-sistemas-l.salida
  (:require [tp-sistemas-l.tortuga :refer :all])
  (:import (java.util Locale)))

(defn linea-formateada
  "dada una tortuga con la posicion de inicio y otra con el fin, obtiene una cadena con el formato deseado de svg"
  [t-inicio t-fin]
  (let [s "<line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" stroke-width=\"1\" stroke=\"black\" />"
        coords-inicio (vec (vals (tortuga-coordenadas t-inicio)))
        coords-final (vec (vals (tortuga-coordenadas t-fin)))]
    (Locale/setDefault Locale/US)
    (apply #(format s %1 %2 %3 %4) (concat coords-inicio coords-final))))

(defn formato-medidas
  [medidas]
  (let [x-min (medidas 0)
        y-min (medidas 1)
        ancho (- (medidas 2) x-min)
        altura (- (medidas 3) y-min)
        s "<svg viewBox=\"%f %f %f %f\" xmlns=\"http://www.w3.org/2000/svg\">"]
    (Locale/setDefault Locale/US)
    (apply #(format s %1 %2 %3 %4) [x-min y-min ancho altura])))

(defn escribir-archivo
  "escribe un archivo con formato svg, siguiendo las instrucciones recibidas"
  [nombre-archivo lineas medidas]
  (with-open [wrtr (clojure.java.io/writer nombre-archivo)]
    (.write wrtr (str (formato-medidas medidas) lineas "</svg>"))))
