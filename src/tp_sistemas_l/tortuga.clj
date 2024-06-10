(ns tp-sistemas-l.tortuga)

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
        nuevo-x (+ (* 10 coseno-angulo) (vec2d-x posicion-actual))
        nuevo-y (+ (* 10 seno-angulo) (vec2d-y posicion-actual))
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
