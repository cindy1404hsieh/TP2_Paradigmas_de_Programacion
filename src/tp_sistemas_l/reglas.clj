(ns tp-sistemas-l.reglas)

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
                  nil) cadena)))
