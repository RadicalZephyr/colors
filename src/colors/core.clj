(ns colors.core)

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))

(defn color->list
  ([hex-number] (color->list hex-number ()))
  ([hex-number result]
     (let [hex-sq (* 16 16)]
       (cond
        (= 0 hex-number) result
        :else (recur (quot hex-number hex-sq)
                     (conj result (rem hex-number hex-sq)))))))

(defn list->color [[r g b]]
  (format "#%02x%02x%02x" r g b))

(defn color-difference [a b]
  (map #(max 0 (- %1 %2)) a b))

(defn make-color-relation
  ([base derived]
     (let [base-color (color->list base)
           derived-colors (map color->list derived)]
       (make-color-relation (map #(color-difference base-color %1)
                                 derived-colors))))
  ([difference]
     (fn [base]
       (map #(color-difference (color->list base) %1) difference))))

(defn print-color-list [colors]
  (map list->color colors))