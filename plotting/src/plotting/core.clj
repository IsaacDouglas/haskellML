(ns plotting.core
  (require [clojure.string :as str])
  (require [com.hypirion.clj-xchart :as c])
  (:gen-class))

(def data-url (atom "src/data/out.data"))

(def file-lines (slurp @data-url))

(defn process-content
  [str xPos yPos]
  (map
   #(hash-map :Sepal-Length
              (Double/parseDouble (nth (str/split % #",") xPos))
              :Sepal-Width
              (Double/parseDouble (nth (str/split % #",") yPos))
              :Class
              (nth (str/split % #",") 4))

   (str/split str #"\n")))
(def data
  (process-content file-lines 0 1))

(defn gen-chart
  [data]
  (c/xy-chart
   {"Iris-setosa" (c/extract-series
                   {:x :Sepal-Length
                    :y :Sepal-Width}
                   (filter #(= (:Class %) "Iris-setosa") data))
    "Iris-versicolor" (c/extract-series
                       {:x :Sepal-Length
                        :y :Sepal-Width}
                       (filter #(= (:Class %) "Iris-versicolor") data))
    "Iris-virginica" (c/extract-series
                      {:x :Sepal-Length
                       :y :Sepal-Width}
                      (filter #(= (:Class %) "Iris-virginica") data))}
   {:title "Iris dataset"
    :render-style :scatter
    :x-axis {:title "Sepal Length"}
    :y-axis {:title "Sepal Width"}}))

(defn show-chart
  []
  (c/view (gen-chart data)))

(defn export-png 
  [name]
  (c/spit (gen-chart data) (str "src/charts/" name ".png")))

(defn -main
  [& args]
  (do 
    (println "Loading .data...")
    (swap! data-url (fn [_] (str "src/data/" (nth args 0))))
    (println "Loading " @data-url)
    (export-png (nth args 1))))