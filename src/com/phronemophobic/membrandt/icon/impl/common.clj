(ns com.phronemophobic.membrandt.icon.impl.common
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.xml :as xml]
            [clojure.zip :as z]
            [membrane.skia :as skia]
            [com.phronemophobic.membrandt.icon :as icon])
  (:import java.io.ByteArrayInputStream))

(def icon-types
  #{"filled"
    "outlined"
    "twotone"})

(defn list-icons
  ([]
   (into {}
         (map (fn [icon-type]
                [icon-type (list-icons icon-type)]))
         icon-types))
  ([type]
   (->> (io/file
         (io/file "../membrandt/resources/com/phronemophobic/membrandt/ant-design-icons/svg/")
         type)
        .listFiles
        (map #(.getName %))
        (clojure.core/filter
         (fn [s]
           (str/ends-with? s ".svg" )))
        (map #(subs % 0 (- (count %) 4))))))

(defn string->input-stream [s]
  (ByteArrayInputStream. (.getBytes s "utf-8")))


(defn zxml [xml]
  (z/zipper :content :content (fn [node children]
                                (assoc node :content children))
            xml))


(defn color->css-color [color]
  (assert (= 3 (count color)))
  (apply format "#%02X%02X%02X"
         (eduction
          (map (fn [d]
                 (Math/round (* 255.0 d))))
          color)))

(defn use-colors [svg-str primary secondary]
  (let [svg (xml/parse (string->input-stream svg-str))
        svg (-> svg
                (assoc-in [:attrs :fill] "currentColor")
                (assoc-in [:attrs :color] primary))

        ;; change all path's fills
        ;;     '#333' -> primary
        ;;    '#E6E6E6' -> secondary
        ;; https://github.com/ant-design/ant-design-icons/blob/d7d0627628cdf47fa79e5c7b878aa205a977681d/packages/icons-angular/src/utils.ts#L76
        svg (loop [z (zxml svg)]
              (if (z/end? z)
                (z/root z)
                (let [node (z/node z)
                      z (if (and (= :path (:tag node))
                                 (#{"#333" "#E6E6E6" "#D9D9D9" "#D8D8D8"}
                                  (-> node :attrs :fill)))
                          (z/edit z #(update-in % [:attrs :fill]
                                                {"#333" primary
                                                 "#E6E6E6" secondary
                                                 "#D9D9D9" secondary
                                                 "#D8D8D8" secondary}))
                          ;; else
                          z)]
                  (recur (z/next z)))))]
    (with-out-str
      (xml/emit svg))))

(defmacro deficon [icon-name type {:keys [primary-color
                                          secondary-color]}]
  (let [default-size [32 32]]
    `(let [~'s* (delay
                  (use-colors
                   (icon/svg-str ~icon-name ~type)
                   ~primary-color
                   ~secondary-color))]
       (defn ~(symbol icon-name)
         ([]
          (skia/svg @~'s* ~default-size))
         ([~'{:keys [size]}]
          (skia/svg @~'s*
                    (or ~'size
                        ~default-size)))))))

(defmacro deficons [type {:keys [primary-color
                                 secondary-color]
                          :as opts}]
  `(do
     ~@(eduction
        (map (fn [icon-name]
               `(deficon ~icon-name ~type ~opts)))
        (list-icons type))))




(comment
  (def test-svg
    (->> svg-strs
         (filter #(and (str/includes? % "#333")
                       (str/includes? % "#E6E6E6")))
         first
         ))

  ;; outlined and filled
  ;; set fill:currentColor; color: red;
  ;; two tone
  ;; set fill:currentColor; color: red;. also modify all paths to change fills
  ;;     '#333' -> primary
;;;    '#E6E6E6' -> secondary

  (def svg-strs
    (into []
          (comp
           (map (fn [type]
                  (io/file
                   (io/file "../membrandt/resources/com/phronemophobic/membrandt/ant-design-icons/svg/")
                   type)))
           (mapcat #(.listFiles %))
           (map slurp))

          icon-types))

  (-> svg-strs
      first
      string->input-stream
      xml/parse
      xml/emit)
  ,)



