(ns com.phronemophobic.membrandt.icon
  (:require [membrane.skia :as skia]
            [membrane.ui :as ui]
            [membrane.component :refer [defui]]
            [membrane.basic-components :as basic]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))
(defn svg-str [name type]
  (let [resource (io/resource
                  (str "com/phronemophobic/membrandt/ant-design-icons/svg/"
                       type "/"
                       name ".svg"))]
    (slurp resource)))


(comment

  (require '[com.phronemophobic.membrandt.icon.impl.common :as common])

  (defn view-icons []
    (let [sections
          (into []
                (map (fn [type]
                       (let [icons (into []
                                         (comp
                                          (map (fn [icon-name]
                                                 (let [sym (symbol
                                                            (str "com.phronemophobic.membrandt.icon." type)
                                                            icon-name)
                                                       v (requiring-resolve sym)
                                                       elem (v {:size [20 20]})]
                                                   (ui/on
                                                    :mouse-down
                                                    (fn [_]
                                                      (println icon-name)
                                                      nil)
                                                    elem))))
                                          
                                          ;;(map #(assoc % :container-size [20 20]))
                                          )
                                         (common/list-icons type))
                             icon-table (ui/table-layout
                                         (eduction
                                          (partition-all 15)
                                          icons))]
                        (ui/vertical-layout
                         (ui/label type)
                         icon-table))))
                common/icon-types)]
      (apply
       ui/horizontal-layout
       sections)))

  (skia/run #'view-icons)

  (svg-str "dashboard" "outlined")

  com.phronemophobic.membrandt.icon.twotone

  

  ,)



