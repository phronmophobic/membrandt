(ns com.phronemophobic.membrandt.icon.ui
  (:require [membrane.skia :as skia]
            [membrane.ui :as ui]
            [membrane.component :refer [defui]]
            [membrane.basic-components :as basic]
            [com.phronemophobic.membrandt.icon :as icon]
            [com.phronemophobic.membrandt.icon.impl.common :as icon-common]))

(def icon-size [18 18])
(defui icon [{:keys [name size hover? on-click]}]
  (let [primary-color (if hover?
                        "#1677ff"
                        "#555555")
        secondary-color (if hover?
                          "#1677ff"
                          "#555555")
        elem (basic/on-hover
              {:hover? hover?
               :$body nil
               :body
               (skia/svg
                (icon-common/use-colors
                 (icon/svg-str name "outlined")
                 primary-color
                 secondary-color)
                (or size icon-size))})
        elem (if on-click
               (ui/on
                :mouse-down
                (fn [_]
                  (on-click))
                elem)
               elem)]
    elem))
