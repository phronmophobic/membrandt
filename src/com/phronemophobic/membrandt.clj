(ns com.phronemophobic.membrandt
  (:require [clojure.spec.alpha :as s]
            [clojure.math.combinatorics :as combo]
            [membrane.skia.paragraph :as para]
            [membrane.skia :as skia]
            [membrane.ui :as ui]
            [membrane.basic-components :as basic]
            [clojure.core.match :refer [match]]
            [membrane.component
             :refer [defui defeffect]]))


;; https://ant.design/components/overview

;; # general
;; button


(s/def :button/type #{:primary
                      :default
                      :dashed
                      :text
                      :link})
(s/def ::icon nil?)
(s/def :button/icon ::icon)
(s/def :button/size #{:large
                      :default
                      :middle
                      :small})
(s/def :button/disabled? boolean?)
(s/def :button/loading? boolean?)
(s/def :button/danger? boolean?)
;; Option to fit button width to its parent width
(s/def :button/block? boolean?)
(s/def :button/text string?)
(s/def :button/hover? boolean?)
(s/def ::button
  (s/keys :opt-un
          [:button/type
           ;; :button/icon
           :button/hover?
           :button/size
           ;; :button/disabled?
           ;; :button/loading?
           :button/danger?
           ;; :button/block?
           :button/text]))



(comment
  (require '[clojure.spec.gen.alpha :as gen])

  (gen/sample (s/gen ::button))
  ,)


(def button-primary-paragraph-style
  #:paragraph-style
  {:text-style
   #:text-style {;; :font-families ["SF Pro"]
                 :font-size 14
                 :height 1.5714285714285714
                 :height-override true
                 :color [0 0 0]}})


(defn ^:private ->color [s]
  (into []
        (comp
         (drop 1)
         (partition-all 2)
         (map (fn [xs]
                (Long/parseLong (str (nth xs 0)
                                     (nth xs 1))
                                16)))
         (map #(double (/ % 255))))
        s))

(def ant-color-primary
  (->color "#1677ff"))
(def ant-color-primary-hover
  (->color "#4096ff"))
(def ant-button-primary-shadow
  [(/ 5 255.0), (/ 145 255.0), (/ 255 255.0),
   0.1] )
(def ant-color-default-border-color
  (->color "#d9d9d9"))
(def ant-color-default-hover-border-color
  (->color "#4096ff"))
(def ant-color-error
  (->color "#ff4d4f"))
(def ant-color-error-hover
  (->color "#ff7875"))
(def ant-color-error-bg
  (->color "#fff2f0"))
(def ant-color-border-error-hover
  (->color "#ffa39e"))
(def ant-button-danger-shadow
  [(/ 255 255.0) (/ 38 255.0) (/ 5 255.0) 
   0.06])
(def ant-button-default-shadow
  [0 0 0 0.02])

(def ant-button-text-hover-bg
  [0 0 0 0.06])

(def color-white [1 1 1])

(def button-padding
  {:large [16 8]
   :default [16 5]
   :middle [16 5]
   :small [8 1]})

(defn button-text-style [{:keys [size type danger?]}]
  #:text-style
  {:font-size (get {:large 16
                    :default 16
                    :middle 16
                    :small 14}
                   size
                   16)
   :height-override true
   :height (get
            {:large 1.5
             :default 1.5714285714285714
             :middle 1.5714285714285714
             :small 1.5714285714285714}
            size
            1.5714285714285714)
   :color (case type
            :primary
            [1 1 1]
            
            ;; else
            (cond
              danger? ant-color-error

              :else
              [0 0 0 0.8]))})

(defn button-background-color [{:keys [type danger? hover?] :as m}]
  (match
   [m]

   [{:danger? true
     :type :primary
     :hover? true}] {:background-color ant-color-error-hover
                     :shadow-color ant-button-danger-shadow}

   [{:danger? true
     :type :primary}] {:background-color ant-color-error
                       :shadow-color ant-button-danger-shadow}

   [{:danger? true
     :type :text
     :hover? true}] {:background-color ant-color-error-bg
                     :shadow-color nil}

   [{:type :text
     :hover? true}] {:background-color ant-button-text-hover-bg}

   [{:type (:or :text :link)}] {:background-color nil
                                :shadow-color nil}

   [{:danger? true}] {:background-color color-white
                      :shadow-color [0, 0, 0, 0.02]}

   [{:type :primary
     :hover? true}] {:background-color ant-color-primary-hover
                     :shadow-color ant-button-primary-shadow}

   [{:type :primary}] {:background-color ant-color-primary
                       :shadow-color ant-button-primary-shadow}

   :else {:background-color color-white
          :shadow-color ant-button-default-shadow})
)

(defn button-background-border [{:keys [type danger? hover?] :as m}]
  (match
   [m]

   [{:type (:or :primary :text :link)}] {:background-border-color nil}

   [{:danger? true
     :type :default
     :hover? true}] {:background-border-color ant-color-border-error-hover}

   [{:danger? true}] {:background-border-color ant-color-error}

   [({:hover? true}
     :guard
     (fn [m]
       (let [t (:type m)]
         (or (nil? t)
             (= t :default)))))] {:background-border-color ant-color-default-hover-border-color}

   [(_
     :guard
     (fn [m]
       (let [t (:type m)]
         (or (nil? t)
             (= t :default)))))] {:background-border-color ant-color-default-border-color}

   :else (throw
          (ex-info "No matching"
                   {:button (into {} m)}))))


(defui button [{:keys [type
                       size
                       disabled?
                       loading?
                       danger?
                       block?
                       text
                       hover?]
                :as this}]
  (let [paragraph-style
        (assoc-in button-primary-paragraph-style
                  [:paragraph-style/text-style]
                  (button-text-style this))
        text (para/paragraph text nil
                             paragraph-style)
        [tw th] (ui/bounds text)

        [px py] (get button-padding
                     size
                     (get button-padding :default))
        w (+ tw (* 2 px ))
        h (+ th (* 2 py))
        rect (ui/rounded-rectangle
              w
              h
              6)

        {:keys [background-color
                shadow-color]} (button-background-color this)
        background (when background-color
                     (ui/with-color background-color
                       (ui/with-style ::ui/style-fill 
                         rect)))

        background-shadow (when shadow-color
                            (ui/fixed-bounds
                             [0 0]
                             (ui/with-color shadow-color
                               (ui/translate 0 2
                                             rect))))
        {:keys [background-border-color]} (button-background-border this)
        background-border (when background-border-color
                            (ui/with-color background-border-color
                              (ui/with-style ::ui/style-stroke
                                rect)))]
    (ui/fixed-bounds
     [w h]
     [background-shadow
      background
      background-border
      (ui/translate px py
                    text)])))

(defui debug-view [{}]
  (ui/translate
   20 20
   (apply
    ui/horizontal-layout
    (for [danger? [false true]]
      (apply
       ui/vertical-layout
       (into
        []
        (comp
         (interpose (ui/spacer 20)))
        (for [size [:small
                    :middle
                    :large]
              type [:primary
                    :default
                    ;; :dashed
                    :text
                    :link]]
          (let [hover? (get extra [:hover? size type danger?])]
            (basic/on-hover
             {:hover? hover?
              :body
              (button {:size size
                       :type type
                       :danger? danger?
                       :hover? hover?
                       :text (str size "-" type)})})))
        
        
        
        ))))
   #_(para/paragraph "ehllo" nil
                     #:paragraph-style
                     {:text-style
                      #:text-style { ;; :font-families ["SF Pro"]
                                    :color [0 0 0]}})))

(comment
  (skia/run (membrane.component/make-app #'debug-view {}))
  ,)

;; h1...h5
;; icons


;; # layout
;; divider
;; flex
;; grid
;; https://github.com/ant-design/ant-design/blob/3e156b0a7f3c0e943ede4b7be3e823fa74964f3b/components/layout/index.en-US.md?plain=1#L5
;; Layout
;; Space

;; Navigation

;;     Anchor
;;     Breadcrumb
;;     Dropdown
;;     Menu
;;     Pagination
;;     Steps

;; Data Entry

;;     AutoComplete
;;     Cascader
;;     Checkbox
;;     ColorPickerNew
;;     DatePicker
;;     Form
;;     Input
;;     InputNumber
;;     Mentions
;;     Radio
;;     Rate
;;     Select
;;     Slider
;;     Switch
;;     TimePicker
;;     Transfer
;;     TreeSelect
;;     Upload

;; Data Display

;;     Avatar
;;     Badge
;;     Calendar
;;     Card
;;     Carousel
;;     Collapse
;;     Descriptions
;;     Empty
;;     Image
;;     List
;;     Popover
;;     QRCodeNew
;;     Segmented
;;     Statistic
;;     Table
;;     Tabs
;;     Tag
;;     Timeline
;;     Tooltip
;;     TourNew
;;     Tree

;; Feedback

;;     Alert
;;     Drawer
;;     Message
;;     Modal
;;     Notification
;;     Popconfirm
;;     Progress
;;     Result
;;     Skeleton
;;     Spin
;;     Watermark



(defn foo
  "I don't do a whole lot."
  [x]
  (prn x "Hello, World!"))

