(ns com.phronemophobic.membrandt
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [membrane.skia.paragraph :as para]
            [membrane.skia :as skia]
            [membrane.ui :as ui]
            [membrane.basic-components :as basic]
            [clojure.core.match :refer [match]]
            [membrane.component
             :refer [defui defeffect]])
  (:import java.util.regex.Pattern))


;; https://ant.design/components/overview

;; # general
;; button

(defn glyph-index [para x y]
  (let [[idx aff] (para/glyph-position-at-coordinate para x y)]
    (if (= 1 aff)
      idx
      (dec idx))))

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


(defmacro ^:private cond-let
  "Takes a binding-form and a set of test/expr pairs. Evaluates each test
  one at a time. If a test returns logical true, cond-let evaluates and
  returns expr with binding-form bound to the value of test and doesn't
  evaluate any of the other tests or exprs. To provide a default value
  either provide a literal that evaluates to logical true and is
  binding-compatible with binding-form, or use :else as the test and don't
  refer to any parts of binding-form in the expr. (cond-let binding-form)
  returns nil."
  [binding & clauses]
  (when-let [[test expr & more] clauses]
    (if (= test :else)
      expr
      `(if-let [~binding ~test]
         ~expr
         (cond-let ~binding ~@more)))))

(def ^:private rgba-regex
  #"rgba\(\s*([0-9]+)[\s,]+([0-9]+)[\s,]+([0-9]+)[\s,]+([0-9.]+)\)\s*")

(defn ^:private ->color [s]
  (cond-let match
    (re-matches rgba-regex s)
    (let [[_ r g b a] match]
      [(/ (parse-long r) 255.0)
       (/ (parse-long g) 255.0)
       (/ (parse-long b) 255.0)
       (parse-double a)])
    
    (str/starts-with? s "#")
    (into []
          (comp
           (drop 1)
           (partition-all 2)
           (map (fn [xs]
                  (Long/parseLong (str (nth xs 0)
                                       (nth xs 1))
                                  16)))
           (map #(double (/ % 255))))
          s)

    :else
    (throw (IllegalArgumentException.
            (str "Cannot parse rgb color " s)))))



(def ^:private
  shadow-regex
  (let [whitespace "\\s*"
        length (str whitespace "([0-9]+)(?:px)?")
        regex-str (str length length length length whitespace rgba-regex)]
    (Pattern/compile regex-str)))
(defn ^:private ->shadow [s]
  (if-let [[_ ox oy blur-radius spread-radius r g b a] (re-matches shadow-regex s)]
    {:ox (parse-long ox)
     :oy (parse-long oy)
     :blur-radius (parse-long blur-radius)
     :spread-radius (parse-long spread-radius)
     :color [(/ (parse-long r) 255.0)
             (/ (parse-long g) 255.0)
             (/ (parse-long b) 255.0)
             (parse-double a)]}
    (throw (IllegalArgumentException. (str "Could not parse shadown: " s))))
  )

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
(def ant-color-bg-container-disabled
  [0 0 0 0.04])
(def ant-button-border-color-disabled
  [(/ 217 255.0) (/ 217 255.0) (/ 217 255.0)])
(def ant-color-text-disabled
  [0 0 0 0.25])
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

(defn button-text-style [{:keys [size type danger? disabled?]}]
  #:text-style
  {:font-size (get {:large 16
                    :default 14
                    :middle 14
                    :small 14}
                   size
                   14)
   :height-override true
   :height (get
            {:large 1.5
             :default 1.5714285714285714
             :middle 1.5714285714285714
             :small 1.5714285714285714}
            size
            1.5714285714285714)
   ;; TODO: add link and hover states
   :color (if disabled?
            ant-color-text-disabled
            (case type
              :primary
              [1 1 1]

              ;; else
              (cond
                danger? ant-color-error

                :else
                [0 0 0 0.8])))})

(defn button-background-color [{:keys [type danger? hover?] :as m}]
  (match
   m

   {:disabled? true
    :type (:or :text :link)} {:background-color nil}

   {:disabled? true} {:background-color ant-color-bg-container-disabled}

   {:danger? true
    :type :primary
    :hover? true} {:background-color ant-color-error-hover
                   :shadow-color ant-button-danger-shadow}

   {:danger? true
    :type :primary} {:background-color ant-color-error
                     :shadow-color ant-button-danger-shadow}

   {:danger? true
    :type :text
    :hover? true} {:background-color ant-color-error-bg
                   :shadow-color nil}

   {:type :text
    :hover? true} {:background-color ant-button-text-hover-bg}

   {:type (:or :text :link)} {:background-color nil
                              :shadow-color nil}

   {:danger? true} {:background-color color-white
                    :shadow-color [0, 0, 0, 0.02]}

   {:type :primary
    :hover? true} {:background-color ant-color-primary-hover
                   :shadow-color ant-button-primary-shadow}

   {:type :primary} {:background-color ant-color-primary
                     :shadow-color ant-button-primary-shadow}

   :else {:background-color color-white
          :shadow-color ant-button-default-shadow})
)

(defn button-background-border [{:keys [type danger? hover?] :as m}]
  (match
   m

   {:disabled? true
    :type (:or :text :link)} {:background-border-color nil}

   {:disabled? true} {:background-border-color ant-button-border-color-disabled}

   {:type (:or :primary :text :link)} {:background-border-color nil}

   {:danger? true
    :type :default
    :hover? true} {:background-border-color ant-color-border-error-hover}

   {:danger? true} {:background-border-color ant-color-error}

   ({:hover? true}
    :guard
    (fn [m]
      (let [t (:type m)]
        (or (nil? t)
            (= t :default))))) {:background-border-color ant-color-default-hover-border-color}

   (_
    :guard
    (fn [m]
      (let [t (:type m)]
        (or (nil? t)
            (= t :default))))) {:background-border-color ant-color-default-border-color}

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
                       hover?
                       on-click]
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
                                rect)))

        body (ui/fixed-bounds
              [w h]
              [background-shadow
               background
               background-border
               (ui/translate px py
                             text)])
        body (if on-click
               (ui/on
                :mouse-down
                (fn [_]
                  (on-click))
                body)
               ;; else
               body)]
    (basic/on-hover
     {:hover? hover?
      :$body nil
      :body body})))

(comment
  (dev/add-component-as-applet #'button-debug-view {})
  ,)

(defui button-debug-view [{}]
  (let [[cw ch] (get context :membrane.stretch/container-size
                     [800 800])]
    (basic/scrollview
     {:scroll-bounds [(- cw 7) (- ch 7)]
      :$body nil
      :body
      (ui/translate
       20 20
       (apply
        ui/horizontal-layout
        (for [danger? [false true]
              disabled? [false true]]
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
              (let [hover? (get extra [:hover? size type danger? disabled?])]
                (button {:size size
                         :type type
                         :danger? danger?
                         :disabled? disabled?
                         :hover? hover?
                         :text (str size "-" type (when disabled? "-disabled?"))}))))))))})))

(comment
  (skia/run (membrane.component/make-app #'button-debug-view {}))
  ,)


;; # Text Input

(s/def :text-input/disabled? boolean?)
;; showCount
;; Whether to show character count
;; (s/def :show-count? boolean?)

;; validation status
(s/def :text-input/status #{:warning :error :ok})
(s/def :text-input/size #{:small :middle :large})
(s/def :text-input/variant #{:outlined :borderless :filled})
(s/def :text-input/text string?)
(s/def :text-input/placeholder string?)

(def global-design-tokens
  { ;; "Container background color, e.g: default button, input box, etc. Be sure not to confuse this with `colorBgElevated`."
   :colorBgContainer (->color "#ffffff")
   ;; "Control the background color of container in disabled state."
   :colorBgContainerDisabled (->color "rgba(0, 0, 0, 0.04)")
   ;; "Default border color, used to separate different elements, such as: form separator, card separator, etc."
   :colorBorder (->color "#d9d9d9")
   ;; "Used to represent the visual elements of the operation failure, such as the error Button, error Result component, etc."
   :colorError (->color "#ff4d4f")
   ;; "The background color of the error state."
   :colorErrorBg (->color "#fff2f0")
   ;; "The hover state background color of the error state."
   :colorErrorBgHover (->color "#fff1f0")
   ;; "The hover state border color of the error state."
   :colorErrorBorderHover (->color "#ffa39e")
   ;; "The default state of the text in the error color."
   :colorErrorText (->color "#ff4d4f")
   ;; "The second level of fill color can outline the shape of the element more clearly, such as Rate, Skeleton, etc. It can also be used as the Hover state of the third level of fill color, such as Table, etc."
   :colorFillSecondary (->color "rgba(0, 0, 0, 0.06)")
   ;; "The third level of fill color is used to outline the shape of the element, such as Slider, Segmented, etc. If there is no emphasis requirement, it is recommended to use the third level of fill color as the default fill color."
   :colorFillTertiary (->color "rgba(0, 0, 0, 0.04)")
   ;; "Weak action. Such as `allowClear` or Alert close button"
   :colorIcon (->color "rgba(0, 0, 0, 0.45)")
   ;; "Weak action hover color. Such as `allowClear` or Alert close button"
   :colorIconHover (->color "rgba(0, 0, 0, 0.88)")
   ;; "Brand color is one of the most direct visual elements to reflect the characteristics and communication of the product. After you have selected the brand color, we will automatically generate a complete color palette and assign it effective design semantics."
   :colorPrimary (->color "#1677ff")
   ;; "Dark active state under the main color gradient."
   :colorPrimaryActive (->color "#0958d9")
   ;; "Hover state under the main color gradient."
   :colorPrimaryHover (->color "#4096ff")
   ;; "Used as the color of separator, this color is the same as colorBorderSecondary but with transparency."
   :colorSplit (->color "rgba(5, 5, 5, 0.06)")
   ;; "Default text color which comply with W3C standards, and this color is also the darkest neutral color."
   :colorText (->color "rgba(0, 0, 0, 0.88)")
   ;; "Control the font color of text description."
   :colorTextDescription (->color "rgba(0, 0, 0, 0.45)")
   ;; "Control the color of text in disabled state."
   :colorTextDisabled (->color "rgba(0, 0, 0, 0.25)")
   ;; "Control the color of placeholder text."
   :colorTextPlaceholder (->color "rgba(0, 0, 0, 0.25)")
   ;; "The fourth level of text color is the lightest text color, such as form input prompt text, disabled color text, etc."
   :colorTextQuaternary (->color "rgba(0, 0, 0, 0.25)")
   ;; "The third level of text color is generally used for descriptive text, such as form supplementary explanation text, list descriptive text, etc."
   :colorTextTertiary (->color "rgba(0, 0, 0, 0.45)")
   ;; "Used to represent the warning map token, such as Notification, Alert, etc. Alert or Control component(like Input) will use these map tokens."
   :colorWarning (->color "#faad14")
   ;; "The background color of the warning state."
   :colorWarningBg (->color "#fffbe6")
   ;; "The hover state background color of the warning state."
   :colorWarningBgHover (->color "#fff1b8")
   ;; "The hover state border color of the warning state."
   :colorWarningBorderHover (->color "#ffd666")
   ;; "The default state of the text in the warning color."
   :colorWarningText (->color "#faad14")
   ;; "Border radius of base components"
   :borderRadius 6
   ;; "LG size border radius, used in some large border radius components, such as Card, Modal and other components."
   :borderRadiusLG 8
   ;; "SM size border radius, used in small size components, such as Button, Input, Select and other input components in small size"
   :borderRadiusSM 4
   ;; "The height of the basic controls such as buttons and input boxes in Ant Design"
   :controlHeight 32
   ;; "LG component height"
   :controlHeightLG 40
   ;; "SM component height"
   :controlHeightSM 24
   ;; "The font family of Ant Design prioritizes the default interface font of the system, and provides a set of alternative font libraries that are suitable for screen display to maintain the readability and readability of the font under different platforms and browsers, reflecting the friendly, stable and professional characteristics."
   ;; :fontFamily -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, 'Noto Sans', sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'
   ;; "The most widely used font size in the design system, from which the text gradient will be derived."
   :fontSize 14
   :fontSizeLG 16
   ;; "Control the font size of operation icon in Select, Cascader, etc. Normally same as fontSizeSM."
   :fontSizeIcon 12
   ;; "Line height of text."
   :lineHeight 1.5714285714285714
   ;; "Line height of large text."
   :lineHeightLG 1.5
   ;; "Border style of base components"
   ;; :lineType solid
   ;; "Border width of base components"
   :lineWidth 1
   ;; "Motion speed, medium speed. Used for medium element animation interaction."
   ;; :motionDurationMid 0.2s
   ;; "Motion speed, slow speed. Used for large element animation interaction."
   ;; :motionDurationSlow 0.3s

   :padding 16

   ;; "Control the large padding of the element."
   :paddingLG 24
   ;; "Control the extra small padding of the element."
   :paddingXS 8
   ;; "Control the extra extra small padding of the element."
   :paddingXXS 4
   })
(def
  text-input-design-tokens
  { ;; "Background color when the input box is activated"
   :activeBg (->color "#ffffff")
   ;; "Active border color"
   :activeBorderColor (->color "#1677ff")
   ;; "Box-shadow when active"
   :activeShadow (->shadow "0 0 0 2px rgba(5, 145, 255, 0.1)")
   ;; "Background color of addon"
   :addonBg (->color "rgba(0, 0, 0, 0.02)")
   ;; "Box-shadow when active in error status"
   :errorActiveShadow (->shadow "0 0 0 2px rgba(255, 38, 5, 0.06)")
   ;; "Background color when the input box hovers"
   :hoverBg (->color "#ffffff")
   ;; "Hover border color"
   :hoverBorderColor (->color "#4096ff")
   ;; "Font size"
   :inputFontSize 14
   ;; "Font size of large"
   :inputFontSizeLG 16
   ;; "Font size of small"
   :inputFontSizeSM 14
   ;; "Vertical padding of input"
   :paddingBlock 4
   ;; "Vertical padding of large input"
   :paddingBlockLG 7
   ;; "Vertical padding of small input"
   :paddingBlockSM 0
   ;; "Horizontal padding of input"
   :paddingInline 11
   ;; "Horizontal padding of large input"
   :paddingInlineLG 11
   ;; "Horizontal padding of small input"
   :paddingInlineSM 7
   ;; "Box-shadow when active in warning status"
   :warningActiveShadow (->shadow "0 0 0 2px rgba(255, 215, 5, 0.1)")})

(defn text-border-color [m]
  (match
   m

   {:variant :borderless} nil
   {:disabled? true} (:colorBorder global-design-tokens)
   {:status :error} (:colorError global-design-tokens)
   {:status :warn} (:colorWarning global-design-tokens)
   {:focused? true}  (:activeBorderColor text-input-design-tokens)
   {:variant :filled} nil

   :else
   (:colorBorder global-design-tokens)))

(defn text-border-fill-color [m]
  (match
   m

   {:variant :filled
    :focused? true} nil
   {:variant :filled} (:colorFillTertiary global-design-tokens)
   {:disabled? true} (:colorBgContainerDisabled global-design-tokens)

   :else nil))

(defn text-input-text-style [m]
  (let [font-size
        (match
         m

         {:size :large} (:inputFontSizeLG text-input-design-tokens)
         {:size :small} (:inputFontSizeSM text-input-design-tokens)

         :else (:inputFontSize text-input-design-tokens))

        line-height
        (match
         m

         {:size :large} (:lineHeightLG global-design-tokens)

         :else (:lineHeight global-design-tokens))

        color (:colorText global-design-tokens)]
    #:text-style { ;; :font-families ["SF Pro"]
                  :font-size font-size
                  :height line-height
                  :height-override true
                  :color color}))

(defeffect ::previous-line [$cursor $select-cursor text]
  (run! #(apply dispatch! %)
        [[:set $select-cursor nil]
         [:update $cursor
          (fn [cursor]
            (let [prev-newline (.lastIndexOf ^String text "\n" (int (dec cursor)))]
              (if (not= -1 prev-newline)
                prev-newline
                0)))]]))

(defeffect ::next-line [$cursor $select-cursor text]
  (run! #(apply dispatch! %)
        [[:set $select-cursor nil]
         [:update $cursor
          (fn [cursor]
            (let [next-newline (.indexOf ^String text "\n" (int cursor))]
              (if (not= -1 next-newline)
                (inc next-newline)
                (count text))))]]))

(defeffect ::forward-char [$cursor $select-cursor text]
  (run! #(apply dispatch! %)
        [[:set $select-cursor nil]
         [:update $cursor
          (fn [cursor]
            (min (count text)
                 (inc cursor)))]]))


(defeffect ::backward-char [$cursor $select-cursor text]
  (run! #(apply dispatch! %)
        [[:set $select-cursor nil]
         [:update $cursor
          (fn [cursor]
            (max 0
                 (dec (min (count text) cursor))))]]))

(defeffect ::insert-newline [$cursor $select-cursor $text]
  (dispatch! ::insert-text $cursor $select-cursor $text "\n"))

(defeffect ::insert-text [$cursor $select-cursor $text s]
  (run! #(apply dispatch! %)
        [
         [:update [(list 'collect-one $cursor)
                   (list 'collect-one $select-cursor)
                   $text]
          (fn [cursor select-cursor text]
            (let [
                  start-clip-index (min
                                    (count text)
                                    (if select-cursor
                                      (min cursor select-cursor)
                                      cursor))
                  end-clip-index (min
                                  (count text)
                                  (if select-cursor
                                    (max cursor select-cursor)
                                    cursor))]
             (if text
               (str (subs text 0 start-clip-index) s (subs text end-clip-index))
               s)))]
         [:update [(list 'collect-one $select-cursor)
                   $cursor]
          (fn [select-cursor cursor]
            (let [cursor (or cursor 0)
                  index (if select-cursor
                          (min select-cursor cursor)
                          cursor)]
             (+ (count s) index)))]
         [:set $select-cursor nil]
         ]))


(defeffect ::move-cursor-to-pos [$cursor para pos]
  (run! #(apply dispatch! %)
        [[:update $cursor (fn [cursor]
                            (let [[mx my] pos
                                  [new-cursor _] (para/glyph-position-at-coordinate para mx my)
                                  ;;new-cursor (glyph-index para mx my)
                                  ]
                              new-cursor))]]))


(defeffect ::start-drag [$mpos $down-pos pos]
  (run! #(apply dispatch! %)
        [[:set $mpos pos]
         [:set $down-pos pos]]))


(defeffect ::drag [$mpos pos]
  (run! #(apply dispatch! %)
        [[:set $mpos pos]]))

(defeffect ::finish-drag [$select-cursor $cursor $down-pos pos text para]
  (let [[mx my] pos

        [idx aff] (para/glyph-position-at-coordinate para mx my)
        select-end-index (if (= 1 aff)
                           idx
                           (dec idx))]
    (run! #(apply dispatch! %)
          [
           [:update [(list 'collect-one $down-pos)
                     $select-cursor]
            (fn [down-pos select-cursor]
              (when-let [[dx dy] down-pos]
                (let [idx (glyph-index para dx dy)]
                  (when (not= idx select-end-index)
                    (if (> idx select-end-index)
                      (min (count text) (inc idx))
                      idx)))))]
           [:set $down-pos nil]
           [:update [(list 'collect-one $select-cursor)
                     $cursor]
            (fn [select-cursor cursor]
              (if (and select-cursor (> select-end-index select-cursor))
                (min (count text) (inc select-end-index))
                idx))]])))


(def double-click-threshold 500)
(let [getTimeMillis (fn [] (.getTime ^java.util.Date (java.util.Date.)))

      pow (fn [n x] (Math/pow n x))
      find-white-space (fn [text start]
                          (let [matcher (doto (re-matcher  #"\s" text)
                                          (.region start (count text)))]
                            (when (.find matcher)
                              (.start matcher))))]
  (defeffect ::text-double-click [$last-click $select-cursor $cursor pos text para]
    (let [now (getTimeMillis)
          [mx my] pos]
      (run! #(apply dispatch! %)
            [
             [:update [(list 'collect-one $last-click)
                       $select-cursor]
              (fn [[last-click [dx dy]] select-cursor]
                (if last-click
                  (let [diff (- now last-click)]
                    (if (and (< diff double-click-threshold)
                             (< (+ (pow (- mx dx) 2)
                                   (pow (- my dy) 2))
                                100))
                      (let [[index _] (para/glyph-position-at-coordinate para mx my)]
                        (if-let [start (find-white-space text index)]
                          start
                          (count text)))
                      select-cursor))
                  select-cursor))]
             [:update [(list 'collect-one $last-click)
                       $cursor]
              (fn [[last-click [dx dy]] cursor]
                (if last-click
                  (let [diff (- now last-click)]
                    (if (and (< diff double-click-threshold)
                             (< (+ (pow (- mx dx) 2)
                                   (pow (- my dy) 2))
                                100))
                      (let [[index _] (para/glyph-position-at-coordinate para mx my)
                            text-backwards (clojure.string/reverse text)]
                        (if-let [start (find-white-space text-backwards
                                                         (- (count text) index))]
                          (- (count text) start)
                          0)
                        )
                      cursor))
                  cursor))]

             [:set $last-click [now pos]]]))))


(defeffect ::delete-backward [$cursor $select-cursor $text]
  (run!
   #(apply dispatch! %)
   [
    [:update [(list 'collect-one $cursor)
              (list 'collect-one $select-cursor)
              $text]
     (fn [cursor select-cursor text]
       (let [cursor (min (count text) cursor)
             [clip-start clip-end] (if select-cursor
                                     (let [select-cursor (min (count text) select-cursor)]
                                       (if (< cursor select-cursor)
                                         [cursor select-cursor]
                                         [select-cursor cursor]))
                                     [(max 0 (dec cursor)) cursor])]
         (str (subs text 0 clip-start)
              (subs text clip-end))))]
    [:update [(list 'collect-one [$select-cursor])
              $cursor]
     (fn [select-cursor cursor]
       (max 0 (if select-cursor
                (min select-cursor cursor)
                (dec cursor))))]
    [:set $select-cursor nil]]))

(defn wrap-keyboard-events* [input elem]
  (ui/on
   :key-press
   (fn [s]
     (let [{:keys [$cursor
                   $select-cursor
                   text
                   $text]}
           input]
       (case s

         :up
         [[::previous-line $cursor $select-cursor  text]]

         :enter
         [[::insert-newline $cursor $select-cursor $text]]

         :down
         [[::next-line $cursor $select-cursor text]]

         :left
         [[::backward-char $cursor $select-cursor text]]

         :right
         [[::forward-char $cursor $select-cursor text]]

         :backspace
         [[::delete-backward $cursor $select-cursor $text]]

         ;; else
         (when (string? s)
           [[::insert-text  $cursor $select-cursor $text s]]))))
   elem))

(defn wrap-keyboard-events [input elem]
  (ui/maybe-key-press (:focused? input)
    (wrap-keyboard-events* input elem)))

(defn wrap-clipboard-events [input elem]
  (let [{:keys [text
                $text
                focused?
                select-cursor
                $select-cursor
                cursor
                $cursor]} input]
    (ui/on
     :clipboard-copy
     (fn []
       (when (and focused? select-cursor)
         [[:clipboard-copy (subs text
                                 (min cursor select-cursor)
                                 (max cursor select-cursor))]]))
     :clipboard-cut
     (fn []
       (when (and focused? select-cursor)
         (let [new-text (when text
                          (str (subs text 0 (min cursor select-cursor))
                               (subs text (max cursor select-cursor))))]
           [[:set $cursor (min cursor select-cursor)]
            [:set $select-cursor nil]
            [:set $text new-text]
            [:clipboard-cut (subs text
                                  (min cursor select-cursor)
                                  (max cursor select-cursor))]
            [::new-text new-text]])
         )
       )
     :clipboard-paste
     (fn [s]
       (when focused?
         [[::insert-text $cursor $select-cursor $text s]]))
     elem)))

(defn wrap-mouse-events [input px py para elem]
  (let [{:keys [text
                font
                select-cursor
                $select-cursor
                $cursor
                $mpos
                down-pos
                $down-pos
                $last-click]}
        input

        wrapper (fn [handler]
                  (fn [[mx my]]
                    (handler [(- mx px)
                              (- my py)])))]
    (ui/wrap-on
     :mouse-down (fn [handler pos]
                   (let [intents (handler pos)]
                     (cons [::request-focus]
                             intents)))
     (ui/on
      :mouse-up
      (wrapper
       (fn [[mx my :as pos]]
         [[::finish-drag $select-cursor $cursor $down-pos pos text para]
          [::text-double-click $last-click $select-cursor $cursor pos text para]]))

      :mouse-down
      (wrapper
       (fn [[mx my :as pos]]
         [[::move-cursor-to-pos $cursor para pos]
          [::start-drag $mpos $down-pos pos]
          [:set $select-cursor nil]]))

      :mouse-move
      (wrapper(fn [[mx my :as pos]]
         (when down-pos
           [[::drag $mpos pos]])))
      elem))))

(defui text-input* [{:keys [disabled?
                            status
                            size
                            variant
                            text
                            focused?
                            cursor
                            select-cursor
                            placeholder

                            mpos
                            down-pos
                            last-click
                            ]
                    :or {cursor 0
                         text ""}
                    :as this}]

  (let [border-radius (case size
                        :small (:borderRadiusSM global-design-tokens)
                        :middle (:borderRadius global-design-tokens)
                        :large (:borderRadiusLG global-design-tokens)
                        ;; else
                        (:borderRadius global-design-tokens))


        text-style (text-input-text-style this)
        para (para/paragraph
              text
              nil
              #:paragraph-style
              {:text-style text-style})
        [tw th] (ui/bounds para)

        placeholder-view
        (when (and placeholder
                   (= "" text))
          (let [;;:colorTextPlaceholder
                placeholder-text-style (assoc text-style
                                              :text-style/color (:colorTextPlaceholder
                                                                 global-design-tokens))
                placeholder-para (para/paragraph
                                  placeholder
                                  nil
                                  #:paragraph-style
                                  {:text-style placeholder-text-style})]
            placeholder-para))

        cursor (min cursor (count text))
        cursor-view
        (when-not select-cursor
          (let [{:keys [x y width height] :as rect}
               (cond

                 (= "" text)
                 (first
                  (para/get-rects-for-range (assoc para :paragraph " ")
                                            0 1
                                            :max
                                            :tight))

                 (>= cursor (count text))
                 (let [r1 (first
                           (para/get-rects-for-range (assoc para :paragraph " ")
                                                     0 1
                                                     :max
                                                     :tight))
                       r2 (first
                           (para/get-rects-for-range para
                                                     (dec cursor) cursor
                                                     :max
                                                     :tight))]
                   ;; should actually be using a place holder so that
                   ;; if the cursor is exactly at the wrap point,
                   ;; it should wrap
                   {:x (+ (:x r2) (:width r2))
                    :y (:y r2)
                    :width (:width r1)
                    :height (:height r1)})

                 :else
                 (first
                  (para/get-rects-for-range para cursor (inc cursor)
                                            :max
                                            :tight)))
               _ (assert rect)
                width (if (zero? width)
                        (-> (para/get-rects-for-range (assoc para :paragraph " ")
                                                      0 1
                                                      :max
                                                      :tight)
                            first
                            :width)
                        width)]
           (ui/translate x y
                         (ui/filled-rectangle
                          [0.5725490196078431
                           0.5725490196078431
                           0.5725490196078431
                           0.4]
                          width height))))

        selection-view
        (when select-cursor
          (into []
                (map (fn [{:keys [x y width height]}]
                       (ui/translate
                        x y
                        (let [selection-bg-color [0.6980392156862745
                                                  0.8431372549019608
                                                  1]]
                          (ui/filled-rectangle selection-bg-color
                                               width height)))))
                (para/get-rects-for-range para
                                          (min select-cursor cursor)
                                          (max select-cursor cursor)
                                          :max
                                          :tight)))

        ;; inc to include border
        px (inc
            (match
             this

             {:size :small} (:paddingInlineSM text-input-design-tokens)
             {:size :large} (:paddingInlineLG text-input-design-tokens)

             :else (:paddingInline text-input-design-tokens)))
        py (inc
            (match
             this

             {:size :small} (:paddingBlockSM text-input-design-tokens)
             {:size :large} (:paddingBlockLG text-input-design-tokens)

             :else (:paddingBlock text-input-design-tokens)))

        [pvw pvh] (ui/bounds placeholder-view)
        w (if-let [w (:flex-layout.stretch/width this)]
            w
            (+ px (max pvw tw) px))
        h (+ py (max pvh th) py)

        border-shape (ui/rounded-rectangle w h border-radius)
        border-color (text-border-color this)
        border (when border-color
                 (ui/with-style ::ui/style-stroke
                   (ui/with-color border-color
                     border-shape)))

        border-fill-color (text-border-fill-color this)
        border-fill (when border-fill-color
                      (ui/with-style ::ui/style-fill
                        (ui/with-color border-fill-color
                          border-shape)))

        elem (ui/fixed-bounds
              [w h]
              [border-fill
               border
               (ui/translate px py
                             [(when focused?
                                (if selection-view
                                  selection-view
                                  cursor-view))
                              placeholder-view
                              para])])]
    (->> elem
         (wrap-clipboard-events this)
         (wrap-mouse-events this px py para)
         (wrap-keyboard-events this))))

(defui text-input [{:keys [disabled?
                           status
                           size
                           variant
                           text
                           cursor
                           select-cursor
                           placeholder

                           mpos
                           down-pos
                           last-click

                           ^:membrane.component/contextual
                           focus]
                    :or {cursor 0
                         text ""}
                    :as this}]
  (let [focused? (and (= focus
                         $text)
                      (not disabled?))]
    (ui/on
     ::request-focus
     (fn []
       [[:set $focus $text]])
     (text-input* {:disabled? disabled?
                   :$disabled? $disabled?
                   :status status
                   :$status $status
                   :size size
                   :$size $size
                   :variant variant
                   :$variant $variant
                   :text text
                   :$text $text
                   :cursor cursor
                   :$cursor $cursor
                   :select-cursor select-cursor
                   :$select-cursor $select-cursor
                   :placeholder placeholder
                   :$placeholder $placeholder
                   :mpos mpos
                   :$mpos $mpos
                   :down-pos down-pos
                   :$down-pos $down-pos
                   :last-click last-click
                   :$last-click $last-click
                   :focused? focused?
                   :flex-layout.stretch/width (:flex-layout.stretch/width this)}))))

(defui text-input-debug-view [{}]
  (ui/translate
   20 20
   (apply
    ui/horizontal-layout
    (for [status [:ok :warn :error]]
      (apply
       ui/vertical-layout
       (into
        []
        (comp
         (interpose (ui/spacer 20)))
        (for [size [:small
                    :middle
                    :large]
              variant [:outlined
                       :borderless
                       :filled]]
          (let [hover? (get extra [:hover? size variant status])
                text (get extra [:text size variant status] "hello")]
            (basic/on-hover
             {:hover? hover?
              :body
              (text-input {:size size
                           :variant variant
                           :status status
                           :placeholder (str/join
                                         " "
                                         (eduction
                                          (map str)
                                          [size variant status]))
                           :text text})})))))))))

(comment
  (skia/run (membrane.component/make-app #'text-input-debug-view {}))
  ,)


;; # Radio



(def radio-design-tokens
  { ;; Background color of Radio button
   :buttonBg (->color "#ffffff")
   ;; Background color of checked Radio button
   :buttonCheckedBg (->color "#ffffff")
   ;; Background color of checked and disabled Radio button
   :buttonCheckedBgDisabled (->color "rgba(0, 0, 0, 0.15)")
   ;; Color of checked and disabled Radio button text
   :buttonCheckedColorDisabled (->color "rgba(0, 0, 0, 0.25)")
   ;; Color of Radio button text
   :buttonColor (->color "rgba(0, 0, 0, 0.88)")

   ;; not sure where specified. found empirically
   :buttonPaddingInlineSM 7

   ;; Horizontal padding of Radio button
   :buttonPaddingInline 15
   ;; Background color of checked solid Radio button text when active
   :buttonSolidCheckedActiveBg (->color "#0958d9")
   ;; Background color of checked solid Radio button text
   :buttonSolidCheckedBg (->color "#1677ff")
   ;; Color of checked solid Radio button text
   :buttonSolidCheckedColor (->color  "#ffffff")
   ;; Background color of checked solid Radio button text when hover
   :buttonSolidCheckedHoverBg (->color "#4096ff")
   ;; Color of disabled Radio dot
   :dotColorDisabled (->color "rgba(0, 0, 0, 0.25)")
   ;; Size of Radio dot
   :dotSize 8
   ;; Radio size
   :radioSize 16
   ;; Margin right of Radio button
   :wrapperMarginInlineEnd 8})


(s/def :radio-bar-item/text string?)
(s/def :radio-bar-item/selected? boolean?)
(s/def :radio-bar/position #{:left :middle :right})

(defn left-rrect [w h border-radius]
  (let [pad (+ border-radius 5)]
    (ui/fixed-bounds
     [w h]
     (ui/translate
      (- pad) (- pad)
      (ui/scissor-view
       [0 0]
       [(+ w pad)
        (+ h pad pad)]
       (ui/translate pad pad
                     (ui/rounded-rectangle
                      (+ w pad)
                      h
                      border-radius)))))))

(defn right-rrect [w h border-radius]
  (let [pad (+ border-radius 5)]
    (ui/fixed-bounds
     [w h]
     (ui/translate
      0 (- pad)
      (ui/scissor-view
       [0 0]
       [(+ w pad)
        (+ h pad pad)]
       (ui/translate (- pad) pad
                     (ui/rounded-rectangle
                      (+ w pad)
                      h
                      border-radius))))))
  )


(defui radio-bar-item [{:keys [text
                               selected?
                               hover?
                               position
                               size]
                        :as this}]
  (let [border-radius (case size
                        :small (:borderRadiusSM global-design-tokens)
                        :large (:borderRadiusLG global-design-tokens)
                        ;; else
                        (:borderRadius global-design-tokens))

        font-size (case size
                    :large (:fontSizeLG global-design-tokens)
                    ;; else
                    (:fontSize global-design-tokens))

        


        font-color (cond
                     selected? (:buttonSolidCheckedColor radio-design-tokens)
                     hover? (:buttonSolidCheckedHoverBg radio-design-tokens)
                     :else (:buttonColor radio-design-tokens))

        para (para/paragraph
              text
              nil
              #:paragraph-style
              {:text-style
               #:text-style {;; :font-families ["SF Pro"]
                             :font-size font-size
                             :color font-color}})
        [tw th] (ui/bounds para)
        pw (case size
             :small (:buttonPaddingInlineSM radio-design-tokens)
             (:buttonPaddingInline radio-design-tokens))
        ph 0

        w (+ pw pw
             tw
             (case position
               :left 1
               :right 1
               ;; else
               1))
        h (case size
            :small (:controlHeightSM global-design-tokens)
            :large (:controlHeightLG global-design-tokens)
            ;; else
            (:controlHeight global-design-tokens))

        border-color (if selected?
                       nil
                       (:colorBorder global-design-tokens))
        border-view (when border-color
                      (let [shape (case position
                                    :left (left-rrect w h border-radius)
                                    
                                    :right (right-rrect w h border-radius)
                                    ;;else
                                    [(ui/path [0 0] [w 0])
                                     (ui/path [0 h] [w h])])]
                       (ui/with-color border-color
                         (ui/with-style ::ui/style-stroke
                           shape))))
        background-color (if selected?
                            (:buttonSolidCheckedBg radio-design-tokens)
                            nil)
        background-view (when background-color
                          (let [fw w
                                fh (+ 2 h)
                                shape (case position
                                        :left (left-rrect fw fh border-radius)
                                        
                                        :right (right-rrect fw fh border-radius)
                                        ;;else
                                        (ui/rectangle fw fh))]
                            (ui/with-color background-color
                              (ui/with-style ::ui/style-fill
                                (ui/translate
                                 0 -1
                                 shape)))))

        elem [background-view
              border-view
              (ui/center
               para
               [w h])]]
    (ui/fixed-bounds
     [(dec w) h]
     elem)))

(defn radio-bar-separator [{:keys [size]
                            :as this}]
  (let [h (case size
            :small (:controlHeightSM global-design-tokens)
            :large (:controlHeightLG global-design-tokens)
            ;; else
            (:controlHeight global-design-tokens))

        border-color (:colorBorder global-design-tokens)
        border-view (when border-color
                      (let [shape (ui/path [0 (- 0.5)] [0 (+ h 1)])]
                        (ui/with-color border-color
                          (ui/with-style ::ui/style-stroke
                            shape))))]
    (ui/fixed-bounds
     [0 0]
     border-view)))

(defui debug-radio-bar-item [{}]
  (ui/translate 5 5
                (ui/horizontal-layout
                 (radio-bar-item {:text "Hangzhou"
                                  :position :left})
                 (radio-bar-separator {})
                 (radio-bar-item {:text "Shanghai"
                                  :selected? true
                                  :position :middle})
                 (radio-bar-item {:text "Beijing"
                                  :position :middle
                                  
                                  })
                 (radio-bar-item {:text "Chengdu"
                                  :position :right}))
                
                ))


(comment
  (skia/run (membrane.component/make-app #'debug-radio-bar-item
                                         {}))
  ,)

(defui radio-bar [{:keys [size
                          options
                          selection]}]
  (let [buttons
        (into
         []
         (map-indexed
          (fn [i option]
            (let [position (cond
                             (zero? i) :left
                             (= (dec (count options)) i) :right
                             :else :middle)
                  val (:value option)
                  hover? (get extra [::hover val])]
              (basic/on-hover
               {:hover? hover?
                :$body nil
                :body
                (ui/on
                 :mouse-down
                 (fn [_]
                   [[:set $selection val]])
                 (radio-bar-item {:text (:text option)
                                  :hover? hover?
                                  :position position
                                  :selected? (= val selection)
                                  :size size}))}))))
         options)
        bar (radio-bar-separator {:size size})
        bars
        (loop [offset (ui/width (first buttons))
               prev-option (first options)
               buttons (next buttons)
               options (next options)
               bars []]
          (let [option (first options)]
            (if options
              (let [show-bar? (and (not= selection
                                     (:value prev-option))
                                   (not= selection
                                     (:value option)))
                    button (first buttons)]
                (recur (+ (ui/width button)
                          offset)
                       option
                       (next buttons)
                       (next options)
                       (if show-bar?
                         (conj bars
                               (ui/translate offset 0 bar))
                         bars)))
              bars)))]
    [(apply
      ui/horizontal-layout
      buttons)
     bars]))

(defui debug-radio-bar [{}]
  (let [size (get extra :size)]
    (ui/translate
     5 5
     (apply
      ui/vertical-layout
      (radio-bar
       {:options [{:text "small" :value :small}
                  {:text "middle" :value :middle}
                  {:text "large" :value :large}]
        :selection size})
      (ui/spacer 8)
      (eduction
       (map (fn [n]
              (subvec ["a" "B" "C" "d" "e"]
                      0 n)))
       (map (fn [options]
              (radio-bar
               {:extra (get extra [:extra options])
                :size size
                :options
                (into []
                      (map (fn [x]
                             {:text (str x)
                              :value x}))
                      options)})))
       (interpose (ui/spacer 10))
       (range 2 5))))))

(comment
  (skia/run (membrane.component/make-app #'debug-radio-bar
                                         {}))
  ,)

;; # Progressbar

(def progress-bar-design-tokens
  {
   ;; "Icon size of circular progress bar"
   ;; :circleIconFontSize string	1.1666666666666667em
   ;; "Text color of circular progress bar"
   :circleTextColor (->color "rgba(0, 0, 0, 0.88)")
   ;; "Text size of circular progress bar"
   ;; :circleTextFontSize string	1em
   ;; "Default color of progress bar"
   :defaultColor (->color "#1677ff")
   ;; "Border radius of line progress bar"
   :lineBorderRadius 100
   ;; "Color of remaining part of progress bar"
   :remainingColor (->color "rgba(0, 0, 0, 0.06)") })
(defui progress-bar [{:keys [progress
                             height
                             width]}]
  (let [;; found empirically
        height (if (number? height)
                 height
                 (case height
                   :small 6
                   :middle 8))
        bg-color (:remainingColor progress-bar-design-tokens)
        bg (ui/with-color bg-color
             (ui/rounded-rectangle
              width height
              (/ height 2.0)))

        fg-color (:defaultColor progress-bar-design-tokens)
        fg (ui/with-color fg-color
             (ui/rounded-rectangle
              (min width
                   (* width progress))
              height
              (/ height 2)))]
    [bg
     fg]))

(comment

  (skia/run
    (constantly
     (ui/translate
      5 5
      (apply
       ui/vertical-layout
       (eduction
        (map (fn [pct]
               (/ pct 100.0)))
        (mapcat (fn [progress]
                  (for [height [:small :middle]]
                    (progress-bar {:width 400
                                   :progress progress
                                   :height height}))))
        (interpose (ui/spacer 16))
        (range 0 105 10))
       ))))

  ,)

(def number-slider-design-tokens
  {
   ;; Height of slider
   :controlSize 10
   ;; Border color of dot when active
   :dotActiveBorderColor (->color "#91caff")
   ;; Border color of dot
   :dotBorderColor (->color "#f0f0f0")
   ;; Size of dot
   :dotSize 8
   ;; Color of handle when active
   :handleActiveColor (->color "#1677ff")
   ;; Color of handle
   :handleColor (->color "#91caff")
   ;; Color of handle when disabled
   :handleColorDisabled (->color "#bfbfbf")
   ;; Border width of handle
   :handleLineWidth 2
   ;; Border width of handle when hover
   :handleLineWidthHover 4
   ;; Size of handle
   :handleSize 10
   ;; Size of handle when hover
   :handleSizeHover 12
   ;; Background color of rail
   :railBg (->color "rgba(0, 0, 0, 0.04)")
   ;; Background color of rail when hover
   :railHoverBg (->color "rgba(0, 0, 0, 0.06)")
   ;; Height of rail
   :railSize 4
   ;; Background color of track
   :trackBg (->color "#91caff")
   ;; Background color of track when disabled
   :trackBgDisabled (->color "rgba(0, 0, 0, 0.04)")
   ;; Background color of track when hover
   :trackHoverBg (->color "#69b1ff")})

(defeffect ::update-number-slider [{:keys [slider mpos]}]
  (let [[mx my] mpos
        pct (/ mx
               (:width slider))
        new-val (* pct
                   (+ (:min slider)
                      (- (:max slider)
                         (:min slider))))
        new-val (if (:integer? slider)
                  (java.lang.Math/round new-val)
                  new-val)
        new-val (max
                 (:min slider)
                 (min
                  new-val
                  (:max slider)))]
    (dispatch! [[:set (:$value slider) new-val]])))

(defeffect ::adjust-number-slider [{:keys [slider delta]}]
  (let [new-val (+ (:value slider)
                   delta)
        new-val (max
                 (:min slider)
                 (min
                  new-val
                  (:max slider)))]
    (dispatch! [[:set (:$value slider) new-val]])))

(defui number-slider* [{:keys [focused?
                               hover?
                               integer?
                               width
                               min
                               max
                               value]
                        :as this}]
  (let [mdown? (get extra :mdown?)
        value (clojure.core/max
               min
               (clojure.core/min
                max value))
        
        height (:railSize number-slider-design-tokens)
        bg-color (if hover?
                   (:railHoverBg number-slider-design-tokens)
                   (:railBg number-slider-design-tokens))
        bg (ui/filled-rectangle
            bg-color
            width height)

        

        track-width (* (/ (- value min)
                          (- max min))
                       width)
        track-color (:trackBg number-slider-design-tokens)
        track (ui/filled-rectangle track-color
                                   track-width height)

        handle-size (if focused?
                      (:handleSizeHover number-slider-design-tokens)
                      (:handleSize number-slider-design-tokens))
        handle-shape (ui/rounded-rectangle
                      handle-size
                      handle-size
                      (/ handle-size 2))
        handle-color (if focused?
                       (:handleActiveColor number-slider-design-tokens)
                       (:handleColor number-slider-design-tokens))
        handle-stroke-width (if focused?
                              (:handleLineWidthHover number-slider-design-tokens)
                              (:handleLineWidth number-slider-design-tokens))
        handle-stroke (ui/with-style ::ui/style-stroke
                        (ui/with-color handle-color
                          (ui/with-stroke-width handle-stroke-width
                            handle-shape)))
        handle-fill (ui/with-style ::ui/style-fill
                      (ui/with-color color-white
                        handle-shape))
        handle-view
        (ui/translate
         (- track-width
            (/ handle-size 2))
         (/ (- (:handleSizeHover number-slider-design-tokens)
               handle-size)
            2)
         [handle-fill
          handle-stroke])

        track-offset-y (/ (- (:handleSizeHover number-slider-design-tokens)
                             height)
                          2)
        elem (ui/fixed-bounds
              [width
               (:handleSizeHover number-slider-design-tokens)]
              [(ui/translate
                0 track-offset-y
                [bg
                 track])
               handle-view])

        elem+events
        (if mdown?
          (ui/on
           :mouse-up
           (fn [mpos]
             [[::update-number-slider {:slider this
                                       :mpos mpos}]
              [:set $mdown? false]])
           :mouse-move
           (fn [mpos]
             [[::update-number-slider {:slider this
                                       :mpos mpos}]])
           :mouse-move-global
           (fn [[mx my]]
             (let [[w h] (ui/bounds elem)]
               (when (or (neg? mx)
                         (> mx w)
                         (neg? my)
                         (> my h))
                 [[:set $mdown? false]])))
           elem)
          ;; else
          (ui/on
           :mouse-down
           (fn [mpos]
             [[:set $mdown? true]
              [::request-focus]
              [::update-number-slider {:slider this
                                       :mpos mpos}]])
           elem))

        elem+events
        (if focused?
          (ui/on
           :key-press
           (fn [s]
             (case s
               :left [[::adjust-number-slider {:delta (- 1)
                                               :slider this}]]
               :right [[::adjust-number-slider {:delta 1
                                                :slider this}]]
               ;; else
               nil
               ))
           elem+events)
          elem+events)
        ]
    elem+events))

(defui number-slider [{:keys [^:membrane.component/contextual
                              focus
                              hover?
                              integer?
                              width
                              min
                              max
                              value]}]
  (let [focused? (= focus
                    $value)]
    (ui/on
     ::request-focus
     (fn []
       [[:set $focus $value]])
     (number-slider* {:focused? focused?
                      
                      :hover? hover?
                      :$hover? $hover?
                      :integer? integer?
                      :$integer? $integer?
                      :width width
                      :$width $width
                      :min min
                      :$min $min
                      :max max
                      :$max $max
                      :value value
                      :$value $value}))))

(defui debug-number-slider [{}]
  (ui/translate
   5 5
   
   (let [num (get extra :value 75)]
     (ui/vertical-layout
      (number-slider {:width 200
                      :min 0
                      :max 10
                      :integer? false
                      :value num})
      (ui/label num)))))

(comment
  (skia/run
    (membrane.component/make-app #'debug-number-slider {}))
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

