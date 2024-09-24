(ns core
  (:require [instaparse.core :as insta]
  					[clojure.core.match :refer [match]]
            [clojure.string :as str]))

;; All cars must have:
;;  - a driver's side airbag
;;  - for the second row, an airbag OR shoulder belt for each seat.
;;  - if there is a third row, an airbag OR shoulder belt for each seat.
;;  - two of:
;;    - antilock brakes
;;    - electronic stability control
;;    - rear view camera

;; It might look like:

;; This regulation concerns a Car.
;; A Car must have a Driver-side-airbag.
;; A Car must have 2 of Antilock-brakes, Electronic-stability-control, and a Rear-view-camera
;; Every Car's Second-row must have one of Airbags OR Shoulder-belts.
;; If the Car has a Third-row, every Car's Third-Row must have one of Airbags OR Shoulder-belts.

(def env
  {:errors []
   :car {:driver-side-airbag           true
         :antilock-brakes              true
         :electronic-stability-control true
         :rear-view-camera             true
         :crash-rating                 5
         :second-row                   {:airbags        "I have airbags"
                                        :shoulder_belts false }}})

;; THE PARSER

(def whitespace
  (insta/parser
    "whitespace = #'\\s+'"))
(def car-reg
  (insta/parser
   "<Reg>            = Def+ Rule*
    Def              = <'This regulation concerns '> DefSymbol <'.'>
    <Rule>           = RuleExistence | RuleExistenceNum | RuleNumComparison

    RuleExistence    = [Symbol | DataAccess] <' must have '> Symbol <'.'>
    RuleExistenceNum = [Symbol | DataAccess] <' must have '> Integer <' of '> Symbol+ <'.'>

    RuleNumComparison = DataAccess <' must be '> NumComparison Integer <'.'>
    <NumComparison>  = NCLT | NCEq | NCGT
    NCLT             = <'less than '>
    NCEq             = <'equal to '>
    NCGT             = <'greater than '>

    DataAccess       = Symbol SubKeyAccess
    <SubKeyAccess>   = <'\\'s'> Symbol (SubKeyAccess)*
    Integer          = #'-?\\d+'
    Symbol           = #'\\*\\*[a-zA-Z_ -]*\\*\\*'
    <DefSymbol>        = Symbol"
   :auto-whitespace whitespace))

;; old symbol line
;; Symbol           = #'[A-Z][a-zA-Z_-]*'
;; old rule existence
;; RuleExistence    = DataAccess <' must be present.'>

(car-reg "**Car**'s **Antilock brakes** must have **Antilock brakes**." :start :RuleExistence)
;; TOKENIZING
(def hanging-a #"\s*[Aa]\s+")
(def hanging-the #"\s*[Tt]he\s+")
(def hanging-and #"\s*[Aa]nd\s+")
(def comma #",")

(defn cleanup
  [input-str]
  (-> input-str
  		(str/replace comma "")
      (str/replace hanging-and " ")
      (str/replace hanging-the " ")
      (str/replace hanging-a " ")))

(defn parse-str
  [str]
  (-> str cleanup car-reg))

(defn parse-str-with-rule-tag
  [str start-tag]
  (-> str cleanup (car-reg :start start-tag)))

;; string testing
(def ruledef "This regulation concerns a **Car**. ")
(comment
  (let [str-input (str/join [ruledef ""])]
    (parse-str str-input))
  )

(defn tags-for-rule-string
  [start-tag rule-str]
    (parse-str-with-rule-tag rule-str start-tag))
  (comment
    (tags-for-rule-string :RuleExistence "The **Car** must have **antilock brakes**." )
    (tags-for-rule-string :RuleExistence "**Car** must have **Antilock brakes**." )
    )


;; EVALUATION
;; All evaluation functions take a vec of [:ParserTag & rest]
;; :DataAccess
(defn unpack-symbol-vec
  [[_symbol-key keystr]]
  (-> keystr
      ( str/replace "**" "")
      (str/replace " " "-")
      str/lower-case
      keyword))
  (comment
    (unpack-symbol-vec [:Symbol "**Car**"]))

(defn get-toplevel-data
  [env head-symbol]
  (get env (unpack-symbol-vec head-symbol)))

(defn data-access-fn
  [env access-vec]
  (let [[_ head-symbol & rest] access-vec
        data-map (get-toplevel-data env head-symbol)]
    (get-in data-map
            (map unpack-symbol-vec rest))))
  (comment
  	(data-access-fn env (tags-for-rule-string :DataAccess "The **Car**'s **second row**"))
    (data-access-fn env [:DataAccess [:Symbol "Car"] [:Symbol "second row"] [:Symbol "airbags"]])
    )

;; NUMERIC COMPARISONS
(defn num-compare-fn
  [[comparison]]
  (match comparison
    :NCLT <
    :NCEq =
    :NCGT >))

(defn integer-fn
  [[_ int-str]]
  (parse-long int-str))

(defn rule-num-comparison-fn
  "Numeric comparison of two values, the first being data."
  [env [_ data-access comparison num]]
  (let [eval-data (data-access-fn env data-access)
        eval-comparison (num-compare-fn comparison)
        eval-num (integer-fn num)]
    (eval-comparison eval-data eval-num)))
(comment
	(rule-num-comparison-fn env
                         (tags-for-rule-string
                          :RuleNumComparison
                          "The **Car**'s **crash rating** must be greater than 3."))
  )

(defn get-map-from-accessor-tag
  "Retrieve a map from the env, either a top-level symbol or a DataAccess vec."
  [env accessor]
  (let [[a-head & _] accessor
        access-fn (match a-head
                    :Symbol get-toplevel-data
                    :DataAccess data-access-fn)]
    (access-fn env accessor)))
(comment
  (get-map-from-accessor-tag env [:DataAccess [:Symbol "**Car**"] [:Symbol "**Antilock brakes**"]])
  (get-map-from-accessor-tag env [:Symbol "Car"]))

;; RuleExistence
(defn rule-existence-fn
  "Test for the existence of a key in the map defined by data-access-vec"
  [env [_tag data-access-vec test-key-vec]]
  (let [data-object  (get-map-from-accessor-tag env data-access-vec)
        test-key (unpack-symbol-vec test-key-vec)]
    (get data-object test-key)))
  (comment
    (rule-existence-fn env [:RuleExistence [:DataAccess [:Symbol "**Car**"] [:Symbol "**second row**"]] [:Symbol "**airbags**"]]))
    (rule-existence-fn env (tags-for-rule-string :RuleExistence "The **Car** must have **antilock brakes**."))
    (rule-existence-fn env (tags-for-rule-string :RuleExistence "The **Car**'s **second row** must have **airbags**."))

;; RuleExistenceNum
(defn rule-existence-num-fn
  [env [_tag accessor num & symbols]]
  (let [data (get-map-from-accessor-tag env accessor)
        num-int (integer-fn num)
        ;; TODO: (>= num-int (length (filter #(existence data %) [accessor-values+])))
        ]))
  (comment
    (let [rule-existence-num-tree
          [:RuleExistenceNum [:Symbol "Car"] [:Integer "2"] [:Symbol "Antilock-brakes"] [:Symbol "Driver-side-airbag"] [:Symbol "Rear-view-camera"]]]
      rule-existence-num-tree)
    )

;; test types to think about
;; - existence
;; - numerical comparison
;; - date comparison
;; - # of