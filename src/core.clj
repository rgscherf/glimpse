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

    RuleExistence    = DataAccess <' must be present.'>
    RuleExistenceNum = [Symbol | DataAccess] <' must have '> Integer <' of '> Symbol+ <'.'>

    RuleNumComparison = DataAccess <' must be '> NumComparison Integer <'.'>
    <NumComparison>  = NCLT | NCEq | NCGT
    NCLT             = <'less than '>
    NCEq             = <'equal to '>
    NCGT             = <'greater than '>

    DataAccess       = Symbol SubKeyAccess
    <SubKeyAccess>   = <'\\'s'> Symbol (SubKeyAccess)*
    Integer          = #'-?\\d+'
    Symbol           = #'[A-Z][a-zA-Z_-]*'
    DefSymbol        = #'[A-Z][a-zA-Z_-]*'"
   :auto-whitespace whitespace))


;; TOKENIZING
(def hanging-a #"\s+[Aa]\s+")
(def hanging-the #"\s+[Tt]he\s+")
(def hanging-and #"\s+[Aa]nd\s+")
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


;; string testing
(def ruledef "This regulation concerns a Car. ")
(def rule-num-comparison "The Car's Crash-rating must be greater than 3.")
(def rule-existence-num-str "The Car must have 2 of Antilock-brakes, Driver-side-airbag, and Rear-view-camera.")
(def rule-existence-str "The Car's Antilock-brakes must be present.")

(comment
  (let [str-input (str/join [ruledef rule-existence-num-str])]
    (parse-str str-input))
  )


;; EVALUATION
;; All evaluation functions take a vec of [:ParserTag & rest]
;; :DataAccess
(defn unpack-symbol-vec
   [[_symbol-key keystr]]
   (-> keystr str/lower-case keyword))

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
	(data-access-fn env [:Symbol "Car"])
  (data-access-fn env [:DataAccess [:Symbol "Car"] [:Symbol "Second-row"] [:Symbol "Airbags"]])
  )

;; RuleExistence
(defn rule-existence-fn
  [_env [_tag result]]
  result)

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
  [env [_ data-access comparison num]]
  (let [eval-data (data-access-fn env data-access)
        eval-comparison (num-compare-fn comparison)
        eval-num (integer-fn num)]
    (eval-comparison eval-data eval-num)))
(comment
  (let [rule-nc-data [:RuleNumComparison [:DataAccess [:Symbol "Car"] [:Symbol "Crash-rating"]] [:NCGT] [:Integer "3"]]]
    (rule-num-comparison-fn env rule-nc-data)))

(defn get-map-from-accessor-tag
  [env accessor]
  (let [[a-head & _] accessor
        access-fn (match a-head
                    :Symbol get-toplevel-data
                    :DataAccess data-access-fn)]
    (access-fn env accessor)))
(comment
  (get-map-from-accessor-tag env [:DataAccess [:Symbol "Car"] [:Symbol "Second-row"]])
  (get-map-from-accessor-tag env [:Symbol "Car"]))

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