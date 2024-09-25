(ns core
  (:require [instaparse.core :as insta]
  					[clojure.core.match :refer [match]]
            [clojure.string :as str]))

;; It might look like:
;; This regulation concerns a Car.
;; A Car must have a Driver-side-airbag.
;; A Car must have 2 of Antilock-brakes, Electronic-stability-control, and a Rear-view-camera
;; Every Car's Second-row must have one of Airbags OR Shoulder-belts.
;; If the Car has a Third-row, every Car's Third-Row must have one of Airbags OR Shoulder-belts.
(declare resolve-clause-vec)

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

    RuleExistence    = DataAccess <' must have '> Symbol <'.'>
    RuleExistenceNum = DataAccess <' must have '> Integer <' of '> Symbol+ <'.'>

    RuleNumComparison = DataAccess <' must be '> NumComparison Integer <'.'>
    NumComparison     = NCLT | NCEq | NCGT
    NCLT              = <'less than '>
    NCEq              = <'equal to '>
    NCGT              = <'greater than '>

    DataAccess        = Symbol | (Symbol SubKeyAccess)
    <SubKeyAccess>    = <'\\'s'> Symbol (SubKeyAccess)*
    Integer           = #'-?\\d+'
    Symbol            = #'\\*\\*[a-zA-Z_ -]*\\*\\*'
    <DefSymbol>       = Symbol"
   :auto-whitespace whitespace))

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

(defn tags-for-rule-string
  [start-tag rule-str]
    (parse-str-with-rule-tag rule-str start-tag))
  (comment
    (tags-for-rule-string :RuleExistence "The **Car** must have **antilock brakes**." )
    (tags-for-rule-string :RuleExistence "**Car** must have **Antilock brakes**." )
    )

(defn resolve-symbol
  [_env [_symbol-key keystr]]
  (-> keystr
      (str/replace "**" "")
      (str/replace " " "-")
      str/lower-case
      keyword))
  (comment
    (resolve-symbol env [:Symbol "**Car**"]))

(defn resolve-data-access
  [env [_tag head & rest]]
  (if (empty? rest)
    (get env (resolve-clause-vec env head))
    (get-in env (map #(resolve-clause-vec env %) (cons head rest)))))
  (comment
    (resolve-data-access env (tags-for-rule-string :DataAccess "**Car**"))
    (resolve-data-access env (tags-for-rule-string :DataAccess "**Car**'s **second row**"))
    )

(defn resolve-rule-existence
  [env [_tag data-access search-key]]
  (get (resolve-clause-vec env data-access)
       (resolve-clause-vec env search-key)))
  (comment
    (resolve-rule-existence env [:RuleExistence [:DataAccess [:Symbol "**Car**"] [:Symbol "**second row**"]] [:Symbol "**airbags**"]])
    (resolve-rule-existence env (tags-for-rule-string :RuleExistence "The **Car** must have **antilock brakes**."))
    (resolve-rule-existence env (tags-for-rule-string :RuleExistence "The **Car**'s **second row** must have **airbags**."))
    )

(defn resolve-num-comparison
  [_env [_tag [comparison]]]
  (match comparison
    :NCLT <
    :NCEq =
    :NCGT >))

(defn resolve-integer
  [_env [_ int-str]]
  (parse-long int-str))

(defn resolve-rule-num-comparison
  "Numeric comparison of two values, the first being data."
  [env [_ data-access comparison num]]
  ((resolve-clause-vec env comparison)
   (resolve-clause-vec env data-access)
   (resolve-clause-vec env num)))
  (comment
    (tags-for-rule-string :RuleNumComparison "The **Car**'s **crash rating** must be greater than 3.")
    (resolve-rule-num-comparison env (tags-for-rule-string :RuleNumComparison "The **Car**'s **crash rating** must be greater than 3."))
    )

(defn resolve-clause-vec
  "The first element of the clause vec is a keyword denoting its parse rule.
  Every parse rule corresponds to a resolve-[clause-name] fn."
  [env clause]
  ((match (first clause)
     :Symbol              resolve-symbol
     :Integer             resolve-integer
     :NumComparison       resolve-num-comparison
     :DataAccess          resolve-data-access
     :RuleNumComparison   resolve-rule-num-comparison
     :RuleExistence       resolve-rule-existence)
   env clause))
  (comment
    (resolve-clause-vec env [:Symbol "**Car**"])
    (resolve-clause-vec env (tags-for-rule-string :DataAccess "**Car**"))
    (resolve-clause-vec env (tags-for-rule-string :DataAccess "**Car**'s **second row**"))
    )

;; TODO
;; resolve-rule-existence-num
;; date comparisons?