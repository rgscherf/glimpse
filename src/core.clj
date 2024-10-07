(ns core
  (:require [instaparse.core :as insta]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(declare resolve-clause-vec)

;; Target reg

(def target-reg-text
  "# Car safety regulation

  ## Definitions

  1. A **Car** _is a vehicle with four wheels._
  2. A **Car** contains:
    - **antilock brakes** _, which increases safety in high-speed situations._
    - a **rear-view camera** _, which increases safety in low-speed situations._
    - a **crash rating** _, on a scale of 1-5, assigned by the NTSB._

  ## Rules

  3. A **Car** must have 2 of **antilock brakes**, **electronic-stability-control**, and a **rear view mirror** _in order to promote balanced safety_.
  4. _Additionally, the Province requires that cars perform as expected in crashes. There are two measures, both of which must be met._ The **Car**'s **crash rating** must be greater than 3. Also, the **Car** must have a **driver side airbag**.")

;; Default environment

(def test-env
  {:errors []
   :data {:car  {:driver-side-airbag           true
                 :antilock-brakes              true
                 :electronic-stability-control true
                 :rear-view-camera             true
                 :crash-rating                 2
                 :second-row                   {:airbags        "I have airbags"
                                                :shoulder-belts false}}}})
(def env (atom {}))
(defn reset-env [] (reset! env test-env))
(reset-env)

;; Parser

(def whitespace
  (insta/parser
   "whitespace = #'\\s+'"))

(def car-reg
  (insta/parser
   "Regulation                   = Headline Definitions Rules
    <Headline>                   = <'# '> <Word*>
    <Definitions>                = <'## Definitions'> DefinitionClause+
    <Rules>                      = <'## Rules'> RuleClause+

    ClauseNum = Integer <'. '>
    RuleClause = ClauseNum Rule+
    DefinitionClause = ClauseNum ForwardDeclaration+

    <ForwardDeclaration>         = DataAccess | NestedFwdDeclaration
    NestedFwdDeclaration         = DataAccess <' contains: '> NestedFwdDeclarationSubkey+
    <NestedFwdDeclarationSubkey> = <'- '> DataAccess

    Def                          = <'This regulation concerns '> DefSymbol <'.'>
    <Rule>                       = RuleExistence | RuleExistenceNum | RuleNumComparison

    RuleExistence                = DataAccess <' must have '> Symbol <'.'>
    RuleExistenceNum             = DataAccess <' must have '> Integer <' of '> Symbol+ <'.'>

    RuleNumComparison             = DataAccess <' must be '> NumComparison Integer <'.'>
    NumComparison                 = NCLT | NCEq | NCGT
    NCLT                          = <'less than '>
    NCEq                          = <'equal to '>
    NCGT                          = <'greater than '>

    DataAccess                    = Symbol | (Symbol SubKeyAccess)
    <SubKeyAccess>                = <'\\'s'> Symbol (SubKeyAccess)*
    Integer                       = #'-?\\d+'
    Word                          = #'[a-zA-Z]*'
    Symbol                        = #'\\*\\*[a-zA-Z -]*\\*\\*'
    <DefSymbol>                   = Symbol"
   :auto-whitespace whitespace))


;; Tokenizing

(def hanging-a #"\s*[Aa]\s+")
(def hanging-the #"\s*[Tt]he\s+")
(def hanging-and #"\s*[Aa]nd\s+")
(def hanging-also #"\s*[Aa]lso\s+")
(def multi-space #"\s{2,}")
(def comma #",")
(def code-comment #"_.*?_")

(def replacement-regexes
  [comma
   hanging-a
   hanging-the
   hanging-and
   hanging-also
   code-comment])

(defn cleanup
  [input-str]
  (let [cleaned-except-for-spacing
        (reduce #(str/replace %1 %2 " ")
                input-str
                replacement-regexes)]
    (str/replace cleaned-except-for-spacing multi-space " ")))

(defn parse-str
  [str]
  (-> str cleanup car-reg))

(defn parse-str-with-rule-tag
  [str start-tag]
  (-> str cleanup (car-reg :start start-tag)))

(defn tags-for-rule-string
  [start-tag rule-str]
  (parse-str-with-rule-tag rule-str start-tag))
(comment
  (tags-for-rule-string :RuleExistence "_This car is a car_ The **Car** must_must have_ have **antilock brakes**.")
  (tags-for-rule-string :RuleExistence "**Car** must have **Antilock brakes**."))

(comment
 target-reg-text
	(cleanup target-reg-text)
  (parse-str target-reg-text)
  (tags-for-rule-string :Headline "# sar safety regulations")
  (tags-for-rule-string :Definitions "## Definitions **Car**")
  (tags-for-rule-string :Definitions "## Definitions **Car** contains: - **antilock brakes** - **rear-view camera** - **crash rating**")
  (tags-for-rule-string :Definitions "## Definitions **Car** **Car** contains: - **antilock brakes** - **rear-view camera** - **crash rating**")
  )

(defn make-error-record
  [ctx clause-vec]
  {:clause-num (:clause-num ctx)
   :section (:section ctx)
   :clause-vec clause-vec})

(defn false-with-env-error
  [env ctx clause-vec]
  (swap! env assoc :errors (cons (make-error-record ctx clause-vec)
                                 (:errors @env)))
  false)


(defn resolve-symbol
  [_env _ctx [_symbol-key keystr]]
  (-> keystr
      (str/replace "**" "")
      (str/replace " " "-")
      str/lower-case
      keyword))
(comment
  (resolve-symbol env 0 [:Symbol "**Car**"]))

(defn resolve-data-access
  [env ctx [_tag head & rest]]
  (let [envmap (:data @env)]
    (if (empty? rest)
      (get envmap (resolve-clause-vec env ctx head))
      (get-in envmap (map #(resolve-clause-vec env ctx %) (cons head rest))))))
(comment
  (resolve-data-access env 0 (tags-for-rule-string :DataAccess "**Car**"))
  (resolve-data-access env 0 (tags-for-rule-string :DataAccess "**Car**'s **second row**"))
  (tags-for-rule-string :DataAccess "**Car**'s **second row**"))

(defn resolve-rule-existence
  [env ctx [_tag data-access search-key]]
  (get (resolve-clause-vec env ctx data-access)
       (resolve-clause-vec env ctx search-key)))
(comment
  (resolve-rule-existence env 0 [:RuleExistence [:DataAccess [:Symbol "**Car**"] [:Symbol "**second row**"]] [:Symbol "**airbags**"]])
  (resolve-rule-existence env 0 (tags-for-rule-string :RuleExistence "The **Car** must have **antilock brakes**."))
  (resolve-rule-existence env 0 (tags-for-rule-string :RuleExistence "The **Car**'s **second row** must have **airbags**.")))

(defn resolve-num-comparison
  [_env _ctx [_tag [comparison]]]
  (match comparison
    :NCLT <
    :NCEq =
    :NCGT >))

(defn resolve-integer
  [env ctx [_ int-str :as clause-vec]]
  (let [res (parse-long int-str)]
    (if res
      true
      (false-with-env-error env ctx clause-vec))))

(defn resolve-rule-num-comparison
  "Numeric comparison of two values, the first being data."
  [env ctx [_ data-access comparison num :as clause-vec]]
  (let [comparison-result
        ((resolve-clause-vec env ctx comparison)
         (resolve-clause-vec env ctx data-access)
         (resolve-clause-vec env ctx num))]
    (if comparison-result
      true
      (do (false-with-env-error env ctx clause-vec)
          false))))
(comment
  (let [vec
        [:RuleNumComparison [:DataAccess [:Symbol "**Car**"] [:Symbol "**crash rating**"]] [:NumComparison [:NCGT]] [:Integer "3"]]
        at (atom {:data {:car {:crash-rating 2}}})
        ctx {:clause-num 1
             :section :rules}]
    (resolve-rule-num-comparison at ctx vec)
    @at)
  (tags-for-rule-string :RuleNumComparison "The **Car**'s **crash rating** must be greater than 3.")
  (resolve-rule-num-comparison env 0 (tags-for-rule-string :RuleNumComparison "The **Car**'s **crash rating** must be greater than 3."))
  )

(defn truthy?
  [x]
  (not (or (nil? x) (false? x))))
(comment
  (truthy? 3)
  (truthy? nil)
  (truthy? false))

(defn resolve-rule-existence-num
  [env ctx [_tag data-access num & access-key-symbols :as this-vec]]
  (let [data-to-access (resolve-data-access env ctx data-access)
        result (->> access-key-symbols
                    (map #(resolve-symbol env ctx %))
                    (map #(get data-to-access %))
                    (filter truthy?)
                    count
                    (<= (resolve-integer env ctx num)))]
    (if result
      true
      (false-with-env-error env ctx this-vec))))
(comment
	(reset-env)
  @env
  (resolve-rule-existence-num env 0
                              (tags-for-rule-string :RuleExistenceNum "The **Car** must have 2 of **superlock brakes**, **electronic stability control**, and **drivers side airbag**."))
  (resolve-rule-existence-num env 0
                              (tags-for-rule-string :RuleExistenceNum "The **Car** must have 2 of **antilock brakes**, **electronic stability control**, and **drivers side airbag**.")))

(defn resolve-forward-declaration
  [env ctx [_tag & data-accesses]]
  (let [check-declaration (fn [acc x]
                            (if
                             (nil? (resolve-clause-vec env ctx x))
                             (do (false-with-env-error env ctx x) acc)
                             acc))]
    (reduce check-declaration env data-accesses)))

(comment
  (tags-for-rule-string :Definitions "## Definitions
                                          **Car** _is defined as a vehicle with 4 wheels._
                                          **Hello** _hello_")
  (tags-for-rule-string :Definitions "## Definitions A **Car** _is defined as a vehnicle with 4 wheels._")
  (resolve-forward-declaration env {}
                               (tags-for-rule-string :Definitions "## Definitions
                                          **Car**_is defined as a vehicle with 4 wheels._
                                          **Hello** ")))
(defn upsplice-data-access
  [top-access bottom-access]
  (vec (concat [:DataAccess]
               (rest top-access)
               (rest bottom-access))))

(defn resolve-nested-forward-declaration
  [env ctx [_tag top-access & rest]]
  ;; top-access is a data access representing a map,
  ;; and rest is a seq of data accesses representing keys of that map.
  ;; for each of rest, make a data access with top-access at the front (using concat)
  ;; then feed it into resolve-forward-declaration.
  (let [data-accesses (map #(upsplice-data-access top-access %) rest)]
    (map #(resolve-clause-vec env ctx %) data-accesses)))
(comment
  (resolve-clause-vec env 0
                      (upsplice-data-access
                       (tags-for-rule-string :DataAccess "**Car**")
                       (tags-for-rule-string :DataAccess "**rear view camera**")))
  (resolve-nested-forward-declaration env 0
                                      (tags-for-rule-string :NestedFwdDeclaration "A **Car** contains:
                                          - **crash rating**
                                          - **rear view camera**"))
  (resolve-nested-forward-declaration env 0
                                      [:NestedFwdDeclaration
                                       (tags-for-rule-string :DataAccess "**Car**")
                                       (tags-for-rule-string :DataAccess "**crash rating**")
                                       (tags-for-rule-string :DataAccess "**rear view camera**")]))

(defn resolve-clause-num
  [env ctx [_tag int-clause]]
  (resolve-clause-vec env ctx int-clause))

(defn resolve-numbered-rule-or-definition-clause
  [env ctx [_tag clause-num-vec & rest]]
  (let [new-ctx (assoc ctx :clause-num (resolve-clause-vec env nil clause-num-vec))]
    (map #(resolve-clause-vec env new-ctx %) rest)))

(defn resolve-numbered-definition
  [env ctx vec]
  (resolve-numbered-rule-or-definition-clause env (assoc ctx :section :definitions) vec))

(defn resolve-numbered-rule
  [env ctx vec]
  (resolve-numbered-rule-or-definition-clause env (assoc ctx :section :rules) vec))

(defn resolve-regulation
  [env ctx [_tag & rest]]
  (->> rest
       (map #(resolve-clause-vec env ctx %))
       doall))

(comment
 (reset-env)
	(parse-str target-reg-text)
  (resolve-clause-vec env {} (parse-str target-reg-text))
  @env
  (tags-for-rule-string :RuleClause "  4. _Additionally, the Province requires that cars perform as expected in crashes. There are two measures, both of which must be met._ The **Car**'s **crash rating** must be greater than 3. Also, the **Car** must have a **driver side airbg**.")
  )

(defn resolve-clause-vec
  "The first element of the clause vec is a keyword denoting its parse rule.
  Every parse rule corresponds to a resolve-[clause-name] fn."
  [env ctx clause]
  ((match (first clause)
  	 :Regulation           resolve-regulation
     :ForwardDeclaration   resolve-forward-declaration
     :NestedFwdDeclaration resolve-nested-forward-declaration
     :ClauseNum            resolve-clause-num
     :DefinitionClause     resolve-numbered-definition
     :RuleClause           resolve-numbered-rule
     :Symbol               resolve-symbol
     :Integer              resolve-integer
     :NumComparison        resolve-num-comparison
     :DataAccess           resolve-data-access
     :RuleNumComparison    resolve-rule-num-comparison
     :RuleExistence        resolve-rule-existence
     :RuleExistenceNum     resolve-rule-existence-num)
   env ctx clause))
(comment
  (resolve-clause-vec env {} [:Symbol "**Car**"])
  (resolve-clause-vec env {} (tags-for-rule-string :DataAccess "**Car**"))
  (resolve-clause-vec env {} (tags-for-rule-string :DataAccess "**Car**'s **second row**")))
