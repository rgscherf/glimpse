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
                 :crash-rating                 5
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

(defn add-error-to-env
  [env clause-num clause-vec]
  (swap! env assoc :errors (cons (str "Failed to meet condition in clause #" clause-num " with clause vector " clause-vec)
                                 (:errors @env)))
  false)


(defn resolve-symbol
  [_env _clause-num [_symbol-key keystr]]
  (-> keystr
      (str/replace "**" "")
      (str/replace " " "-")
      str/lower-case
      keyword))
(comment
  (resolve-symbol env 0 [:Symbol "**Car**"]))

(defn resolve-data-access
  [env clause-num [_tag head & rest]]
  (let [envmap (:data @env)]
    (if (empty? rest)
      (get envmap (resolve-clause-vec env clause-num head))
      (get-in envmap (map #(resolve-clause-vec env clause-num %) (cons head rest))))))
(comment
  (resolve-data-access env 0 (tags-for-rule-string :DataAccess "**Car**"))
  (resolve-data-access env 0 (tags-for-rule-string :DataAccess "**Car**'s **second row**"))
  (tags-for-rule-string :DataAccess "**Car**'s **second row**"))

(defn resolve-rule-existence
  [env clause-num [_tag data-access search-key]]
  (get (resolve-clause-vec env clause-num data-access)
       (resolve-clause-vec env clause-num search-key)))
(comment
  (resolve-rule-existence env 0 [:RuleExistence [:DataAccess [:Symbol "**Car**"] [:Symbol "**second row**"]] [:Symbol "**airbags**"]])
  (resolve-rule-existence env 0 (tags-for-rule-string :RuleExistence "The **Car** must have **antilock brakes**."))
  (resolve-rule-existence env 0 (tags-for-rule-string :RuleExistence "The **Car**'s **second row** must have **airbags**.")))

(defn resolve-num-comparison
  [_env _clause-num [_tag [comparison]]]
  (match comparison
    :NCLT <
    :NCEq =
    :NCGT >))

(defn resolve-integer
  [_env _clause-num [_ int-str]]
  (parse-long int-str))

(defn resolve-rule-num-comparison
  "Numeric comparison of two values, the first being data."
  [env clause-num [_ data-access comparison num]]
  ((resolve-clause-vec env clause-num comparison)
   (resolve-clause-vec env clause-num data-access)
   (resolve-clause-vec env clause-num num)))
(comment
  (tags-for-rule-string :RuleNumComparison "The **Car**'s **crash rating** must be greater than 3.")
  (resolve-rule-num-comparison env 0 (tags-for-rule-string :RuleNumComparison "The **Car**'s **crash rating** must be greater than 3.")))

(defn truthy?
  [x]
  (not (or (nil? x) (false? x))))
(comment
  (truthy? 3)
  (truthy? nil)
  (truthy? false))

(defn resolve-rule-existence-num
  [env clause-num [_tag data-access num & access-key-symbols :as this-vec]]
  (let [data-to-access (resolve-data-access env clause-num data-access)
        result (->> access-key-symbols
                    (map #(resolve-symbol env clause-num %))
                    (map #(get data-to-access %))
                    (filter truthy?)
                    count
                    (<= (resolve-integer env clause-num num)))]
    (if result
      true
      (add-error-to-env env clause-num this-vec))))
(comment
	(reset-env)
  @env
  (resolve-rule-existence-num env 0
                              (tags-for-rule-string :RuleExistenceNum "The **Car** must have 2 of **superlock brakes**, **electronic stability control**, and **drivers side airbag**."))
  (resolve-rule-existence-num env 0
                              (tags-for-rule-string :RuleExistenceNum "The **Car** must have 2 of **antilock brakes**, **electronic stability control**, and **drivers side airbag**.")))

(defn forward-declaration-error-string
  [clause-num data-access]
  (str "Forward declaration at clause number " clause-num " could not be found in the input. Here was the data access vec: " data-access))

(defn resolve-forward-declaration
  [env clause-num [_tag & data-accesses]]
  (let [check-declaration (fn [acc x]
                            (if
                             (nil? (resolve-clause-vec env clause-num x))
                              (swap! acc assoc :errors
                                     (cons (forward-declaration-error-string clause-num x)
                                           (:errors @acc)))
                              acc))]
    (reduce check-declaration env data-accesses)))

(comment
  (tags-for-rule-string :Definitions "## Definitions
                                          **Car** _is defined as a vehicle with 4 wheels._
                                          **Hello** _hello_")
  (tags-for-rule-string :Definitions "## Definitions A **Car** _is defined as a vehnicle with 4 wheels._")
  (resolve-forward-declaration env 0
                               (tags-for-rule-string :Definitions "## Definitions
                                          **Car**_is defined as a vehicle with 4 wheels._
                                          **Hello** ")))
(defn upsplice-data-access
  [top-access bottom-access]
  (vec (concat [:DataAccess]
               (rest top-access)
               (rest bottom-access))))

(defn resolve-nested-forward-declaration
  [env clause-num [_tag top-access & rest]]
  ;; top-access is a data access representing a map,
  ;; and rest is a seq of data accesses representing keys of that map.
  ;; for each of rest, make a data access with top-access at the front (using concat)
  ;; then feed it into resolve-forward-declaration.
  (let [data-accesses (map #(upsplice-data-access top-access %) rest)]
    (map #(resolve-clause-vec env clause-num %) data-accesses)))
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
  [env _clause-num [_tag int-clause]]
  (resolve-clause-vec env nil int-clause))

(defn resolve-numbered-rule-or-definition-clause
  [env _nil-clause-num [_tag clause-num-vec & rest]]
  (let [resolved-clause-num (resolve-clause-vec env nil clause-num-vec)]
    (map #(resolve-clause-vec env resolved-clause-num %) rest)
    ))

(defn resolve-regulation
  [env _nil-clause-num [_tag & rest]]
  (->> rest
       (map #(resolve-clause-vec env nil %))
       doall))

(comment
	(parse-str target-reg-text)
  (resolve-clause-vec env nil (parse-str target-reg-text))
  )

(defn resolve-clause-vec
  "The first element of the clause vec is a keyword denoting its parse rule.
  Every parse rule corresponds to a resolve-[clause-name] fn."
  [env clause-num? clause]
  ((match (first clause)
  	 :Regulation           resolve-regulation
     :ForwardDeclaration   resolve-forward-declaration
     :NestedFwdDeclaration resolve-nested-forward-declaration
     :ClauseNum            resolve-clause-num
     :DefinitionClause     resolve-numbered-rule-or-definition-clause
     :RuleClause           resolve-numbered-rule-or-definition-clause
     :Symbol               resolve-symbol
     :Integer              resolve-integer
     :NumComparison        resolve-num-comparison
     :DataAccess           resolve-data-access
     :RuleNumComparison    resolve-rule-num-comparison
     :RuleExistence        resolve-rule-existence
     :RuleExistenceNum     resolve-rule-existence-num)
   env clause-num? clause))
(comment
  (resolve-clause-vec env 0 [:Symbol "**Car**"])
  (resolve-clause-vec env 0 (tags-for-rule-string :DataAccess "**Car**"))
  (resolve-clause-vec env 0 (tags-for-rule-string :DataAccess "**Car**'s **second row**")))
