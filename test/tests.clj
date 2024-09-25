(ns tests
  (:require [core ]))

(def env
  {:errors []
   :car {:driver-side-airbag           true
         :antilock-brakes              true
         :electronic-stability-control true
         :rear-view-camera             true
         :crash-rating                 5
         :second-row                   {:airbags        "I have airbags"
                                        :shoulder_belts false }}})
