(set-option :produce-models true)
(set-logic QF_UF)

(declare-fun p33() Bool)
(declare-fun p43() Bool)
(declare-fun p51() Bool)
(declare-fun p74() Bool)

(assert
(and
  (or (not p43) (not p23) p51)
  (or (not p43) p23 (not p51))
  (or p74 p23 )
  (or p33 (not p51))
  (or p43 (not p74) (not p33) (not p51) (not p23))
))
(check-sat)
(get-model)
