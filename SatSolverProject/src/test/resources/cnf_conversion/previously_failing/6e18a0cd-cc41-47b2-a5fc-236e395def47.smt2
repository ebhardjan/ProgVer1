(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p2() Bool)
(declare-fun p1() Bool)
(assert
(not (and (not false) (= p2 p1) (or true false)))
)
(check-sat)
(get-model)
