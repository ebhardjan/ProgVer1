(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p2() Bool)
(declare-fun p3() Bool)
(assert
(not (=> (=> true true) (and p2 p3 true)))
)
(check-sat)
(get-model)
