(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p2() Bool)
(assert
(= (not (=> p0 true)) (= (or p2 p2 false) (= true true)))
)
(check-sat)
(get-model)
