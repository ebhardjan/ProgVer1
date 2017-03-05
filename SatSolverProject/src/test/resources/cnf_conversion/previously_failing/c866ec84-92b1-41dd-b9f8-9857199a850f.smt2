(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p2() Bool)
(declare-fun p0() Bool)
(assert
(= (not (= true true)) (and (= p2 p0) (=> p2 true) (and p2 true p2)))
)
(check-sat)
(get-model)
