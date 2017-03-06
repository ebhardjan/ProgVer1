(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p4() Bool)
(assert
(= (= (and p0 true p4) (= p4 false)) (not (=> true true)))
)
(check-sat)
(get-model)
