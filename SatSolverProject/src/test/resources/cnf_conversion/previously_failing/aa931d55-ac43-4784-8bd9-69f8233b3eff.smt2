(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p1() Bool)
(declare-fun p4() Bool)
(assert
(=> (not (=> p1 false)) (not (or p4 p1 p4)))
)
(check-sat)
(get-model)
