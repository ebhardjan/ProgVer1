(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p2() Bool)
(declare-fun p3() Bool)
(assert
(not (=> (or p0 p2) (and p3 true p0)))
)
(check-sat)
(get-model)