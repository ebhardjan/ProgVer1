(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p1() Bool)
(declare-fun p4() Bool)
(assert
(=> (=> (=> p0 false) (not true)) (= (= true p1) (or p4 p1 p0)))
)
(check-sat)
(get-model)