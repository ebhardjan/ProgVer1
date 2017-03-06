(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p1() Bool)
(declare-fun p4() Bool)
(declare-fun p0() Bool)
(declare-fun p2() Bool)
(declare-fun p3() Bool)
(assert
(or (not (and p1 p4 true)) (=> (or p0 p2 p3) (and p1 true false)))
)
(check-sat)
(get-model)
