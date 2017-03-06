(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p2() Bool)
(declare-fun p4() Bool)
(declare-fun p1() Bool)
(declare-fun p0() Bool)
(assert
(or (= (or false true) (=> p2 p4)) (not (or p4 p1 p0)) (= (not true) (or p1 false)))
)
(check-sat)
(get-model)
