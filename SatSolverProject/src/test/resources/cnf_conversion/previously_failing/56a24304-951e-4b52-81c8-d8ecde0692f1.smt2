(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p4() Bool)
(declare-fun p2() Bool)
(declare-fun p1() Bool)
(declare-fun p3() Bool)
(assert
(= (=> (not p0) (not p4)) (and (=> p2 p0) (=> p2 p1) (= p1 p3)))
)
(check-sat)
(get-model)
