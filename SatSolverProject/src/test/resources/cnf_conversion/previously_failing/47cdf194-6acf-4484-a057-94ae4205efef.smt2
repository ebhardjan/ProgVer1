(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p2() Bool)
(declare-fun p3() Bool)
(declare-fun p1() Bool)
(declare-fun p4() Bool)
(assert
(=> (=> (=> false p0) (=> p2 p3)) (=> (and p1 p4 p4) (=> p4 p1)))
)
(check-sat)
(get-model)