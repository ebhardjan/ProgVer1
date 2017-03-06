(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p4() Bool)
(declare-fun p0() Bool)
(declare-fun p1() Bool)
(declare-fun p2() Bool)
(declare-fun p3() Bool)
(assert
(= (and (=> p4 p0) (and p1 false)) (or (=> p2 p3) (not p3) (not p4)))
)
(check-sat)
(get-model)