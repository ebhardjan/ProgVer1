(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p4() Bool)
(declare-fun p1() Bool)
(declare-fun p3() Bool)
(declare-fun p2() Bool)
(assert
(and (=> (or p0 p4 p0) (=> false false)) (and (or false true) (and true true) (or true p1 p3)) (not (or p0 p2 p3)))
)
(check-sat)
(get-model)
