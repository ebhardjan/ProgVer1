(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p2() Bool)
(declare-fun p3() Bool)
(declare-fun p4() Bool)
(declare-fun p1() Bool)
(assert
(and (= (= p2 p3) (and p4 p1 p2)) (or (or p1 false true) (= p3 p4) (=> false p3)))
)
(check-sat)
(get-model)
