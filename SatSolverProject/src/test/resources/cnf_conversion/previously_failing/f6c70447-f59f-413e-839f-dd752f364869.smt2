(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p4() Bool)
(declare-fun p2() Bool)
(declare-fun p1() Bool)
(assert
(and (= (and p4 p4 true) (and p2 true true)) (=> (and p1 false p2) (=> p4 false)))
)
(check-sat)
(get-model)
