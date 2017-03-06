(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p1() Bool)
(declare-fun p4() Bool)
(assert
(=> (or (or p1 p1) (and false false false) (and false p1)) (and (or p1 true false) (=> true p4) (= false true)))
)
(check-sat)
(get-model)
