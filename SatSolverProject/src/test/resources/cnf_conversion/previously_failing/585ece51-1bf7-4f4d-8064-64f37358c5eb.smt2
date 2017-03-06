(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p4() Bool)
(declare-fun p2() Bool)
(assert
(=> (or (=> true false) (or true true) (=> true true)) (= (or p4 p4) (and p2 p4 true)))
)
(check-sat)
(get-model)
