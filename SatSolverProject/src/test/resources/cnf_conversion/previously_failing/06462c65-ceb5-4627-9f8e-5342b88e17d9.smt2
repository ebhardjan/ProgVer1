(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p3() Bool)
(declare-fun p1() Bool)
(assert
(= (or (and false false) (= p3 p1) (not true)) (=> (=> false true) (=> p1 p1)))
)
(check-sat)
(get-model)
