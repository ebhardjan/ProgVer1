(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p1() Bool)
(declare-fun p3() Bool)
(assert
(=> (or (not true) (or false true)) (not (and p1 p3 true)))
)
(check-sat)
(get-model)
