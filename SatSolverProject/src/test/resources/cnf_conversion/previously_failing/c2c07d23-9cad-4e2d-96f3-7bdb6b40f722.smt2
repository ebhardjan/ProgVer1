(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p3() Bool)
(declare-fun p2() Bool)
(assert
(not (and (= true p3) (=> true true) (or false false p2)))
)
(check-sat)
(get-model)
