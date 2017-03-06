(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p3() Bool)
(declare-fun p4() Bool)
(assert
(= (not (=> false false)) (and (or false false p3) (=> true p4) (and true true)))
)
(check-sat)
(get-model)
