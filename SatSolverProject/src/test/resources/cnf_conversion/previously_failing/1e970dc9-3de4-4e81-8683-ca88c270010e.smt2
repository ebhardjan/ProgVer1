(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p3() Bool)
(declare-fun p1() Bool)
(declare-fun p4() Bool)
(assert
(not (and (=> false p3) (and true p1 p3) (=> true p4)))
)
(check-sat)
(get-model)
