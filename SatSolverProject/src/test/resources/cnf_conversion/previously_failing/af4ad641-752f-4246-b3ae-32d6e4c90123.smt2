(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p3() Bool)
(assert
(not (=> (or true false) (or p3 false p3)))
)
(check-sat)
(get-model)
