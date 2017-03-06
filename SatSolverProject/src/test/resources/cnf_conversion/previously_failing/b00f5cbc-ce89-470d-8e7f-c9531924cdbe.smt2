(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p4() Bool)
(declare-fun p0() Bool)
(assert
(not (= (not false) (or p4 false p0)))
)
(check-sat)
(get-model)
