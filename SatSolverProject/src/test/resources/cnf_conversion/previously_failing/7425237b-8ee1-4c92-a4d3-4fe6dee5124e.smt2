(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p2() Bool)
(assert
(not (or (not true) (or p0 false p2)))
)
(check-sat)
(get-model)
