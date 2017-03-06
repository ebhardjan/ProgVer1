(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p1() Bool)
(assert
(not (and (not false) (=> p0 p0) (not p1)))
)
(check-sat)
(get-model)
