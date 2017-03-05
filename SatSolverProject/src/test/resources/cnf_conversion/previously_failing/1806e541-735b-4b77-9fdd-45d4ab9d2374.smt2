(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p1() Bool)
(declare-fun p2() Bool)
(assert
(not (and (=> p1 false) (or true false p2) (not false)))
)
(check-sat)
(get-model)
