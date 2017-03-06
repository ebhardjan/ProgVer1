(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p4() Bool)
(assert
(= (and (=> p0 p4) (not false) (or p0 true false)) (= (or true false true) (= false p0)))
)
(check-sat)
(get-model)
