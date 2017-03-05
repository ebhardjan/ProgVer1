(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p3() Bool)
(assert
(=> (or (and p0 false p3) (and p3 true false) (= false p0)) (= (= false p3) (=> true false)))
)
(check-sat)
(get-model)
