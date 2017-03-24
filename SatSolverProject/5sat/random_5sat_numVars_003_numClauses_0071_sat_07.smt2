(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p2() Bool)
(declare-fun p1() Bool)
(declare-fun p0() Bool)
(assert
(and (or p2 (not p1) (not p2) (not p2) (not p2)) (or (not p2) p1 p0 (not p1) (not p2)) (or (not p0) (not p2) (not p0) p1 p1) (or p0 (not p0) p2 (not p1) p0) (or p1 (not p1) p1 p1 p2) (or p1 p2 p0 p1 p0) (or (not p0) p0 (not p2) p1 p2) (or p0 p1 p0 (not p0) (not p2)) (or p0 p0 (not p0) (not p1) (not p1)) (or p1 p0 (not p1) (not p0) p2) (or p2 (not p2) (not p0) (not p2) (not p0)) (or (not p1) p0 p2 (not p2) (not p0)) (or p2 p1 (not p2) p0 p2) (or p0 (not p2) (not p2) p2 p0) (or p1 p1 (not p1) p2 p0) (or p1 p1 p2 p1 p2) (or (not p0) p1 (not p0) (not p1) p0) (or (not p2) p1 (not p1) p2 p0) (or (not p2) p0 (not p2) p2 p1) (or p0 p1 (not p0) (not p1) (not p2)) (or p1 (not p2) p2 (not p0) p2) (or (not p2) p1 p1 (not p2) (not p0)) (or p0 p2 (not p0) (not p1) p1) (or p1 (not p0) p1 p0 p1) (or (not p1) (not p0) (not p0) (not p2) (not p0)) (or (not p0) (not p0) p1 (not p2) p2) (or p2 (not p0) (not p1) p0 (not p1)) (or (not p1) (not p1) (not p1) p1 p1) (or (not p0) (not p1) p2 (not p1) p0) (or (not p1) p0 (not p0) (not p1) p0) (or p0 p1 p1 (not p2) (not p0)) (or p2 (not p1) (not p0) p1 p1) (or (not p1) (not p2) p1 (not p1) (not p0)) (or p1 p1 (not p1) (not p1) (not p0)) (or p0 (not p1) (not p1) (not p2) p0) (or p0 p1 (not p2) p1 (not p0)) (or (not p1) (not p0) (not p2) (not p1) p2) (or p0 (not p1) p1 (not p1) p1) (or p2 (not p1) p0 (not p1) (not p2)) (or (not p2) (not p1) (not p1) (not p0) p1) (or p0 (not p0) (not p0) (not p1) p2) (or (not p2) (not p0) (not p0) p2 p0) (or p2 p1 p1 (not p2) p1) (or p0 (not p1) (not p2) p0 (not p2)) (or (not p2) (not p0) p1 p1 p0) (or (not p2) p1 (not p0) (not p2) p0) (or p1 p0 p0 p0 (not p1)) (or p1 (not p1) p2 (not p1) (not p0)) (or (not p0) p2 (not p2) (not p2) (not p0)) (or p0 p2 p1 p0 p2) (or p0 p0 p1 (not p1) (not p0)) (or p2 (not p2) (not p2) (not p2) p0) (or p1 (not p1) (not p1) p0 p1) (or p0 (not p2) (not p2) (not p2) p0) (or p1 p1 (not p0) p2 (not p0)) (or p0 (not p1) (not p2) p1 (not p2)) (or (not p0) p1 (not p2) p1 p2) (or p0 (not p2) p2 (not p0) (not p2)) (or p1 (not p0) (not p1) p2 (not p2)) (or (not p2) p0 (not p1) (not p2) (not p2)) (or p0 (not p0) (not p0) (not p2) p0) (or (not p2) (not p0) p1 p2 p1) (or (not p1) (not p1) (not p0) p0 p0) (or p1 p2 p1 p1 (not p1)) (or (not p2) (not p0) p1 (not p0) p0) (or (not p0) p0 (not p0) (not p1) (not p2)) (or p1 (not p1) p0 p2 (not p2)) (or p1 p2 p0 p1 (not p1)) (or p0 p0 p0 (not p2) (not p0)) (or p2 (not p0) p2 p2 p0) (or p0 (not p2) p1 p0 p2))
)
(check-sat)
(get-model)
