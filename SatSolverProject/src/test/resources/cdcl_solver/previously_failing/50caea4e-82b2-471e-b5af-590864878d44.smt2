(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p3() Bool)
(declare-fun p0() Bool)
(declare-fun p4() Bool)
(declare-fun p2() Bool)
(declare-fun p5() Bool)
(declare-fun p1() Bool)
(assert
(and (or p4 p1 p2) (or (not p4) (not p0) (not p1)) (or (not p3) p0) (or p4 p0 (not p5)) (or (not p0) (not p2) p1) (or p4 p1 (not p3) p4) (or p4 (not p1) (not p2)) (or p5 p3) (or (not p5) (not p4) (not p4))))
(check-sat)
(get-model)
