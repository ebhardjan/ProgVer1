(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p4() Bool)
(declare-fun p2() Bool)
(declare-fun p1() Bool)
(declare-fun p3() Bool)
(declare-fun p5() Bool)
(assert
(and (or (not p0) (not p4) (not p2)) (or (not p1) p1) (or p3 p1 p4) (or p1 p4) (or p1 p5 p5 (not p1)) (or p0 (not p2) (not p1)) (or (not p1) (not p5) (not p1) (not p3)) (or p5 p5 (not p5) (not p5)) (or p2 p0 p1 p5) (or (not p5) (not p1) (not p5)) (or p3 p5 (not p5)) (or p2 (not p1) p2) (or p1 p2) (or p0 p1) (or p2 p0) (or p0 p1) (or (not p3) p5) (or p0 p5 (not p4) p1) (or (not p1) (not p4) p4) (or (not p2) (not p5) (not p5)) (or (not p4) (not p1) p5) (or p2 (not p4) (not p2)) (or p0 (not p5) p0 p3) (or (not p5) p5 p1) (or (not p2) (not p1) (not p5)))
)
(check-sat)
(get-model)
