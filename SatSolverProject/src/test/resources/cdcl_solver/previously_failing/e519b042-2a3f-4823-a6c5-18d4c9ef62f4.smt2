(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p3() Bool)
(declare-fun p2() Bool)
(declare-fun p1() Bool)
(declare-fun p0() Bool)
(declare-fun p5() Bool)
(declare-fun p4() Bool)
(assert
(and (or p3 p3 p3 p2) (or p1 (not p0)) (or p3 (not p5)) (or p2 p5 (not p3) p5) (or p2 (not p4)) (or p5 (not p3) (not p2)) (or (not p2) (not p1) (not p0)) (or (not p5) (not p4) p2 p1) (or p5 (not p0)) (or (not p0) (not p2) (not p2)) (or (not p0) (not p0) p2 p5) (or (not p0) (not p3) p5 p4) (or (not p3) p3 (not p5) p4) (or p0 p0 (not p4)) (or (not p3) (not p1) p3 (not p2)) (or (not p2) p4 (not p3)) (or (not p2) p2 p4) (or (not p0) p0 p4 p0) (or (not p3) (not p5) p3) (or p4 (not p1)) (or (not p0) p0 (not p1) p2) (or (not p2) (not p0)) (or p2 p1) (or (not p1) (not p3)) (or p5 p5 p3 (not p4)))
)
(check-sat)
(get-model)
