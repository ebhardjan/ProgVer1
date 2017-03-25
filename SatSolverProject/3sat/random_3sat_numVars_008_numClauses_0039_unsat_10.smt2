(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p1() Bool)
(declare-fun p6() Bool)
(declare-fun p0() Bool)
(declare-fun p4() Bool)
(declare-fun p2() Bool)
(declare-fun p3() Bool)
(declare-fun p5() Bool)
(declare-fun p7() Bool)
(assert
(and (or p1 p6 p0) (or (not p4) (not p1) p1) (or p1 (not p2) (not p0)) (or p2 (not p3) p0) (or (not p6) (not p1) p4) (or p3 (not p4) (not p4)) (or (not p0) p5 (not p6)) (or p3 (not p3) p3) (or p4 p4 p6) (or p7 p6 p3) (or (not p7) (not p4) (not p7)) (or p1 p5 p2) (or (not p7) (not p6) (not p3)) (or p3 p3 (not p5)) (or (not p3) (not p3) p7) (or p2 p3 p4) (or (not p3) (not p2) (not p6)) (or p7 (not p4) p4) (or p5 (not p7) p5) (or p1 (not p2) (not p7)) (or p2 (not p6) (not p3)) (or (not p3) p6 (not p5)) (or (not p1) (not p6) p6) (or (not p4) p4 (not p1)) (or (not p2) (not p4) p0) (or (not p7) p6 p4) (or (not p6) p7 (not p7)) (or (not p0) (not p6) p4) (or (not p7) (not p7) (not p6)) (or (not p3) (not p4) p7) (or (not p3) p2 p6) (or (not p7) (not p1) p1) (or p7 (not p6) (not p6)) (or (not p4) (not p3) p1) (or (not p1) p6 (not p7)) (or (not p6) (not p0) (not p5)) (or (not p3) p0 (not p6)) (or (not p6) p0 (not p1)) (or p5 (not p2) p7))
)
(check-sat)
(get-model)
