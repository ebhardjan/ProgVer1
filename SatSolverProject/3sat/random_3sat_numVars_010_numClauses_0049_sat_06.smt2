(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p5() Bool)
(declare-fun p8() Bool)
(declare-fun p4() Bool)
(declare-fun p1() Bool)
(declare-fun p0() Bool)
(declare-fun p3() Bool)
(declare-fun p9() Bool)
(declare-fun p7() Bool)
(declare-fun p6() Bool)
(declare-fun p2() Bool)
(assert
(and (or p5 p8 p5) (or (not p4) (not p1) p0) (or (not p3) p5 p5) (or (not p9) p8 p7) (or (not p5) p1 p1) (or p7 p8 (not p1)) (or p9 p6 p2) (or (not p5) (not p9) p4) (or p0 (not p3) (not p5)) (or (not p5) p6 p3) (or p9 p1 p0) (or p1 (not p9) (not p8)) (or (not p4) (not p9) p6) (or p9 (not p5) p6) (or (not p1) p1 p2) (or (not p6) (not p1) p5) (or (not p4) (not p5) (not p8)) (or p1 p9 (not p7)) (or p6 p0 (not p5)) (or (not p8) (not p7) (not p6)) (or (not p9) p9 (not p7)) (or p5 p8 p9) (or (not p7) (not p6) p1) (or (not p9) (not p4) p1) (or (not p7) (not p3) p4) (or (not p9) p7 p9) (or p2 (not p3) (not p0)) (or p6 p9 p4) (or p5 p3 p9) (or (not p0) p1 (not p4)) (or (not p8) (not p7) (not p4)) (or p1 (not p8) (not p9)) (or p0 p4 (not p2)) (or p6 (not p1) p0) (or (not p6) (not p2) p5) (or p8 (not p4) (not p0)) (or (not p4) p8 (not p7)) (or p7 p2 p0) (or (not p5) (not p9) p7) (or (not p7) p1 p5) (or p9 p9 (not p6)) (or (not p3) (not p7) p2) (or (not p1) (not p4) p3) (or p3 (not p6) p0) (or p4 (not p3) p1) (or (not p8) (not p1) (not p4)) (or p6 p6 p0) (or (not p2) (not p7) p2) (or (not p7) (not p1) (not p4)))
)
(check-sat)
(get-model)
