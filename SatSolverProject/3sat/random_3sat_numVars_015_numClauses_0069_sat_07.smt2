(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p8() Bool)
(declare-fun p12() Bool)
(declare-fun p9() Bool)
(declare-fun p3() Bool)
(declare-fun p2() Bool)
(declare-fun p4() Bool)
(declare-fun p13() Bool)
(declare-fun p0() Bool)
(declare-fun p11() Bool)
(declare-fun p1() Bool)
(declare-fun p10() Bool)
(declare-fun p5() Bool)
(declare-fun p7() Bool)
(declare-fun p14() Bool)
(declare-fun p6() Bool)
(assert
(and (or (not p8) (not p12) (not p12)) (or (not p8) p9 p3) (or (not p2) (not p4) p9) (or (not p8) (not p4) (not p3)) (or p13 (not p9) p0) (or (not p9) p11 p1) (or p1 p0 p12) (or (not p13) p0 p2) (or p0 (not p8) (not p10)) (or p8 (not p5) p5) (or (not p4) (not p8) p7) (or p4 (not p5) p10) (or p8 p14 p6) (or p13 p9 (not p0)) (or p2 (not p5) (not p10)) (or p1 p9 (not p8)) (or p9 p11 (not p3)) (or (not p5) p3 p7) (or (not p12) p12 p1) (or p5 (not p3) (not p14)) (or p9 (not p0) (not p9)) (or p14 p12 p11) (or (not p6) p2 p14) (or p7 p5 (not p2)) (or (not p6) (not p4) (not p5)) (or (not p9) p0 p13) (or (not p11) (not p2) (not p8)) (or (not p5) p8 p7) (or p11 p6 p7) (or (not p4) p1 (not p7)) (or (not p10) p5 p13) (or (not p5) (not p5) p14) (or (not p14) (not p6) (not p1)) (or p6 p1 p7) (or (not p7) (not p5) (not p13)) (or p12 (not p3) p7) (or p11 p0 p13) (or (not p12) p12 (not p2)) (or p3 (not p11) (not p14)) (or (not p0) p3 p12) (or (not p8) p5 (not p5)) (or p12 p13 p2) (or p8 p3 (not p5)) (or p13 (not p2) p11) (or (not p2) (not p7) (not p9)) (or p12 (not p13) (not p1)) (or (not p7) p13 p7) (or p2 p10 (not p0)) (or (not p7) (not p8) p1) (or (not p11) (not p7) p11) (or (not p1) p4 p14) (or p2 p7 p12) (or (not p10) (not p10) p10) (or (not p8) p6 (not p2)) (or (not p6) p13 (not p9)) (or (not p0) (not p8) p11) (or p11 p12 (not p11)) (or (not p13) p10 p7) (or p7 p0 (not p7)) (or (not p10) (not p2) p12) (or (not p9) p14 p1) (or p6 p2 (not p5)) (or (not p3) p6 p13) (or p8 (not p4) (not p14)) (or (not p1) (not p5) p3) (or p1 (not p11) (not p6)) (or (not p13) (not p10) p4) (or (not p1) p1 (not p13)) (or (not p4) (not p4) p5))
)
(check-sat)
(get-model)
