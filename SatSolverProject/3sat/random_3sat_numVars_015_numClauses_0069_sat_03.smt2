(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p11() Bool)
(declare-fun p7() Bool)
(declare-fun p1() Bool)
(declare-fun p0() Bool)
(declare-fun p12() Bool)
(declare-fun p2() Bool)
(declare-fun p6() Bool)
(declare-fun p9() Bool)
(declare-fun p10() Bool)
(declare-fun p5() Bool)
(declare-fun p13() Bool)
(declare-fun p8() Bool)
(declare-fun p3() Bool)
(declare-fun p14() Bool)
(declare-fun p4() Bool)
(assert
(and (or (not p11) (not p7) p1) (or (not p0) (not p11) (not p12)) (or p12 (not p2) p6) (or (not p2) (not p1) (not p9)) (or (not p6) (not p10) p5) (or p13 (not p10) (not p0)) (or (not p5) (not p8) p1) (or p8 (not p6) p2) (or (not p13) (not p3) p1) (or p7 p2 p14) (or p12 p9 p2) (or p12 (not p1) p3) (or (not p2) p14 p8) (or (not p4) (not p4) p9) (or (not p8) (not p0) p9) (or (not p4) p6 (not p10)) (or p3 (not p7) p3) (or (not p14) p11 (not p5)) (or p3 (not p1) p0) (or p6 p5 p12) (or p7 p5 p7) (or (not p0) p8 p2) (or p9 p13 p3) (or (not p3) (not p10) p6) (or p1 p8 p8) (or (not p10) p8 p5) (or (not p8) p5 p13) (or p13 p4 p0) (or (not p9) p3 p4) (or p11 (not p12) (not p12)) (or (not p3) (not p9) p6) (or (not p6) p1 p4) (or (not p1) p5 p10) (or p8 p3 p11) (or p11 (not p9) (not p3)) (or (not p10) p0 (not p13)) (or (not p12) (not p11) (not p2)) (or (not p14) p7 p11) (or p12 p14 p3) (or (not p11) (not p10) p4) (or p0 p3 p2) (or (not p8) (not p3) (not p14)) (or p6 p9 (not p13)) (or p8 p2 (not p12)) (or p13 p1 (not p13)) (or (not p11) p13 (not p4)) (or p9 p5 p6) (or (not p7) (not p12) (not p2)) (or p1 p1 p9) (or p13 (not p0) p1) (or (not p3) p3 p5) (or p8 (not p9) (not p14)) (or p10 p12 p11) (or p10 (not p14) p14) (or (not p4) (not p10) (not p3)) (or (not p1) p14 p5) (or (not p2) (not p7) p13) (or p3 p3 p5) (or p8 p0 (not p8)) (or (not p12) p9 p0) (or (not p6) p2 (not p13)) (or (not p7) (not p8) (not p11)) (or (not p13) (not p9) (not p9)) (or (not p11) p2 p9) (or (not p5) (not p9) (not p3)) (or (not p8) p6 (not p7)) (or (not p0) p4 p5) (or (not p1) (not p4) (not p5)) (or (not p3) (not p10) p0))
)
(check-sat)
(get-model)
