(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p5() Bool)
(declare-fun p6() Bool)
(declare-fun p2() Bool)
(declare-fun p11() Bool)
(declare-fun p9() Bool)
(declare-fun p8() Bool)
(declare-fun p3() Bool)
(declare-fun p7() Bool)
(declare-fun p4() Bool)
(declare-fun p10() Bool)
(declare-fun p1() Bool)
(declare-fun p0() Bool)
(declare-fun p13() Bool)
(declare-fun p12() Bool)
(declare-fun p14() Bool)
(assert
(and (or (not p5) (not p6) p2) (or p2 (not p11) (not p9)) (or p9 p8 p6) (or p3 p7 (not p5)) (or (not p3) (not p6) (not p8)) (or p11 (not p4) (not p4)) (or p10 (not p2) p8) (or p4 (not p8) (not p1)) (or p0 (not p11) (not p8)) (or (not p9) (not p9) p3) (or (not p6) (not p10) p1) (or p3 (not p1) (not p5)) (or (not p4) p6 p5) (or (not p4) (not p0) p10) (or (not p1) (not p0) p11) (or p4 p8 p10) (or p8 (not p13) p9) (or (not p8) (not p0) p12) (or (not p6) p10 (not p9)) (or (not p2) (not p3) (not p0)) (or (not p0) (not p14) (not p12)) (or p4 p3 p10) (or (not p8) p4 p0) (or p5 p7 p1) (or p10 p4 (not p14)) (or (not p5) (not p2) (not p13)) (or (not p12) (not p6) (not p8)) (or p14 (not p10) p0) (or (not p7) (not p4) (not p1)) (or (not p1) p2 (not p12)) (or p8 (not p3) (not p5)) (or p9 p1 (not p7)) (or (not p1) p2 p3) (or (not p2) (not p1) p6) (or p10 (not p8) p0) (or p9 (not p10) p4) (or p0 (not p8) (not p8)) (or (not p2) p0 p14) (or (not p0) (not p7) p1) (or p6 (not p6) p2) (or p1 (not p2) (not p13)) (or (not p14) (not p1) (not p10)) (or (not p6) (not p7) p9) (or (not p12) (not p13) p11) (or (not p1) p3 p7) (or p5 (not p8) p7) (or (not p3) p5 (not p7)) (or (not p9) (not p7) p6) (or (not p9) (not p3) (not p1)) (or (not p10) p13 p5) (or (not p11) p14 (not p10)) (or p6 (not p6) (not p4)) (or (not p13) (not p5) (not p14)) (or (not p14) p14 (not p4)) (or (not p4) p9 (not p11)) (or (not p6) p10 p6) (or p1 (not p2) p12) (or (not p0) p11 p3) (or p14 (not p6) p7) (or (not p10) p13 (not p1)) (or (not p8) p9 p4) (or (not p6) p9 p2) (or (not p3) (not p14) p8) (or (not p3) p8 (not p10)) (or p8 (not p7) p14) (or (not p1) p1 p6) (or p11 (not p11) (not p0)) (or (not p0) p4 (not p12)) (or (not p10) (not p7) (not p5)))
)
(check-sat)
(get-model)