(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p8() Bool)
(declare-fun p3() Bool)
(declare-fun p6() Bool)
(declare-fun p15() Bool)
(declare-fun p4() Bool)
(declare-fun p17() Bool)
(declare-fun p11() Bool)
(declare-fun p7() Bool)
(declare-fun p1() Bool)
(declare-fun p19() Bool)
(declare-fun p10() Bool)
(declare-fun p9() Bool)
(declare-fun p14() Bool)
(declare-fun p16() Bool)
(declare-fun p18() Bool)
(declare-fun p5() Bool)
(declare-fun p0() Bool)
(declare-fun p13() Bool)
(declare-fun p12() Bool)
(declare-fun p2() Bool)
(assert
(and (or p8 (not p3) p6) (or (not p15) p4 p17) (or p11 (not p17) p7) (or (not p4) p8 (not p1)) (or (not p17) (not p7) (not p19)) (or (not p11) p11 p15) (or (not p10) p3 (not p9)) (or p14 (not p16) p18) (or (not p5) (not p19) (not p17)) (or p4 (not p5) (not p16)) (or (not p11) (not p3) (not p14)) (or p8 (not p19) p11) (or p4 p8 (not p14)) (or (not p14) (not p17) (not p11)) (or (not p5) p0 (not p18)) (or p7 p17 (not p16)) (or p13 (not p0) (not p0)) (or (not p14) (not p10) p18) (or (not p13) (not p17) (not p3)) (or (not p8) (not p13) p1) (or (not p18) (not p3) p19) (or (not p14) (not p5) p14) (or p6 p13 (not p8)) (or (not p8) (not p19) p9) (or (not p12) p7 p11) (or p1 (not p18) p19) (or (not p1) (not p14) p16) (or p6 p16 (not p14)) (or p5 p12 p15) (or (not p0) (not p12) (not p17)) (or p3 (not p8) p3) (or p5 p9 (not p1)) (or (not p3) p5 (not p13)) (or (not p3) p11 (not p0)) (or p9 p12 (not p1)) (or (not p1) (not p13) p0) (or p16 p19 p12) (or p2 (not p4) (not p16)) (or (not p6) p17 (not p18)) (or p3 (not p16) (not p2)) (or (not p5) p2 p0) (or (not p11) p9 p16) (or (not p5) p13 p14) (or p6 (not p15) (not p7)) (or p16 (not p14) (not p2)) (or (not p14) p19 (not p1)) (or p13 (not p1) (not p7)) (or (not p6) p2 p5) (or p3 p9 p13) (or (not p9) (not p15) (not p3)) (or p13 p7 (not p16)) (or p3 p18 (not p19)) (or (not p3) p6 p1) (or (not p2) p0 p0) (or (not p12) p6 p12) (or p19 (not p9) p5) (or (not p1) (not p1) (not p10)) (or (not p15) (not p4) p2) (or (not p11) p2 p12) (or (not p12) p12 p2) (or (not p0) p6 p9) (or (not p11) p16 p14) (or (not p7) (not p13) p10) (or (not p13) (not p7) (not p16)) (or (not p7) p3 (not p11)) (or (not p7) p6 (not p16)) (or p19 (not p17) (not p4)) (or (not p8) (not p8) (not p2)) (or p14 p12 (not p10)) (or (not p5) p17 (not p2)) (or p17 (not p19) (not p17)) (or p13 (not p14) p8) (or p14 (not p6) (not p14)) (or (not p2) p7 p0) (or p5 (not p13) (not p3)) (or (not p19) p2 p13) (or p9 (not p16) p5) (or (not p12) p3 p18) (or (not p3) p5 p18) (or (not p5) p19 p14) (or p9 (not p7) (not p8)) (or p6 p6 p0) (or (not p11) p3 (not p4)) (or p9 (not p19) p18) (or p8 (not p16) p10) (or p3 (not p1) (not p0)) (or p16 (not p13) (not p14)))
)
(check-sat)
(get-model)
