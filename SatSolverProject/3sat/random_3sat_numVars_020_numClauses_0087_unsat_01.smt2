(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p5() Bool)
(declare-fun p9() Bool)
(declare-fun p14() Bool)
(declare-fun p1() Bool)
(declare-fun p7() Bool)
(declare-fun p13() Bool)
(declare-fun p8() Bool)
(declare-fun p6() Bool)
(declare-fun p16() Bool)
(declare-fun p10() Bool)
(declare-fun p3() Bool)
(declare-fun p4() Bool)
(declare-fun p19() Bool)
(declare-fun p17() Bool)
(declare-fun p15() Bool)
(declare-fun p11() Bool)
(declare-fun p18() Bool)
(declare-fun p12() Bool)
(declare-fun p2() Bool)
(declare-fun p0() Bool)
(assert
(and (or (not p5) (not p9) p14) (or (not p1) (not p7) (not p13)) (or p5 p5 p8) (or p13 p6 (not p16)) (or p10 p3 (not p4)) (or (not p19) (not p17) (not p10)) (or p16 p15 (not p4)) (or (not p13) p11 p17) (or (not p13) (not p10) (not p18)) (or p10 (not p6) p17) (or (not p13) p11 p6) (or (not p9) (not p13) p5) (or (not p18) p17 p19) (or p8 p18 (not p10)) (or (not p19) (not p16) p13) (or (not p17) (not p3) p12) (or (not p2) (not p8) (not p8)) (or p0 p17 (not p13)) (or (not p4) (not p15) p4) (or (not p5) p8 p9) (or p7 p10 (not p6)) (or p14 p8 (not p4)) (or (not p13) (not p9) p17) (or (not p14) p10 p12) (or (not p10) (not p1) p0) (or (not p4) p0 (not p9)) (or (not p8) p7 p3) (or (not p19) (not p18) (not p18)) (or p1 (not p3) p6) (or p11 (not p16) (not p10)) (or p12 (not p6) p6) (or (not p18) p3 (not p6)) (or (not p15) p15 (not p16)) (or (not p17) (not p5) p18) (or (not p15) (not p14) p13) (or (not p5) p2 p13) (or (not p12) (not p12) p2) (or (not p16) p9 (not p1)) (or p8 (not p4) (not p15)) (or (not p6) (not p14) p6) (or (not p3) p3 p12) (or (not p16) (not p13) (not p8)) (or p12 (not p4) p11) (or (not p13) p13 (not p17)) (or (not p4) (not p12) (not p18)) (or (not p10) (not p16) p4) (or (not p8) p17 p10) (or p12 (not p0) p17) (or p4 (not p6) p14) (or (not p6) (not p13) p5) (or p14 p16 (not p3)) (or p17 p14 (not p8)) (or (not p11) (not p12) (not p14)) (or (not p11) p12 (not p14)) (or (not p8) (not p0) p6) (or (not p8) (not p16) (not p2)) (or p15 (not p3) (not p14)) (or (not p5) (not p6) p0) (or p18 p0 p18) (or p0 p6 p18) (or (not p7) (not p3) (not p6)) (or p16 p3 p17) (or p12 p0 p13) (or (not p9) (not p5) (not p2)) (or p7 (not p2) (not p5)) (or p0 p14 p12) (or p0 (not p18) p13) (or p3 p18 p14) (or p12 (not p14) (not p14)) (or (not p18) p12 p10) (or (not p18) (not p17) (not p5)) (or p18 (not p7) (not p17)) (or (not p2) (not p12) p1) (or p3 (not p5) (not p7)) (or p5 (not p4) (not p19)) (or (not p13) (not p1) (not p12)) (or (not p10) (not p6) (not p2)) (or p14 p19 (not p10)) (or p2 p12 p5) (or (not p1) (not p14) (not p4)) (or (not p1) p8 p1) (or p16 (not p1) (not p19)) (or (not p16) p11 p15) (or (not p13) (not p15) (not p6)) (or p6 p12 p13) (or p4 (not p1) p1) (or (not p4) (not p17) (not p12)))
)
(check-sat)
(get-model)