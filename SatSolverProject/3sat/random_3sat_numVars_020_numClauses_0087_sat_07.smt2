(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p2() Bool)
(declare-fun p7() Bool)
(declare-fun p15() Bool)
(declare-fun p10() Bool)
(declare-fun p3() Bool)
(declare-fun p16() Bool)
(declare-fun p18() Bool)
(declare-fun p19() Bool)
(declare-fun p8() Bool)
(declare-fun p12() Bool)
(declare-fun p6() Bool)
(declare-fun p17() Bool)
(declare-fun p1() Bool)
(declare-fun p9() Bool)
(declare-fun p13() Bool)
(declare-fun p14() Bool)
(declare-fun p11() Bool)
(declare-fun p4() Bool)
(declare-fun p5() Bool)
(assert
(and (or (not p0) (not p2) (not p7)) (or (not p0) (not p15) (not p2)) (or p10 p10 p3) (or p16 p18 p19) (or p8 p12 (not p8)) (or p18 (not p18) (not p0)) (or (not p19) p3 p2) (or p12 (not p7) (not p8)) (or (not p18) (not p0) (not p6)) (or p17 (not p0) p17) (or p8 (not p8) (not p1)) (or (not p15) (not p10) p9) (or (not p13) p2 (not p13)) (or p18 (not p14) (not p1)) (or p11 p17 p4) (or p5 (not p18) (not p11)) (or (not p0) p10 (not p11)) (or (not p14) (not p13) (not p11)) (or p17 (not p0) (not p1)) (or p10 p4 (not p7)) (or p5 (not p6) p10) (or (not p12) (not p18) (not p0)) (or p17 p9 (not p14)) (or p1 (not p17) p6) (or (not p11) p2 (not p12)) (or p17 (not p14) p12) (or (not p14) (not p7) (not p4)) (or (not p4) p16 (not p4)) (or (not p14) (not p9) (not p13)) (or p2 p0 (not p10)) (or p6 (not p19) (not p19)) (or (not p10) p15 (not p0)) (or p3 p0 (not p1)) (or (not p1) (not p1) (not p14)) (or p7 p0 (not p14)) (or p5 p13 p0) (or p7 (not p19) p16) (or p12 (not p0) (not p14)) (or p12 (not p19) p19) (or (not p18) (not p11) (not p10)) (or (not p4) p12 (not p10)) (or p19 (not p4) p10) (or p2 p15 p6) (or p5 (not p1) (not p6)) (or p18 (not p19) (not p5)) (or p13 (not p8) (not p1)) (or (not p2) (not p8) p19) (or p9 (not p5) (not p12)) (or (not p16) p4 p19) (or (not p10) (not p2) p2) (or (not p11) p15 (not p5)) (or (not p1) p0 p12) (or p8 p0 p17) (or p16 p11 (not p11)) (or p13 (not p1) p15) (or (not p10) p12 p5) (or (not p1) p2 (not p17)) (or (not p0) (not p2) (not p13)) (or (not p4) (not p2) (not p15)) (or (not p15) (not p5) (not p18)) (or (not p3) p15 (not p7)) (or (not p4) p19 p13) (or (not p7) (not p11) p5) (or p19 (not p7) (not p2)) (or p18 (not p14) p6) (or (not p19) p8 (not p19)) (or p5 p16 (not p19)) (or p1 p9 p3) (or p7 p2 (not p16)) (or (not p17) (not p12) p9) (or (not p14) (not p15) p13) (or p17 (not p17) (not p0)) (or (not p1) p3 p16) (or (not p14) (not p12) (not p7)) (or (not p8) (not p17) (not p5)) (or p18 (not p16) (not p13)) (or (not p9) p6 p8) (or p10 (not p7) (not p18)) (or (not p1) (not p3) p14) (or (not p9) (not p1) (not p15)) (or p8 (not p8) p0) (or p2 (not p17) (not p15)) (or p0 (not p10) p14) (or (not p14) (not p6) (not p13)) (or (not p10) (not p3) (not p4)) (or p9 (not p0) (not p14)) (or (not p12) (not p4) p6))
)
(check-sat)
(get-model)
