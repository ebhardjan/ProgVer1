(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p2() Bool)
(declare-fun p19() Bool)
(declare-fun p17() Bool)
(declare-fun p11() Bool)
(declare-fun p15() Bool)
(declare-fun p10() Bool)
(declare-fun p8() Bool)
(declare-fun p13() Bool)
(declare-fun p7() Bool)
(declare-fun p12() Bool)
(declare-fun p5() Bool)
(declare-fun p3() Bool)
(declare-fun p18() Bool)
(declare-fun p1() Bool)
(declare-fun p16() Bool)
(declare-fun p14() Bool)
(declare-fun p9() Bool)
(declare-fun p4() Bool)
(declare-fun p6() Bool)
(declare-fun p0() Bool)
(assert
(and (or (not p2) (not p19) p17) (or p11 p15 p15) (or (not p10) p19 p8) (or (not p13) (not p11) (not p17)) (or (not p10) p7 p12) (or (not p15) p2 p5) (or (not p3) (not p13) p18) (or (not p10) p15 (not p1)) (or p18 (not p17) p13) (or (not p7) (not p11) p11) (or (not p8) p18 (not p16)) (or (not p13) (not p8) (not p10)) (or p13 (not p12) (not p13)) (or (not p15) p13 p14) (or p10 p1 (not p3)) (or p5 p12 p5) (or p5 (not p12) p17) (or (not p16) p5 (not p1)) (or p10 p18 p19) (or p2 p11 (not p9)) (or (not p2) (not p5) (not p17)) (or p5 p12 p12) (or p9 (not p18) p4) (or p12 p9 p9) (or p18 p17 p5) (or (not p10) (not p10) (not p6)) (or p14 (not p6) p3) (or p17 (not p3) p18) (or p9 (not p2) p18) (or p9 (not p19) (not p5)) (or p8 (not p8) p13) (or p14 (not p9) (not p17)) (or (not p9) (not p5) (not p14)) (or p1 (not p17) p5) (or p15 p6 p8) (or (not p12) p1 (not p14)) (or p10 (not p7) (not p1)) (or p6 p6 (not p7)) (or p10 p17 (not p15)) (or p8 p9 p3) (or (not p3) (not p2) (not p9)) (or (not p3) (not p1) (not p7)) (or p6 p17 (not p13)) (or p14 p5 (not p3)) (or (not p1) (not p4) p4) (or p8 (not p14) (not p3)) (or (not p14) (not p12) (not p10)) (or p14 (not p0) p7) (or p18 (not p12) (not p14)) (or p0 p6 (not p16)) (or p14 (not p0) p2) (or p9 p12 (not p10)) (or p1 (not p5) (not p17)) (or p2 p0 p15) (or p11 p14 p10) (or (not p2) (not p10) (not p18)) (or p10 p1 (not p2)) (or p0 p7 p6) (or p14 (not p1) (not p9)) (or (not p18) p7 (not p7)) (or p17 p7 (not p19)) (or p8 p2 p10) (or (not p4) (not p16) p0) (or p11 (not p16) (not p12)) (or (not p16) p10 (not p9)) (or p7 p13 p4) (or p14 p10 (not p6)) (or (not p6) (not p9) (not p10)) (or (not p14) p11 (not p13)) (or (not p5) (not p10) p4) (or (not p17) p0 (not p7)) (or p3 p13 (not p4)) (or p6 p5 p10) (or (not p2) (not p19) (not p10)) (or (not p19) (not p4) (not p19)) (or (not p1) (not p11) (not p10)) (or p13 p11 p6) (or (not p16) p8 (not p11)) (or p17 (not p8) p19) (or (not p11) (not p13) p2) (or p9 p5 (not p14)) (or p6 (not p10) p17) (or p12 (not p1) p13) (or (not p17) p10 p16) (or p2 (not p5) p4) (or (not p1) (not p6) (not p15)) (or (not p11) (not p19) (not p14)))
)
(check-sat)
(get-model)