(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p15() Bool)
(declare-fun p14() Bool)
(declare-fun p7() Bool)
(declare-fun p16() Bool)
(declare-fun p10() Bool)
(declare-fun p4() Bool)
(declare-fun p6() Bool)
(declare-fun p1() Bool)
(declare-fun p3() Bool)
(declare-fun p0() Bool)
(declare-fun p12() Bool)
(declare-fun p9() Bool)
(declare-fun p19() Bool)
(declare-fun p13() Bool)
(declare-fun p2() Bool)
(declare-fun p17() Bool)
(declare-fun p11() Bool)
(declare-fun p5() Bool)
(declare-fun p18() Bool)
(declare-fun p8() Bool)
(assert
(and (or p15 p14 p7) (or (not p16) p14 p7) (or p16 p10 (not p4)) (or p15 (not p6) p1) (or (not p3) p14 p1) (or p0 (not p12) p14) (or (not p7) (not p10) p9) (or (not p14) p16 p7) (or (not p4) p19 p7) (or (not p10) (not p0) p14) (or (not p1) p1 p6) (or p13 (not p6) p14) (or (not p2) p4 p17) (or (not p3) (not p13) (not p11)) (or p11 (not p10) (not p11)) (or p5 p18 p14) (or (not p10) p2 p15) (or (not p3) p12 (not p2)) (or (not p17) p3 p5) (or p6 p7 p10) (or (not p9) (not p0) p9) (or p4 p19 p17) (or (not p15) (not p2) p5) (or (not p6) p1 p2) (or (not p11) p4 p7) (or p7 (not p11) p4) (or (not p15) (not p17) p13) (or p0 p10 p6) (or (not p7) p8 p3) (or p1 p2 p17) (or (not p0) (not p8) (not p11)) (or p8 (not p14) (not p1)) (or p17 (not p2) p8) (or (not p15) (not p11) (not p15)) (or (not p13) p5 (not p0)) (or (not p15) (not p13) (not p1)) (or p19 (not p16) (not p17)) (or (not p11) p16 p6) (or (not p8) p16 p14) (or (not p15) p2 p15) (or p12 p8 p11) (or p7 p10 (not p3)) (or (not p18) (not p1) (not p3)) (or p5 (not p5) p16) (or p18 p0 (not p16)) (or (not p8) (not p13) (not p10)) (or p8 (not p11) (not p18)) (or (not p3) p3 p11) (or p0 (not p6) p13) (or p16 (not p7) p17) (or (not p6) p11 (not p19)) (or (not p1) (not p9) p10) (or (not p1) (not p16) (not p3)) (or p13 p4 p12) (or p12 (not p14) p14) (or (not p5) p14 (not p7)) (or (not p16) p2 (not p2)) (or p5 (not p15) p1) (or p7 p3 (not p16)) (or (not p18) p3 p3) (or (not p9) p4 (not p6)) (or p19 p6 (not p11)) (or p19 p17 p3) (or (not p19) p13 (not p18)) (or p0 p4 p2) (or p1 p8 (not p14)) (or p3 (not p13) p17) (or (not p16) p6 p15) (or (not p18) p9 p7) (or (not p5) p4 (not p16)) (or p17 (not p0) (not p13)) (or (not p14) (not p5) p1) (or p1 p19 p19) (or p18 (not p12) p14) (or p16 (not p16) (not p11)) (or (not p13) p3 p11) (or (not p13) (not p15) (not p17)) (or (not p10) (not p16) (not p19)) (or p19 (not p10) p1) (or (not p5) p15 (not p12)) (or (not p17) p6 (not p2)) (or p17 (not p5) p2) (or (not p12) (not p5) p2) (or (not p8) (not p7) (not p14)) (or p10 p18 p1) (or p14 (not p2) p17) (or (not p11) (not p17) p18))
)
(check-sat)
(get-model)