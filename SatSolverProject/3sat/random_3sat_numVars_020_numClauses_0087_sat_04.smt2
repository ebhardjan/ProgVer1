(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p5() Bool)
(declare-fun p15() Bool)
(declare-fun p6() Bool)
(declare-fun p4() Bool)
(declare-fun p3() Bool)
(declare-fun p9() Bool)
(declare-fun p14() Bool)
(declare-fun p10() Bool)
(declare-fun p1() Bool)
(declare-fun p8() Bool)
(declare-fun p13() Bool)
(declare-fun p11() Bool)
(declare-fun p16() Bool)
(declare-fun p7() Bool)
(declare-fun p18() Bool)
(declare-fun p0() Bool)
(declare-fun p12() Bool)
(declare-fun p19() Bool)
(declare-fun p2() Bool)
(declare-fun p17() Bool)
(assert
(and (or (not p5) p15 (not p6)) (or p4 (not p3) (not p9)) (or (not p14) p10 p1) (or (not p6) (not p8) p13) (or (not p11) p14 p1) (or (not p3) p9 p16) (or p7 (not p15) (not p15)) (or (not p16) (not p14) (not p4)) (or (not p9) p10 p10) (or (not p18) (not p0) p12) (or (not p15) p19 p2) (or p0 (not p16) (not p9)) (or (not p11) (not p15) p10) (or (not p11) (not p17) (not p18)) (or p13 p16 p8) (or (not p19) p9 (not p12)) (or p14 p8 (not p9)) (or p17 p5 p10) (or (not p13) p6 p16) (or (not p9) (not p17) (not p2)) (or (not p6) p16 p13) (or p15 p11 (not p2)) (or (not p4) p16 (not p12)) (or (not p11) (not p5) p2) (or p3 p8 (not p18)) (or (not p17) (not p16) p12) (or (not p1) p12 (not p10)) (or p4 p15 (not p4)) (or p8 p2 p10) (or p7 (not p14) p12) (or p6 p8 (not p0)) (or p4 (not p17) p13) (or (not p9) (not p4) p16) (or (not p10) p18 p0) (or p16 p17 (not p4)) (or (not p2) (not p0) (not p17)) (or (not p16) p7 (not p7)) (or (not p1) p12 p4) (or p1 p2 p10) (or p17 (not p4) (not p6)) (or p12 (not p19) p10) (or p0 (not p6) (not p14)) (or (not p19) (not p5) p10) (or p15 p2 (not p2)) (or (not p19) p15 (not p15)) (or p5 (not p14) p15) (or p14 p10 (not p18)) (or (not p19) (not p17) p18) (or p1 p5 p9) (or p12 (not p4) (not p8)) (or p12 p1 (not p17)) (or p1 p14 (not p7)) (or (not p6) (not p3) p0) (or p17 (not p15) (not p9)) (or p1 (not p8) p14) (or (not p16) (not p12) p10) (or p16 (not p1) p19) (or p10 p11 p0) (or (not p17) p10 (not p18)) (or (not p4) p13 (not p19)) (or (not p3) (not p19) p5) (or p13 (not p9) (not p13)) (or p13 (not p6) p9) (or p9 (not p11) p2) (or (not p7) p9 p4) (or p9 (not p4) (not p1)) (or (not p11) p12 (not p1)) (or p9 p18 (not p18)) (or p15 (not p3) (not p10)) (or p5 p17 p13) (or (not p7) p4 (not p14)) (or (not p10) (not p19) (not p19)) (or (not p11) (not p10) p11) (or (not p2) (not p9) p11) (or p3 p11 (not p14)) (or (not p7) p18 p0) (or p16 p0 (not p10)) (or (not p11) p6 (not p4)) (or (not p19) (not p18) p8) (or p17 p17 (not p14)) (or (not p8) p15 (not p16)) (or (not p14) (not p12) p12) (or (not p9) p3 (not p12)) (or (not p2) (not p5) p0) (or p18 (not p18) p10) (or p2 (not p2) p3) (or (not p11) p4 (not p14)))
)
(check-sat)
(get-model)
