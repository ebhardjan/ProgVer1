(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p15() Bool)
(declare-fun p4() Bool)
(declare-fun p10() Bool)
(declare-fun p11() Bool)
(declare-fun p2() Bool)
(declare-fun p19() Bool)
(declare-fun p13() Bool)
(declare-fun p5() Bool)
(declare-fun p8() Bool)
(declare-fun p17() Bool)
(declare-fun p7() Bool)
(declare-fun p9() Bool)
(declare-fun p20() Bool)
(declare-fun p3() Bool)
(declare-fun p16() Bool)
(declare-fun p0() Bool)
(declare-fun p12() Bool)
(declare-fun p14() Bool)
(declare-fun p6() Bool)
(declare-fun p23() Bool)
(declare-fun p21() Bool)
(declare-fun p24() Bool)
(declare-fun p18() Bool)
(declare-fun p22() Bool)
(declare-fun p1() Bool)
(assert
(and (or (not p15) p4 p10) (or p11 p2 p19) (or p15 p19 p2) (or p13 (not p5) (not p8)) (or (not p17) p13 (not p8)) (or (not p10) (not p7) p9) (or p20 p3 p16) (or p0 p11 (not p11)) (or (not p12) (not p14) (not p12)) (or (not p14) p6 p19) (or (not p23) (not p23) p21) (or p3 (not p10) (not p23)) (or (not p17) (not p5) (not p3)) (or (not p3) p13 p0) (or p3 p24 (not p9)) (or (not p18) (not p6) p5) (or (not p21) (not p13) p24) (or p7 (not p20) (not p4)) (or p22 (not p10) (not p6)) (or p9 (not p8) (not p8)) (or p5 (not p23) (not p9)) (or p2 (not p24) p14) (or (not p14) (not p19) p6) (or p13 p0 (not p18)) (or (not p24) (not p13) (not p4)) (or p15 p4 p8) (or (not p4) p9 (not p5)) (or p15 (not p16) (not p2)) (or (not p11) (not p7) p16) (or (not p13) p16 p12) (or p7 (not p23) p24) (or p1 (not p3) p16) (or (not p20) (not p8) p23) (or (not p9) p10 p21) (or (not p2) p3 (not p22)) (or p0 (not p17) (not p2)) (or (not p21) (not p4) (not p8)) (or (not p12) p22 p18) (or p18 p14 p1) (or p19 p17 (not p15)) (or p9 p17 (not p15)) (or p15 (not p22) p16) (or (not p4) (not p24) p4) (or p1 (not p24) (not p21)) (or (not p13) (not p3) p23) (or (not p3) p24 (not p24)) (or (not p22) p0 (not p14)) (or p0 p1 (not p13)) (or p24 (not p21) p6) (or p20 p4 p6) (or p3 p23 p16) (or (not p1) (not p17) (not p3)) (or (not p22) (not p7) p19) (or (not p24) p13 p18) (or p18 p1 p11) (or (not p24) p9 (not p16)) (or (not p15) (not p3) p0) (or p17 p0 (not p6)) (or p9 p16 (not p6)) (or p2 p19 p6) (or (not p23) (not p7) p18) (or (not p13) p20 (not p15)) (or p5 (not p18) p21) (or p5 (not p4) (not p19)) (or (not p10) p18 (not p1)) (or (not p7) (not p19) (not p14)) (or p23 p6 p9) (or (not p22) p8 (not p7)) (or (not p6) p22 (not p18)) (or (not p11) (not p22) p15) (or p21 p8 p0) (or p24 p12 (not p2)) (or (not p20) (not p1) (not p2)) (or p0 p9 (not p5)) (or p17 (not p22) (not p8)) (or p6 (not p18) p13) (or p18 (not p13) p7) (or p8 (not p12) p10) (or (not p8) p9 p5) (or p15 p1 p11) (or (not p22) (not p13) (not p9)) (or (not p5) (not p3) p18) (or p10 (not p21) p3) (or (not p22) p0 p5) (or p6 p13 (not p4)) (or (not p8) p24 (not p6)) (or p7 (not p0) p6) (or (not p12) (not p13) (not p19)) (or p15 (not p1) (not p6)) (or (not p8) p23 p23) (or p10 p13 p3) (or (not p23) (not p4) (not p18)) (or p17 p3 (not p22)) (or (not p24) (not p21) (not p14)) (or (not p8) (not p16) (not p0)) (or (not p3) p9 (not p16)) (or p17 (not p0) p22) (or (not p14) p20 (not p4)) (or p9 p12 p16) (or (not p22) p5 (not p10)) (or (not p1) p20 (not p14)) (or (not p16) (not p14) (not p2)) (or (not p5) (not p5) (not p6)) (or p16 p4 (not p21)) (or (not p0) p4 p3) (or p15 (not p24) p7) (or p13 (not p15) (not p22)))
)
(check-sat)
(get-model)
