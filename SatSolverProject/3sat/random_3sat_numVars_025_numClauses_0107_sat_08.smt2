(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p14() Bool)
(declare-fun p22() Bool)
(declare-fun p10() Bool)
(declare-fun p17() Bool)
(declare-fun p11() Bool)
(declare-fun p4() Bool)
(declare-fun p24() Bool)
(declare-fun p7() Bool)
(declare-fun p5() Bool)
(declare-fun p12() Bool)
(declare-fun p15() Bool)
(declare-fun p0() Bool)
(declare-fun p20() Bool)
(declare-fun p21() Bool)
(declare-fun p1() Bool)
(declare-fun p19() Bool)
(declare-fun p16() Bool)
(declare-fun p2() Bool)
(declare-fun p13() Bool)
(declare-fun p3() Bool)
(declare-fun p18() Bool)
(declare-fun p23() Bool)
(declare-fun p8() Bool)
(declare-fun p9() Bool)
(declare-fun p6() Bool)
(assert
(and (or (not p14) (not p22) (not p10)) (or (not p17) p11 (not p4)) (or (not p24) (not p7) p5) (or (not p12) p15 (not p0)) (or p15 (not p14) p0) (or (not p12) (not p20) (not p17)) (or (not p21) p0 p1) (or p19 p12 (not p16)) (or (not p7) (not p2) p12) (or (not p20) p0 (not p17)) (or p11 (not p13) (not p15)) (or p3 (not p16) p20) (or (not p13) (not p18) (not p4)) (or p11 (not p10) p4) (or p24 p5 p11) (or p11 (not p16) p23) (or p12 (not p7) (not p15)) (or (not p10) p20 (not p7)) (or p16 (not p3) p8) (or (not p21) p18 (not p21)) (or (not p17) p1 (not p21)) (or p7 (not p13) (not p24)) (or (not p21) p9 (not p15)) (or p21 p0 p17) (or p11 (not p6) (not p19)) (or (not p7) p22 p13) (or (not p1) p3 (not p7)) (or p1 p23 p12) (or (not p23) p13 p14) (or (not p9) p2 (not p24)) (or p11 p8 (not p7)) (or p14 (not p18) p1) (or (not p15) (not p23) p10) (or p5 p13 p11) (or (not p9) p16 (not p13)) (or (not p8) p3 (not p13)) (or p7 p10 (not p1)) (or (not p1) (not p14) (not p6)) (or (not p12) p6 (not p22)) (or (not p15) p15 p2) (or p7 (not p7) p18) (or p16 (not p24) p11) (or p4 (not p7) (not p3)) (or (not p11) (not p17) p6) (or (not p12) (not p18) (not p1)) (or p0 p20 p18) (or p15 p22 (not p2)) (or (not p22) p11 p23) (or (not p14) p19 p17) (or (not p5) (not p9) (not p1)) (or (not p16) (not p12) p15) (or (not p24) (not p9) p8) (or (not p7) p7 p22) (or (not p12) (not p6) p22) (or (not p6) p10 (not p17)) (or (not p23) p9 p14) (or (not p17) (not p6) (not p17)) (or p19 p14 p6) (or p23 p21 (not p18)) (or p7 p19 p7) (or p14 p24 p13) (or (not p2) p13 p7) (or p22 (not p0) (not p5)) (or (not p2) (not p16) (not p20)) (or p23 (not p2) (not p11)) (or p6 (not p1) p2) (or (not p12) (not p1) p22) (or p7 p3 p17) (or (not p6) p23 p7) (or (not p14) (not p6) p3) (or (not p10) p2 (not p1)) (or p5 p17 (not p9)) (or (not p19) p24 p14) (or (not p4) p0 (not p2)) (or (not p14) (not p21) (not p4)) (or p15 (not p16) p3) (or p15 (not p6) (not p23)) (or (not p1) p15 p4) (or (not p5) p8 (not p11)) (or (not p23) p3 p24) (or (not p5) (not p17) (not p19)) (or (not p23) (not p0) (not p17)) (or (not p18) p9 p22) (or (not p11) (not p16) (not p23)) (or p11 (not p22) p9) (or (not p16) p15 p10) (or p0 (not p24) (not p11)) (or p9 p19 p3) (or p17 (not p20) (not p6)) (or p18 (not p18) (not p4)) (or p11 (not p4) p21) (or p15 (not p2) (not p0)) (or (not p1) p4 p17) (or p15 (not p12) p11) (or p7 (not p19) (not p21)) (or p12 p17 p19) (or (not p1) (not p16) p15) (or p7 p21 (not p20)) (or (not p7) (not p22) (not p3)) (or (not p14) p9 p11) (or (not p13) (not p11) (not p23)) (or (not p1) (not p1) p5) (or (not p11) p9 p8) (or (not p20) (not p11) p8) (or p6 (not p19) (not p21)) (or (not p22) (not p4) (not p22)) (or (not p14) (not p17) p23))
)
(check-sat)
(get-model)
