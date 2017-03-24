(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p11() Bool)
(declare-fun p8() Bool)
(declare-fun p10() Bool)
(declare-fun p9() Bool)
(declare-fun p7() Bool)
(declare-fun p13() Bool)
(declare-fun p16() Bool)
(declare-fun p19() Bool)
(declare-fun p18() Bool)
(declare-fun p15() Bool)
(declare-fun p1() Bool)
(declare-fun p17() Bool)
(declare-fun p5() Bool)
(declare-fun p4() Bool)
(declare-fun p2() Bool)
(declare-fun p0() Bool)
(declare-fun p14() Bool)
(declare-fun p12() Bool)
(declare-fun p6() Bool)
(declare-fun p3() Bool)
(assert
(and (or (not p11) (not p8) (not p10) (not p9) p7) (or (not p13) p16 (not p19) p18 (not p15)) (or p16 (not p1) p11 (not p19) (not p16)) (or p17 p5 p19 (not p16) (not p15)) (or p4 p18 (not p15) (not p2) p16) (or (not p19) (not p8) (not p9) (not p0) p9) (or (not p14) (not p14) (not p10) (not p12) (not p13)) (or p6 (not p16) p18 (not p1) (not p8)) (or p13 p14 p17 p12 (not p12)) (or (not p5) (not p1) (not p6) (not p2) p19) (or p16 p18 (not p17) p18 (not p3)) (or (not p1) (not p4) p10 (not p2) (not p14)) (or p8 (not p6) (not p10) (not p18) (not p12)) (or (not p15) p12 (not p10) p1 (not p14)) (or p17 p14 (not p15) p5 (not p17)) (or p7 (not p13) (not p10) (not p16) (not p2)) (or (not p8) p6 p17 p2 p6) (or p10 (not p4) (not p0) (not p4) (not p19)) (or p1 p19 (not p5) (not p15) p14) (or (not p7) p18 p1 p2 p9) (or (not p17) (not p10) p1 p13 p7) (or (not p0) p8 p15 (not p2) (not p16)) (or (not p16) (not p11) p0 p10 p9) (or p10 p2 (not p15) p9 (not p1)) (or p8 (not p0) p9 (not p13) (not p9)) (or (not p11) p8 (not p9) (not p12) p14) (or (not p3) p10 p7 (not p16) (not p4)) (or p19 (not p10) (not p19) (not p0) (not p2)) (or p11 p2 p15 p9 p3) (or (not p17) (not p8) p18 p4 p16) (or p9 p0 p15 (not p14) (not p19)) (or (not p11) (not p18) p5 (not p17) p2) (or (not p19) (not p5) p0 p19 p6) (or (not p10) p10 (not p9) (not p9) (not p19)) (or p18 p3 (not p3) p16 (not p0)) (or p15 (not p10) (not p11) p15 (not p15)) (or (not p2) (not p4) (not p3) p16 (not p10)) (or (not p15) p15 p17 p19 p11) (or (not p17) (not p18) (not p9) (not p16) p19) (or p10 p10 p2 p8 (not p1)) (or p0 p17 p6 p4 p14) (or (not p16) (not p10) p15 (not p9) p9) (or (not p13) p8 p6 (not p17) (not p4)) (or p11 p17 p3 p16 p6) (or (not p6) (not p6) (not p9) p17 p2) (or p12 (not p0) p3 p1 p15) (or (not p13) p10 p2 (not p9) p9) (or (not p4) p7 (not p16) (not p6) p2) (or p9 (not p13) (not p19) (not p5) p13) (or (not p15) p7 (not p5) p4 (not p5)) (or p14 (not p11) (not p15) (not p1) p6) (or p16 (not p5) p5 p12 p15) (or p5 p8 p13 (not p8) p7) (or p4 p13 p2 (not p9) (not p5)) (or p13 (not p11) (not p4) (not p15) p13) (or p18 (not p11) (not p5) (not p6) (not p9)) (or (not p10) (not p7) p14 p0 p2) (or p9 p9 p13 p5 p14) (or (not p17) p16 p8 (not p10) (not p6)) (or (not p16) (not p1) p19 (not p0) (not p2)) (or p8 p7 (not p0) p14 (not p9)) (or (not p18) (not p18) (not p1) (not p12) (not p14)) (or (not p15) (not p1) (not p10) p9 (not p0)) (or (not p12) (not p19) p12 (not p17) (not p10)) (or p8 p17 p4 p10 p18) (or p2 p1 (not p16) (not p18) p11) (or (not p9) (not p5) p15 p9 p18) (or (not p14) p3 p3 p2 (not p6)) (or (not p15) (not p10) (not p13) p2 p4) (or p14 p6 p10 p16 (not p1)) (or (not p19) p7 (not p14) p16 p8) (or p14 (not p13) (not p8) p18 p0) (or (not p11) p14 p19 p9 (not p5)) (or p16 (not p2) (not p8) p15 p14) (or (not p1) (not p8) p2 p9 p7) (or (not p1) p2 p18 (not p1) (not p14)) (or p5 (not p0) p15 p6 p6) (or (not p2) p11 (not p15) (not p16) (not p13)) (or (not p14) p7 p3 (not p15) p16) (or (not p12) p1 p13 p12 p4) (or (not p0) (not p19) p8 p12 (not p13)) (or (not p5) p0 p17 p10 p13) (or p1 p2 (not p1) (not p1) (not p18)) (or (not p12) (not p0) (not p12) p7 (not p5)) (or (not p13) (not p8) (not p9) (not p1) p1) (or (not p16) (not p9) (not p4) p12 (not p14)) (or p6 (not p5) (not p13) (not p11) (not p6)) (or p18 (not p3) p19 (not p16) (not p9)) (or (not p5) (not p0) p6 p3 p5) (or p2 p11 (not p10) p10 (not p7)) (or (not p0) (not p11) p19 p14 (not p3)) (or p14 p3 (not p16) p5 p2) (or p9 (not p17) p13 (not p11) p19) (or (not p9) p18 (not p6) p12 (not p4)) (or (not p9) (not p5) p15 p19 p0) (or p7 (not p14) (not p17) p10 p12) (or (not p5) p18 (not p9) p5 (not p8)) (or (not p1) p8 p16 p15 (not p18)) (or p16 (not p15) (not p13) p10 p2) (or (not p6) p2 p19 p10 p19) (or (not p2) p7 p10 p9 (not p0)) (or p10 p5 p11 p17 p9) (or (not p18) (not p6) (not p1) (not p7) p16) (or p16 p0 p5 (not p7) p12) (or p13 p0 (not p12) p16 p4) (or (not p13) (not p10) p8 p10 p13) (or p5 p8 (not p7) p11 p6) (or (not p3) p16 (not p13) p0 (not p0)) (or (not p17) (not p5) p14 p14 (not p5)) (or p1 p2 p3 p12 (not p2)) (or (not p16) p12 (not p1) (not p9) (not p9)) (or p4 p7 (not p19) p3 p18) (or (not p7) (not p10) p4 (not p5) (not p16)) (or p7 (not p4) (not p13) (not p8) p10) (or (not p11) p18 p5 (not p7) (not p13)) (or (not p16) p14 (not p0) (not p17) p7) (or (not p6) p9 p13 p0 p0) (or p7 p2 (not p16) (not p0) p12) (or p15 p3 (not p7) (not p3) (not p0)) (or p16 p9 (not p11) p19 p17) (or p17 (not p17) p16 (not p13) (not p15)) (or p7 (not p0) p9 p7 p3) (or (not p12) p18 p4 p3 p11) (or (not p19) (not p7) (not p2) (not p7) p0) (or (not p4) p8 (not p6) p5 p13) (or (not p3) p14 (not p7) (not p4) (not p18)) (or p2 (not p18) p5 (not p12) (not p0)) (or p15 p14 (not p1) p3 p13) (or p11 (not p10) p3 p18 p4) (or (not p14) (not p4) p13 p10 p16) (or (not p16) p0 (not p11) p0 p15) (or (not p0) (not p2) p3 p13 (not p2)) (or p15 p19 p9 (not p19) (not p9)) (or p16 p12 p13 (not p11) p8) (or (not p0) (not p2) p7 p8 p4) (or (not p9) (not p5) p2 p8 p14) (or (not p7) (not p14) p6 p8 p16) (or (not p16) (not p17) (not p13) (not p4) p6) (or p5 p6 p0 (not p1) (not p13)) (or p17 p18 p17 (not p19) p15) (or p11 (not p12) (not p8) (not p14) p17) (or (not p9) (not p15) (not p6) (not p6) (not p0)) (or p11 (not p5) p4 (not p7) (not p0)) (or (not p11) (not p18) (not p12) (not p1) p5) (or (not p12) p17 p12 (not p13) p0) (or (not p17) (not p15) p11 (not p4) (not p12)) (or p16 (not p0) p5 p12 (not p11)) (or p2 p15 p14 (not p9) p14) (or (not p3) (not p4) (not p11) p5 (not p16)) (or (not p15) p6 p18 (not p5) (not p11)) (or (not p16) p16 p16 (not p15) p12) (or (not p12) (not p11) (not p19) (not p8) p4) (or p0 (not p1) (not p4) (not p19) p0) (or p3 (not p5) (not p15) (not p13) p1) (or p14 (not p18) (not p15) (not p8) p1) (or p9 p10 (not p5) p17 p10) (or (not p3) p15 p2 p1 p19) (or p0 p4 (not p19) (not p7) p11) (or (not p2) p6 (not p9) (not p0) p9) (or (not p16) (not p17) p5 (not p1) (not p17)) (or p6 p7 (not p3) p1 (not p17)) (or (not p9) p5 p19 (not p5) p18) (or p12 (not p2) p7 p10 p6) (or p2 (not p0) p10 (not p16) p15) (or (not p7) (not p11) p8 p18 p0) (or (not p4) p19 (not p19) p1 (not p10)) (or p12 (not p11) (not p8) p2 p2) (or p12 p18 p14 p12 p10) (or (not p13) p10 (not p16) p14 p5) (or p12 p16 (not p6) p16 (not p10)) (or (not p8) (not p11) p5 p18 (not p9)) (or (not p19) (not p8) (not p13) p4 (not p11)) (or p14 (not p14) p19 (not p12) (not p9)) (or (not p18) (not p12) (not p5) (not p6) p12) (or (not p11) (not p15) (not p1) (not p19) (not p9)) (or (not p10) (not p12) (not p19) (not p13) (not p12)) (or (not p3) (not p0) p10 (not p14) (not p7)) (or p0 (not p12) (not p7) p5 p0) (or (not p19) p19 p3 (not p18) p9) (or p16 p0 p13 (not p10) (not p4)) (or p0 (not p16) p9 p16 (not p2)) (or p14 (not p4) (not p19) (not p10) p7) (or (not p5) p17 (not p15) p14 (not p16)) (or p0 p3 (not p5) (not p10) (not p3)) (or p8 p16 p14 (not p8) (not p16)) (or (not p10) (not p9) (not p10) p10 (not p14)) (or (not p1) p7 (not p1) (not p16) (not p10)) (or p10 (not p15) (not p12) p9 (not p8)) (or (not p1) (not p18) (not p18) p17 p18) (or (not p15) (not p11) (not p16) p14 p1) (or p1 p4 p6 (not p18) (not p9)) (or p12 (not p1) (not p14) (not p19) p19) (or p9 p2 p0 p19 (not p1)) (or (not p11) (not p18) p6 (not p19) (not p19)) (or p5 (not p3) (not p0) p1 p18) (or p6 (not p15) p8 p3 p10) (or p13 p15 p1 (not p17) p7) (or p14 p3 p2 p13 p11) (or p9 (not p10) (not p11) (not p14) p11) (or p15 p1 (not p13) (not p11) p15) (or p19 p3 p14 (not p6) p7) (or (not p14) p19 p10 (not p2) p16) (or p11 p11 (not p5) (not p15) (not p6)) (or p18 p3 p15 p1 (not p4)) (or (not p18) p10 (not p15) (not p18) p2) (or (not p12) p0 (not p3) (not p19) p18) (or (not p4) (not p13) (not p12) (not p19) p16) (or p17 (not p14) (not p12) p0 p16) (or p14 p15 (not p0) (not p16) p9) (or p15 p16 (not p15) p19 (not p13)) (or (not p4) p9 (not p5) (not p15) p15) (or p9 p1 (not p15) p6 (not p11)) (or p6 p14 p8 (not p17) p0) (or p14 p0 (not p10) p12 p12) (or p14 p17 (not p6) p14 (not p1)) (or p14 (not p14) (not p4) (not p10) (not p1)) (or (not p14) (not p2) p11 (not p5) p3) (or (not p9) p10 (not p7) (not p12) (not p8)) (or p2 p1 p8 p12 p13) (or (not p13) (not p19) (not p0) p17 p0) (or p7 p6 (not p3) p19 (not p17)) (or p2 p19 p6 (not p4) p14) (or (not p19) p5 (not p18) p16 (not p18)) (or p9 (not p0) p19 (not p1) p12) (or p14 (not p16) p2 p3 p2) (or (not p11) p8 (not p18) p9 p11) (or (not p14) (not p11) (not p13) (not p0) (not p12)) (or (not p19) p16 p3 p2 p0) (or (not p1) (not p6) p5 p7 (not p13)) (or p6 (not p3) p16 (not p0) (not p19)) (or p13 (not p2) (not p0) (not p8) p0) (or p7 (not p12) (not p19) p6 (not p14)) (or p4 (not p17) (not p4) p14 p1) (or p9 (not p5) p10 (not p15) (not p1)) (or (not p17) (not p14) p18 p2 (not p7)) (or (not p12) p3 (not p4) (not p4) (not p10)) (or (not p8) (not p13) (not p18) (not p6) (not p6)) (or p10 p7 p4 (not p18) (not p6)) (or (not p18) p15 (not p4) (not p0) (not p8)) (or (not p7) (not p19) (not p12) (not p1) (not p13)) (or (not p3) p10 p6 (not p14) (not p16)) (or p15 p15 (not p6) p15 (not p8)) (or p0 p4 p6 p7 (not p0)) (or p9 p16 p1 p11 p2) (or p3 (not p13) p8 p4 (not p13)) (or (not p13) p6 (not p16) (not p11) (not p6)) (or p15 p12 (not p5) p9 (not p4)) (or (not p6) p1 p17 p19 (not p16)) (or (not p2) p15 (not p18) p1 p8) (or (not p12) (not p17) (not p17) p14 (not p3)) (or (not p5) (not p19) p1 (not p10) p7) (or (not p10) (not p17) (not p1) (not p14) (not p15)) (or p12 p15 p11 p17 (not p16)) (or (not p4) (not p16) (not p9) p19 (not p12)) (or p12 p16 (not p1) p0 (not p2)) (or p11 (not p12) (not p19) (not p0) (not p15)) (or p11 (not p19) p1 p6 p9) (or p13 p18 (not p11) (not p13) p19) (or p12 (not p16) p8 p11 (not p11)) (or (not p13) (not p6) (not p1) (not p13) p3) (or p19 p8 p19 p6 p18) (or (not p15) p2 (not p4) p17 (not p2)) (or p11 p15 (not p1) p15 (not p7)) (or (not p7) p16 (not p4) p1 (not p6)) (or p14 p0 p11 (not p13) (not p12)) (or p5 (not p11) (not p7) p9 p8) (or (not p7) (not p15) (not p7) (not p17) (not p4)) (or p19 p14 p7 p2 p2) (or p7 p14 (not p18) (not p18) p19) (or p6 p13 (not p9) (not p2) p4) (or (not p17) p13 (not p10) (not p14) (not p7)) (or p5 (not p9) p17 (not p5) (not p14)) (or (not p15) (not p17) (not p14) (not p17) (not p18)) (or (not p3) (not p16) p12 p15 (not p16)) (or p7 p1 (not p5) (not p4) p8) (or p12 (not p0) p6 (not p3) p17) (or (not p9) (not p14) (not p5) p17 (not p2)) (or p8 p11 (not p16) (not p2) p5) (or p16 (not p14) (not p17) p16 p15) (or (not p2) p10 (not p6) p14 (not p10)) (or p4 p18 p7 p1 (not p16)) (or p5 (not p17) p2 (not p0) (not p9)) (or (not p9) p3 p5 (not p5) p15) (or p2 p18 p18 p0 (not p14)) (or p8 (not p4) (not p2) p9 p16) (or (not p16) p6 (not p2) p8 (not p13)) (or p9 (not p5) p8 p6 (not p2)) (or p12 (not p14) (not p8) p11 (not p17)) (or p9 (not p17) (not p14) p8 (not p6)) (or (not p10) p13 (not p0) (not p4) p10) (or (not p2) (not p4) (not p8) (not p14) p8) (or (not p0) (not p5) p3 p16 (not p9)) (or p0 p7 p13 (not p17) (not p14)) (or (not p19) p2 (not p3) p17 p3) (or p18 p6 p3 (not p5) (not p1)) (or p8 (not p1) p5 p14 p1) (or (not p11) (not p0) p6 (not p13) p0) (or p3 p17 (not p19) p6 p2) (or (not p12) p7 (not p4) p11 (not p18)) (or p10 (not p6) p17 (not p11) p15) (or p7 (not p18) p4 p12 (not p0)) (or (not p10) (not p7) (not p17) p12 p8) (or (not p9) p12 p13 (not p9) p16) (or (not p15) (not p11) (not p13) (not p11) p8) (or (not p10) (not p12) (not p18) p4 (not p8)) (or p12 (not p4) p12 (not p17) p17) (or p12 (not p16) (not p0) p0 (not p8)) (or p14 (not p6) (not p12) (not p0) p1) (or p8 p8 p6 (not p13) (not p17)) (or p11 p13 (not p14) (not p6) (not p5)) (or (not p2) (not p4) p9 (not p19) (not p19)) (or (not p7) p14 p12 (not p14) p15) (or (not p11) (not p5) (not p3) p12 (not p11)) (or (not p14) p3 p17 p10 (not p9)) (or (not p15) p11 p10 p1 p14) (or p11 (not p8) (not p0) (not p3) p12) (or p7 p14 (not p2) (not p5) (not p8)) (or p12 p6 p5 (not p6) p10) (or p16 (not p16) (not p19) (not p0) p3) (or p6 p6 (not p2) p19 (not p2)) (or (not p16) p8 (not p4) p16 (not p16)) (or p13 (not p17) p3 (not p19) (not p1)) (or p5 p9 p4 p17 p2) (or (not p5) p10 (not p16) (not p10) p17) (or p19 (not p16) p17 (not p15) (not p2)) (or (not p0) p10 (not p0) (not p12) p16) (or p0 p13 p9 p12 p11) (or (not p2) p7 p16 (not p7) (not p8)) (or (not p16) (not p9) (not p17) (not p11) p9) (or p10 (not p10) (not p19) p17 (not p18)) (or (not p9) p2 (not p9) (not p13) (not p1)) (or (not p5) p19 p2 (not p16) p8) (or p0 (not p16) p12 (not p18) (not p10)) (or (not p12) (not p15) (not p5) (not p0) (not p13)) (or p10 p7 (not p18) (not p17) p8) (or p12 (not p12) p3 (not p19) p15) (or (not p12) (not p3) p14 (not p16) p10) (or (not p13) (not p3) (not p12) (not p3) p8) (or (not p7) p16 (not p18) (not p10) (not p0)) (or p8 (not p4) p0 (not p19) (not p16)) (or (not p18) (not p16) (not p0) p10 (not p9)) (or (not p6) p6 p19 p15 p13) (or p3 (not p4) (not p2) (not p2) (not p17)) (or (not p12) p1 (not p4) (not p3) (not p12)) (or (not p13) (not p3) p9 p19 p5) (or (not p19) (not p11) (not p17) p5 p8) (or (not p14) p4 (not p5) (not p3) (not p0)) (or p15 (not p6) (not p18) (not p19) (not p16)) (or (not p13) p8 p9 p11 (not p4)) (or (not p10) p6 p0 (not p7) (not p16)) (or (not p10) (not p1) p5 (not p0) (not p0)) (or (not p13) (not p8) (not p15) (not p19) p18) (or p7 p10 (not p8) p18 (not p16)) (or p15 p14 p6 (not p17) (not p15)) (or (not p12) (not p4) (not p6) (not p1) p10) (or (not p18) p8 p17 p5 (not p15)) (or (not p8) (not p17) p4 (not p14) (not p5)) (or p8 (not p3) (not p0) (not p14) (not p19)) (or (not p4) (not p10) (not p4) p9 p15) (or (not p9) p6 p19 p16 (not p18)) (or p12 p15 (not p16) (not p2) p15) (or (not p7) p7 (not p11) (not p14) p12) (or p10 (not p0) p6 p2 p8) (or (not p12) p12 (not p9) (not p14) (not p7)) (or (not p7) p17 (not p2) p2 (not p19)) (or (not p10) p3 p16 p18 (not p5)) (or p8 (not p2) p4 p17 (not p1)) (or p13 p14 (not p2) p2 p18) (or (not p4) (not p6) (not p0) (not p4) (not p12)) (or (not p6) p0 (not p2) p15 (not p5)) (or p2 p19 (not p11) (not p7) (not p19)) (or p4 p6 (not p13) (not p0) p4) (or (not p17) (not p14) (not p3) (not p5) (not p10)) (or p9 p1 p10 (not p0) (not p14)) (or (not p8) (not p1) p12 p0 p15) (or (not p7) (not p14) (not p17) (not p14) (not p5)) (or p4 p8 p5 p1 p9) (or (not p2) (not p16) (not p8) (not p7) p16) (or p10 p17 (not p19) (not p10) (not p4)) (or p13 (not p9) p2 (not p12) p4) (or p7 p3 (not p16) (not p2) p16) (or p19 p4 (not p18) p12 p9) (or p3 p13 p10 p0 (not p0)) (or p19 (not p12) (not p0) (not p15) p8) (or p16 p6 p16 (not p8) (not p18)) (or p19 (not p11) p11 (not p10) p9) (or p0 (not p12) p5 (not p9) p12) (or (not p9) p0 p10 p12 p15) (or p4 (not p14) (not p10) p5 p8) (or p1 (not p11) (not p17) p2 p10) (or p2 (not p9) (not p3) p4 (not p3)) (or p0 p14 p1 p4 (not p9)) (or (not p17) (not p6) (not p12) p14 (not p13)) (or p17 (not p0) (not p11) p9 (not p1)) (or p18 p9 p7 p10 (not p16)) (or p4 (not p13) (not p15) p8 (not p19)) (or p15 p17 (not p17) p10 p6) (or p1 p2 p2 (not p9) (not p10)) (or (not p11) p13 p16 (not p18) (not p8)) (or p14 (not p15) p9 (not p15) (not p2)) (or p12 p6 (not p6) (not p13) p13) (or p4 p4 (not p15) (not p0) p18) (or p14 (not p3) p19 (not p14) p2) (or p0 p2 p16 p11 (not p0)) (or p0 p13 (not p15) p6 (not p8)) (or (not p12) p18 p12 (not p6) p1) (or (not p10) p16 (not p6) (not p6) (not p15)) (or (not p17) p3 p11 p14 p2) (or (not p12) (not p8) (not p7) (not p3) (not p12)) (or p2 (not p19) (not p12) (not p18) p9) (or p5 (not p8) p2 p18 p10) (or p1 (not p7) (not p18) p6 (not p16)) (or p4 p6 p11 p5 (not p17)) (or (not p15) (not p15) (not p4) p12 p2) (or (not p19) (not p11) (not p19) (not p18) (not p5)))
)
(check-sat)
(get-model)
