(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p13() Bool)
(declare-fun p14() Bool)
(declare-fun p4() Bool)
(declare-fun p2() Bool)
(declare-fun p5() Bool)
(declare-fun p3() Bool)
(declare-fun p7() Bool)
(declare-fun p12() Bool)
(declare-fun p1() Bool)
(declare-fun p11() Bool)
(declare-fun p9() Bool)
(declare-fun p6() Bool)
(declare-fun p10() Bool)
(declare-fun p8() Bool)
(assert
(and (or (not p0) p13 (not p0) p14 (not p14)) (or (not p4) p2 p14 (not p5) (not p3)) (or p7 p12 p1 (not p11) p7) (or (not p1) p4 p9 (not p3) p6) (or (not p1) (not p13) (not p13) p6 p11) (or (not p13) (not p3) p6 p3 (not p14)) (or (not p3) p1 (not p14) p7 p12) (or (not p5) p2 p2 p5 (not p5)) (or (not p0) (not p13) p1 p4 (not p4)) (or (not p13) p7 p2 (not p0) p14) (or p9 p5 p7 p14 (not p13)) (or (not p4) p1 p5 p3 (not p3)) (or p0 p9 (not p11) (not p11) (not p9)) (or p10 (not p1) p5 (not p7) p3) (or p3 (not p6) p11 (not p13) p2) (or (not p1) p5 p9 p2 p4) (or p2 p2 (not p14) p2 p6) (or (not p5) p5 (not p3) (not p14) (not p14)) (or p7 p3 (not p11) p13 p3) (or p10 (not p0) p3 (not p10) p13) (or (not p4) p2 (not p7) p0 p13) (or p7 (not p7) p7 p11 p3) (or p13 p5 (not p2) (not p0) p11) (or (not p6) p8 (not p8) (not p0) (not p1)) (or (not p10) (not p8) p3 (not p14) (not p3)) (or p1 (not p2) (not p9) (not p8) (not p5)) (or (not p12) p13 (not p8) p9 (not p6)) (or (not p3) (not p14) (not p11) (not p13) p3) (or p6 p9 p11 (not p13) p3) (or (not p4) p3 p13 (not p12) p14) (or p12 (not p1) (not p14) (not p9) (not p4)) (or p1 p14 p10 (not p1) p3) (or p2 (not p10) (not p14) (not p5) (not p14)) (or p12 (not p4) (not p12) (not p10) p3) (or p2 (not p6) (not p13) p2 (not p1)) (or (not p12) p9 p6 p1 (not p5)) (or p7 p6 p1 p0 (not p12)) (or (not p10) (not p5) p9 p2 p12) (or p12 (not p1) (not p11) (not p4) (not p6)) (or p8 p0 p10 p8 p8) (or p12 (not p10) (not p1) p10 p9) (or p2 (not p8) p4 p12 (not p5)) (or (not p8) (not p4) p1 (not p3) (not p1)) (or (not p4) (not p0) p4 p1 (not p2)) (or (not p11) p3 (not p5) p2 p7) (or (not p9) (not p9) p3 (not p5) (not p7)) (or p10 (not p11) p8 p14 (not p7)) (or (not p14) (not p14) p2 p10 p1) (or p14 p5 (not p8) p10 (not p0)) (or p13 (not p3) p6 p2 (not p7)) (or (not p7) (not p0) (not p0) p6 (not p10)) (or (not p1) (not p6) (not p7) p7 p12) (or p11 p14 (not p5) (not p13) (not p9)) (or p0 p5 p8 p2 p6) (or p1 (not p0) (not p2) (not p4) (not p3)) (or p2 p1 (not p6) (not p14) p0) (or (not p12) (not p13) (not p13) p0 p8) (or (not p11) (not p2) (not p11) (not p5) p6) (or p13 p12 p10 (not p7) (not p10)) (or p8 p11 p0 p14 p3) (or (not p13) (not p7) p8 (not p10) p9) (or (not p7) (not p11) p9 p2 (not p5)) (or (not p3) (not p2) p14 p5 (not p8)) (or (not p4) p9 p7 p13 (not p9)) (or p4 p3 (not p12) p4 p9) (or (not p1) p13 p1 (not p10) (not p10)) (or p3 (not p10) p0 (not p9) (not p14)) (or (not p0) p11 (not p5) (not p2) p12) (or p5 (not p1) p14 p3 p0) (or p0 (not p6) p11 p14 (not p2)) (or (not p11) (not p0) (not p13) (not p10) (not p9)) (or p7 (not p14) p4 p13 (not p5)) (or p3 p7 (not p9) (not p2) p2) (or (not p6) (not p7) p12 (not p0) p2) (or p7 (not p12) p5 (not p1) p3) (or (not p0) p10 p0 p3 p10) (or (not p13) (not p0) p6 p14 p3) (or (not p4) p5 (not p4) p0 (not p13)) (or (not p4) (not p13) (not p12) (not p7) p11) (or p6 p12 p6 (not p6) (not p13)) (or (not p11) p14 p1 p14 (not p10)) (or (not p3) (not p7) (not p7) p10 (not p3)) (or p5 (not p11) (not p11) p4 p11) (or (not p2) p14 p5 (not p1) p6) (or p10 (not p5) (not p11) p13 (not p3)) (or p0 p2 (not p4) p13 (not p5)) (or (not p6) p6 p4 p9 p14) (or p13 (not p14) p12 (not p11) p8) (or p10 p1 p10 (not p2) (not p12)) (or (not p7) p11 p11 (not p1) p4) (or (not p2) (not p5) (not p13) (not p3) (not p7)) (or p7 (not p6) p0 (not p12) p10) (or (not p13) p11 (not p0) (not p11) p0) (or p4 (not p5) p10 (not p12) p0) (or p14 (not p1) p9 (not p10) (not p6)) (or (not p13) (not p7) p13 p9 (not p4)) (or p0 p10 (not p7) (not p8) p4) (or p12 p8 p2 (not p12) p6) (or p11 (not p5) p6 (not p4) p11) (or p4 (not p6) (not p14) p13 p12) (or (not p14) p6 p3 p0 (not p10)) (or p1 p1 p4 (not p1) (not p9)) (or p10 p2 p10 (not p3) p12) (or (not p6) p1 (not p7) p13 (not p2)) (or p10 p11 (not p7) p4 p1) (or (not p10) (not p2) p1 (not p8) p14) (or (not p12) (not p13) p2 p13 (not p11)) (or (not p0) p14 p5 p4 p0) (or (not p0) (not p3) (not p6) p2 p0) (or p12 (not p4) p6 (not p4) (not p3)) (or p13 p3 p11 p5 p4) (or (not p0) (not p10) (not p2) (not p0) p0) (or p1 p7 (not p1) p13 (not p4)) (or p10 (not p7) p10 p3 (not p6)) (or (not p1) (not p4) (not p8) p10 p9) (or p4 p3 (not p4) p7 p1) (or (not p7) (not p13) (not p14) (not p2) (not p4)) (or p4 (not p9) (not p13) (not p5) (not p6)) (or (not p7) (not p6) p9 p5 p14) (or (not p5) p5 p4 (not p11) (not p7)) (or p12 p13 (not p6) p14 (not p13)) (or p9 p3 (not p1) p14 (not p8)) (or (not p10) p5 p6 p11 p12) (or (not p10) (not p4) p11 p1 (not p1)) (or (not p10) p2 p13 p6 (not p3)) (or p6 (not p9) p8 (not p1) p2) (or p2 p5 p3 p4 p6) (or p6 (not p5) p0 p0 (not p12)) (or (not p13) (not p9) (not p10) p13 (not p11)) (or p12 p1 p12 p0 p8) (or (not p1) (not p7) (not p8) (not p0) (not p12)) (or (not p14) (not p2) p9 (not p3) (not p3)) (or (not p3) (not p3) (not p13) (not p6) p1) (or (not p12) p12 (not p4) p8 (not p3)) (or p0 p11 (not p4) p14 (not p4)) (or (not p5) (not p4) (not p12) (not p11) (not p14)) (or p4 (not p10) p12 (not p11) p3) (or (not p10) p1 p2 (not p3) (not p0)) (or (not p8) (not p6) (not p4) p6 (not p1)) (or (not p7) p7 (not p12) (not p9) p7) (or p14 p5 p3 p4 (not p12)) (or (not p10) (not p7) p6 (not p3) (not p1)) (or (not p8) p1 p6 (not p10) (not p10)) (or p11 (not p4) (not p10) (not p7) (not p8)) (or p14 p14 (not p0) p2 (not p0)) (or (not p14) p12 p14 (not p11) p7) (or (not p3) p8 p3 p11 p4) (or (not p11) (not p8) p11 (not p14) (not p11)) (or p2 (not p8) (not p0) (not p8) p13) (or (not p12) p9 p1 p7 p6) (or p4 (not p0) p2 p8 (not p13)) (or (not p11) (not p14) p2 p9 (not p6)) (or p14 p11 (not p5) p9 (not p3)) (or (not p1) p3 p6 p12 (not p2)) (or (not p6) (not p6) p8 p10 (not p0)) (or p3 p8 (not p1) (not p0) (not p12)) (or (not p10) (not p11) (not p12) p9 (not p13)) (or p5 p3 (not p6) (not p10) (not p6)) (or (not p13) p11 p11 p11 p2) (or (not p4) (not p6) (not p7) (not p13) (not p8)) (or (not p0) p13 p13 p2 (not p12)) (or p9 (not p4) (not p13) p13 (not p1)) (or (not p2) p0 p2 (not p11) p5) (or p9 p2 (not p11) p11 (not p4)) (or (not p0) p2 (not p12) p7 (not p2)) (or (not p6) p14 (not p11) p12 p1) (or (not p11) p12 (not p11) p11 (not p3)) (or p1 (not p6) p14 (not p5) p7) (or p3 p9 (not p2) p11 (not p2)) (or p0 p5 (not p6) p2 p14) (or (not p4) (not p5) (not p9) (not p0) p4) (or p10 p0 p5 (not p5) p5) (or p0 (not p2) (not p0) (not p3) p11) (or p13 (not p12) (not p12) (not p2) p12) (or p4 (not p14) (not p2) (not p9) (not p10)) (or (not p12) p4 p4 (not p9) (not p0)) (or p7 (not p14) p13 p14 p5) (or p6 p2 (not p14) (not p6) p0) (or (not p9) p6 p5 p12 (not p10)) (or (not p12) (not p4) p4 p10 p2) (or p14 p14 p4 p1 (not p5)) (or (not p4) (not p3) (not p5) (not p13) (not p12)) (or (not p6) p2 p11 p13 (not p6)) (or p14 (not p8) p12 (not p3) (not p4)) (or (not p14) p5 (not p3) (not p5) (not p1)) (or (not p8) (not p11) p9 (not p1) p8) (or (not p12) p4 (not p10) p5 p14) (or p9 (not p9) (not p8) p9 (not p6)) (or p10 (not p12) (not p14) p6 p10) (or p11 (not p9) p13 p7 (not p8)) (or p2 p7 (not p13) p6 p6) (or p10 p5 (not p12) p13 (not p2)) (or (not p13) p8 (not p5) p10 p7) (or (not p0) (not p11) (not p5) (not p0) (not p3)) (or (not p12) p3 p8 p7 p14) (or (not p7) (not p14) p7 p10 p2) (or (not p5) p5 p0 (not p13) (not p9)) (or p13 (not p8) (not p13) p0 p8) (or (not p5) p11 (not p3) (not p10) p13) (or p13 (not p4) p9 (not p6) (not p10)) (or p9 (not p9) (not p12) p14 p12) (or p0 (not p4) (not p5) p11 (not p7)) (or p1 p10 p0 p5 p1) (or (not p2) (not p10) (not p1) (not p7) p7) (or p9 (not p10) p0 p3 (not p0)) (or p10 (not p7) (not p0) p3 (not p7)) (or (not p8) (not p14) p10 (not p12) p4) (or p8 (not p0) p2 p11 p7) (or (not p0) (not p3) p13 (not p14) p1) (or p11 p0 (not p11) (not p3) p10) (or (not p14) p8 (not p3) (not p14) (not p5)) (or (not p0) p7 (not p14) p6 (not p3)) (or p8 (not p5) p9 (not p12) p5) (or (not p1) (not p12) (not p13) (not p10) p1) (or (not p5) (not p0) p2 p7 (not p14)) (or (not p14) (not p7) (not p1) p10 p11) (or (not p3) p14 p5 (not p4) p4) (or p10 p1 p11 (not p6) p2) (or p12 p9 (not p6) p10 p12) (or p1 p8 p12 p1 p0) (or p4 p7 (not p6) p7 p11) (or p5 (not p0) (not p8) (not p13) p12) (or (not p8) p0 (not p11) (not p1) (not p7)) (or p10 p6 (not p1) p5 (not p3)) (or (not p1) p12 p5 p1 (not p7)) (or (not p1) p10 (not p5) p5 (not p11)) (or (not p2) (not p12) (not p4) p5 (not p2)) (or (not p6) (not p2) (not p13) (not p13) (not p10)) (or (not p2) p14 (not p3) (not p14) (not p6)) (or p11 (not p14) p1 (not p14) (not p6)) (or p2 (not p8) p1 p4 (not p5)) (or p14 (not p14) (not p5) (not p6) (not p8)) (or p4 p5 p14 p4 (not p8)) (or (not p3) p9 p11 p9 p7) (or (not p10) (not p9) (not p12) (not p6) p12) (or (not p12) (not p10) p10 (not p7) (not p2)) (or p7 (not p3) p13 (not p12) p12) (or p4 (not p8) (not p14) p12 (not p14)) (or p8 (not p13) p7 p11 (not p10)) (or p7 p2 p11 p10 p9) (or (not p9) p6 (not p1) (not p0) p0) (or p14 p11 p14 (not p9) p2) (or p1 (not p10) p4 p4 p10) (or (not p9) p8 (not p7) p2 (not p13)) (or (not p10) p13 p5 p10 (not p11)) (or (not p11) p5 p11 (not p0) (not p7)) (or (not p13) (not p2) (not p10) (not p13) p1) (or (not p7) (not p12) p7 (not p11) (not p8)) (or (not p7) (not p5) (not p3) (not p14) p13) (or p14 p6 (not p13) (not p12) (not p5)) (or (not p0) (not p14) p13 (not p11) (not p6)) (or p14 (not p11) (not p11) (not p9) (not p3)) (or (not p12) (not p10) p4 (not p0) p2) (or p0 p5 p11 (not p1) p11) (or p0 (not p3) (not p12) (not p5) p7) (or (not p10) p9 p3 p13 p5) (or (not p0) (not p6) (not p8) p0 p5) (or (not p9) p8 p4 (not p5) (not p13)) (or p8 p14 (not p3) p14 (not p8)) (or p12 p10 (not p11) (not p13) p1) (or p6 p3 (not p2) (not p13) p13) (or (not p0) (not p6) (not p14) (not p3) (not p4)) (or (not p11) p0 (not p7) (not p3) (not p7)) (or p12 p12 p2 p10 p2) (or p9 (not p5) p3 p8 (not p14)) (or (not p10) p5 (not p13) (not p13) p7) (or (not p5) (not p6) p2 (not p8) p9) (or p1 (not p7) (not p9) p0 p14) (or p7 (not p5) p11 p8 p13) (or (not p3) p9 (not p8) p12 (not p14)) (or (not p1) p13 p4 p10 (not p0)) (or p3 (not p7) p12 (not p13) (not p7)) (or (not p5) p2 (not p4) p8 (not p13)) (or p2 p12 p4 (not p13) (not p14)) (or (not p7) p5 p4 (not p10) p5) (or (not p10) p13 p5 p2 (not p13)) (or (not p13) p8 (not p3) (not p3) (not p9)) (or (not p4) p9 (not p9) p11 (not p13)) (or p2 (not p6) (not p3) (not p2) p1) (or (not p9) (not p1) p3 p13 p13) (or p5 (not p1) p1 p9 p0) (or (not p14) p13 (not p11) p5 p3) (or p14 (not p6) (not p13) p4 p2) (or p6 p11 (not p5) (not p13) (not p0)) (or p0 p9 p0 (not p12) p11) (or p5 p13 (not p9) p0 (not p0)) (or p10 (not p1) p6 (not p12) (not p5)) (or p2 (not p8) p0 (not p9) p4) (or (not p9) (not p3) p13 (not p13) (not p10)) (or (not p3) p12 p7 p12 (not p0)) (or (not p5) (not p4) (not p9) (not p10) p7) (or p11 (not p12) p4 (not p11) p2) (or p3 p5 p9 (not p4) p11) (or (not p8) (not p3) p8 p1 p10) (or (not p8) p1 p10 (not p7) (not p4)) (or (not p14) (not p9) p9 p5 p3) (or p0 p12 (not p6) (not p14) p3) (or p3 (not p13) (not p4) p1 (not p1)) (or (not p4) p7 (not p12) p8 p0) (or p14 (not p1) (not p3) (not p14) (not p1)) (or p10 (not p5) (not p14) p12 (not p10)) (or (not p8) (not p14) p8 p2 p0) (or (not p2) p10 (not p4) p10 (not p6)) (or (not p6) (not p10) (not p8) p8 (not p11)) (or p6 p7 (not p11) p10 p0) (or (not p13) (not p10) (not p10) (not p6) (not p14)) (or p9 p14 (not p11) (not p14) p7) (or (not p4) (not p8) (not p0) (not p9) (not p4)) (or (not p6) (not p14) p11 p8 (not p4)) (or p2 (not p0) (not p3) (not p4) (not p8)) (or p4 (not p7) p13 (not p10) (not p2)) (or (not p2) p7 p9 (not p8) p3))
)
(check-sat)
(get-model)
