(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p4() Bool)
(declare-fun p5() Bool)
(declare-fun p0() Bool)
(declare-fun p6() Bool)
(declare-fun p3() Bool)
(declare-fun p1() Bool)
(declare-fun p8() Bool)
(declare-fun p9() Bool)
(declare-fun p2() Bool)
(declare-fun p7() Bool)
(assert
(and (or p4 (not p5) (not p0) (not p6) p6) (or p6 (not p3) p1 p8 p5) (or p5 (not p5) (not p1) (not p5) (not p3)) (or (not p8) p0 p8 (not p8) p5) (or p4 (not p0) (not p3) p9 p8) (or (not p9) (not p6) p9 (not p8) p4) (or p0 p6 p5 (not p8) p2) (or p5 p5 p5 (not p1) p7) (or p2 p5 p1 p6 p2) (or p1 p6 p8 (not p3) (not p4)) (or p5 (not p8) p1 (not p7) p9) (or p2 p2 p3 (not p6) (not p8)) (or p9 (not p8) (not p6) p9 p9) (or p5 p1 p8 p1 (not p1)) (or p1 (not p0) p1 (not p4) (not p1)) (or p8 (not p4) (not p5) (not p8) (not p7)) (or p8 (not p7) p7 p1 p6) (or (not p8) p6 p2 (not p4) p1) (or (not p1) p1 (not p0) (not p2) p7) (or (not p3) (not p5) p0 (not p4) (not p6)) (or p5 (not p8) (not p2) (not p8) p6) (or p3 p5 p9 p9 p5) (or (not p8) p3 p5 p2 p7) (or (not p9) p2 (not p4) (not p1) (not p3)) (or p5 p2 (not p1) p0 (not p4)) (or p3 (not p4) p9 (not p3) (not p2)) (or (not p2) p2 (not p2) (not p5) p3) (or (not p3) p4 (not p1) (not p7) p2) (or (not p3) (not p5) p4 (not p0) (not p8)) (or p5 (not p3) p2 p7 (not p8)) (or p7 (not p5) p6 p3 (not p5)) (or p6 (not p3) p6 (not p2) (not p0)) (or (not p4) (not p6) p6 p6 p2) (or (not p9) (not p6) (not p4) (not p0) p7) (or p9 (not p2) p9 (not p8) (not p9)) (or (not p5) p1 p4 (not p3) p2) (or (not p0) (not p1) p5 (not p1) (not p9)) (or p3 (not p8) p9 (not p6) (not p4)) (or (not p8) (not p9) p0 (not p7) p7) (or (not p6) p5 (not p5) p5 (not p8)) (or (not p1) p7 (not p6) p1 p0) (or (not p5) p1 p6 (not p2) p1) (or p1 p4 (not p5) (not p9) (not p8)) (or p7 (not p8) (not p0) p7 p4) (or (not p0) p7 (not p1) p4 (not p9)) (or (not p0) (not p4) (not p4) p0 p4) (or p7 p8 (not p0) p7 (not p6)) (or p9 (not p5) p5 p9 p3) (or (not p4) (not p0) (not p7) p5 p3) (or p7 (not p5) p8 p9 p0) (or (not p1) (not p2) (not p4) p2 (not p1)) (or (not p8) (not p5) (not p2) (not p6) p6) (or (not p0) p5 p5 p5 (not p1)) (or p7 p0 (not p2) (not p2) (not p7)) (or p4 (not p5) (not p2) p5 p0) (or (not p2) p1 p5 p7 (not p1)) (or p2 (not p6) (not p6) (not p1) (not p9)) (or p7 p3 (not p9) (not p7) (not p5)) (or (not p7) p1 (not p6) (not p3) p6) (or p5 (not p5) (not p8) (not p2) (not p7)) (or p1 p5 p2 (not p5) (not p6)) (or p3 p5 (not p6) (not p2) (not p0)) (or p7 (not p9) p7 p4 (not p0)) (or (not p3) (not p0) (not p9) (not p3) (not p8)) (or (not p1) (not p6) (not p0) (not p0) p2) (or (not p2) (not p1) p6 p1 (not p1)) (or p4 p0 p2 (not p9) (not p8)) (or p1 (not p7) p8 p2 (not p3)) (or p5 (not p3) (not p2) p6 (not p6)) (or p9 p5 (not p4) (not p2) p6) (or p0 (not p4) p0 (not p7) p3) (or (not p2) p3 p2 p7 p1) (or (not p6) (not p8) (not p2) (not p4) (not p5)) (or p1 p3 p4 p2 (not p5)) (or p2 p2 (not p9) (not p2) p6) (or (not p6) p7 p5 p2 (not p2)) (or (not p5) p6 p9 (not p9) (not p7)) (or p6 p6 p9 (not p2) (not p9)) (or (not p7) p5 p1 p6 p8) (or p0 (not p2) (not p1) (not p3) p6) (or p4 (not p6) p1 (not p3) (not p6)) (or (not p4) p4 p0 (not p7) p8) (or (not p7) (not p4) p3 p7 p7) (or (not p0) (not p5) p6 (not p3) (not p6)) (or p2 (not p3) p5 (not p9) p4) (or p7 (not p0) p2 (not p3) (not p1)) (or p6 p8 (not p8) (not p2) p6) (or p0 p1 p2 (not p5) p2) (or p2 (not p1) p2 p8 p0) (or p2 p8 (not p6) p9 p7) (or (not p4) p2 (not p0) (not p5) (not p7)) (or (not p2) (not p1) p4 (not p9) p8) (or (not p1) (not p2) (not p9) (not p5) p1) (or (not p4) p2 (not p0) p7 (not p8)) (or p1 (not p3) (not p7) (not p5) (not p3)) (or p5 (not p4) (not p0) p1 p3) (or (not p1) p5 (not p3) p3 (not p6)) (or (not p4) (not p4) (not p6) (not p9) (not p4)) (or p2 p3 (not p8) p2 p9) (or (not p6) (not p9) p2 p7 p7) (or p2 p6 (not p6) (not p6) p4) (or (not p3) p6 p8 (not p2) p6) (or p1 (not p5) (not p2) (not p5) p6) (or p7 p3 p3 (not p7) (not p7)) (or p3 (not p5) p3 (not p2) p6) (or (not p1) (not p4) p2 p6 p8) (or p9 p3 p3 p7 p9) (or (not p8) (not p7) p5 (not p9) p3) (or p3 (not p9) p0 (not p0) (not p1)) (or (not p8) p3 (not p3) (not p2) (not p1)) (or p2 (not p8) (not p8) (not p7) (not p1)) (or (not p9) (not p5) p5 (not p6) p6) (or p4 (not p8) p5 (not p1) p4) (or p2 (not p3) p6 (not p2) (not p2)) (or p2 p9 p0 p2 p3) (or (not p6) (not p4) (not p5) (not p1) (not p6)) (or p0 p9 p0 p8 (not p6)) (or p1 p3 p9 (not p0) (not p2)) (or (not p5) (not p6) p5 (not p4) (not p6)) (or p1 (not p3) (not p0) p1 p1) (or (not p3) (not p7) (not p6) (not p4) (not p3)) (or p4 p1 p4 p8 (not p2)) (or (not p3) p5 p9 p2 (not p3)) (or p6 p6 p1 (not p2) p6) (or p4 p5 p5 p3 p4) (or (not p7) (not p6) p2 (not p7) p6) (or p1 p8 (not p4) p5 (not p2)) (or p2 p7 p1 (not p7) (not p3)) (or p8 (not p8) p3 p9 (not p6)) (or (not p6) p3 (not p5) p1 (not p9)) (or p4 (not p0) (not p3) p4 (not p2)) (or (not p1) (not p7) (not p4) (not p9) p3) (or (not p8) p2 p3 (not p1) p9) (or p4 (not p2) (not p9) p4 (not p0)) (or p5 p0 p6 (not p1) (not p0)) (or p5 (not p6) p7 (not p0) p0) (or (not p7) (not p6) (not p7) (not p9) p8) (or p1 (not p0) (not p8) (not p2) (not p0)) (or (not p2) (not p5) p4 (not p9) (not p2)) (or (not p2) (not p3) p1 p4 p0) (or (not p6) (not p4) p0 p9 (not p6)) (or (not p6) p0 (not p2) (not p0) (not p2)) (or (not p9) p3 (not p3) (not p6) (not p2)) (or (not p6) (not p8) p0 (not p9) (not p3)) (or (not p0) p9 p0 (not p4) (not p2)) (or p2 p4 p0 p7 p9) (or (not p6) p5 p9 p6 p7) (or p1 (not p5) (not p9) p3 p9) (or p8 (not p4) (not p1) p4 (not p0)) (or (not p9) (not p1) (not p4) (not p3) (not p9)) (or (not p8) p0 p5 (not p1) (not p5)) (or p2 p9 (not p1) p4 (not p6)) (or p4 p2 p8 (not p9) p7) (or p9 p6 p9 p5 p5) (or p2 p3 p5 (not p0) (not p0)) (or p4 (not p9) (not p7) p1 (not p3)) (or p1 p4 (not p0) (not p3) p4) (or (not p4) (not p2) (not p1) p8 p9) (or p4 (not p4) p0 p8 p1) (or (not p2) (not p5) (not p2) p1 (not p0)) (or p2 p2 p6 p0 p0) (or (not p8) p2 p0 p8 (not p2)) (or (not p1) (not p9) (not p7) p7 (not p3)) (or (not p0) p2 (not p8) (not p2) p8) (or p0 (not p0) p7 p2 (not p8)) (or p3 (not p0) p8 (not p4) (not p5)) (or p8 p6 (not p4) (not p6) p8) (or (not p1) (not p9) (not p9) p7 p9) (or p5 p4 (not p7) p1 (not p9)) (or p0 (not p1) (not p5) (not p7) (not p2)) (or (not p1) p7 p4 p1 (not p0)) (or (not p8) (not p5) (not p3) p6 p8) (or p5 p8 p3 p5 (not p4)) (or (not p6) p8 (not p2) (not p4) (not p6)) (or (not p4) p7 p6 (not p7) (not p0)) (or p1 (not p3) p7 p9 (not p7)) (or (not p6) p7 (not p9) (not p8) (not p8)) (or (not p9) p1 (not p9) (not p6) p4) (or (not p1) p6 p4 (not p0) p0) (or p3 (not p4) (not p6) p5 p2) (or (not p5) (not p9) (not p4) p6 p9) (or (not p9) (not p0) p4 p8 p3) (or (not p9) p7 (not p5) p6 (not p4)) (or p1 p9 p2 (not p4) (not p1)) (or p3 p7 (not p1) p3 (not p0)) (or (not p5) p5 (not p6) (not p9) (not p3)) (or (not p2) (not p8) p4 p2 (not p2)) (or (not p4) p5 p0 p7 (not p7)) (or p7 (not p3) p4 p5 p4) (or (not p5) p5 p6 p0 (not p2)) (or (not p7) (not p3) (not p2) p6 p4) (or p2 (not p1) (not p1) (not p4) p2) (or p9 (not p3) (not p5) (not p6) p0) (or p5 p0 p5 p1 p1) (or p9 (not p9) (not p7) p2 p7) (or (not p7) p1 p8 (not p8) (not p2)) (or p4 p9 p3 p0 (not p0)) (or (not p2) (not p7) (not p4) p5 (not p9)) (or p1 p6 p9 p0 p1) (or p7 (not p7) p8 p7 (not p1)) (or (not p7) p5 p8 p0 (not p2)) (or (not p9) (not p6) (not p5) p7 p8) (or (not p6) p3 p8 (not p2) (not p1)) (or p9 (not p2) (not p6) p3 p7) (or (not p2) p8 p9 (not p9) p3) (or (not p9) p5 p1 (not p8) p9) (or p7 (not p4) p5 p6 (not p3)) (or (not p6) p9 (not p5) p6 (not p0)) (or p4 p4 p9 (not p7) (not p1)))
)
(check-sat)
(get-model)
