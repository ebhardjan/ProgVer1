(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p6() Bool)
(declare-fun p15() Bool)
(declare-fun p17() Bool)
(declare-fun p1() Bool)
(declare-fun p29() Bool)
(declare-fun p7() Bool)
(declare-fun p21() Bool)
(declare-fun p3() Bool)
(declare-fun p27() Bool)
(declare-fun p26() Bool)
(declare-fun p2() Bool)
(declare-fun p28() Bool)
(declare-fun p0() Bool)
(declare-fun p22() Bool)
(declare-fun p5() Bool)
(declare-fun p12() Bool)
(declare-fun p19() Bool)
(declare-fun p23() Bool)
(declare-fun p24() Bool)
(declare-fun p4() Bool)
(declare-fun p16() Bool)
(declare-fun p11() Bool)
(declare-fun p10() Bool)
(declare-fun p13() Bool)
(declare-fun p25() Bool)
(declare-fun p9() Bool)
(declare-fun p20() Bool)
(declare-fun p14() Bool)
(declare-fun p18() Bool)
(declare-fun p8() Bool)
(assert
(and (or (not p6) (not p15) (not p17)) (or (not p1) p29 p7) (or p21 (not p3) (not p27)) (or p26 p2 p17) (or p28 p0 p3) (or (not p28) p22 p5) (or (not p22) p12 p15) (or p19 (not p21) (not p17)) (or (not p1) p3 p23) (or (not p12) (not p15) (not p24)) (or p4 p22 p23) (or p16 p15 p12) (or p2 p11 p12) (or p26 (not p10) p0) (or p29 p28 (not p26)) (or p5 p29 p13) (or (not p6) p4 p25) (or p19 p23 (not p29)) (or (not p3) (not p13) p12) (or p15 p15 p29) (or p9 (not p2) p10) (or (not p22) p19 (not p16)) (or (not p28) (not p21) (not p25)) (or p27 (not p20) p21) (or p17 (not p10) (not p29)) (or (not p13) p21 p19) (or (not p19) p6 (not p14)) (or (not p15) p2 (not p15)) (or p13 p17 p15) (or p23 (not p3) p27) (or (not p11) p1 p24) (or (not p28) p14 p10) (or (not p22) p9 p18) (or p14 p9 (not p6)) (or (not p2) (not p0) (not p0)) (or p11 p16 (not p20)) (or (not p4) (not p26) (not p17)) (or (not p2) (not p29) (not p18)) (or (not p11) p22 (not p28)) (or (not p24) p20 p8) (or (not p23) p4 (not p17)) (or p11 (not p18) (not p14)) (or (not p2) p1 (not p4)) (or (not p18) p21 (not p15)) (or (not p7) (not p1) p6) (or p3 (not p12) (not p29)) (or p12 p0 p16) (or p12 (not p19) (not p25)) (or (not p23) p29 (not p23)) (or (not p26) p8 p29) (or (not p9) p3 (not p9)) (or p6 p12 (not p18)) (or (not p15) (not p11) p28) (or (not p24) (not p2) p10) (or (not p29) p25 p13) (or (not p24) p13 p28) (or (not p4) p20 (not p25)) (or (not p28) p19 (not p13)) (or p5 p12 p15) (or p29 p8 p16) (or (not p14) p23 p17) (or (not p23) (not p22) (not p27)) (or (not p27) (not p26) p7) (or p11 p4 (not p28)) (or p3 p10 (not p10)) (or p15 (not p23) (not p15)) (or p7 (not p17) p24) (or p25 (not p26) p10) (or (not p1) (not p21) (not p25)) (or p25 p23 (not p10)) (or p1 (not p14) (not p5)) (or (not p14) p25 (not p4)) (or (not p24) (not p22) p20) (or (not p14) p18 (not p25)) (or p12 p26 p9) (or (not p14) p12 p10) (or p6 (not p27) (not p13)) (or p10 p17 p22) (or (not p28) (not p19) p19) (or (not p23) (not p18) (not p12)) (or (not p10) p28 (not p22)) (or (not p2) p26 p24) (or (not p22) (not p5) (not p9)) (or p12 (not p26) (not p16)) (or (not p29) p28 (not p19)) (or (not p19) p7 p23) (or p15 p14 (not p15)) (or p3 (not p16) p16) (or p29 (not p19) (not p13)) (or p26 (not p19) (not p22)) (or p0 p10 p6) (or (not p10) (not p29) p20) (or p14 (not p26) (not p29)) (or p9 (not p10) p21) (or p14 p8 (not p1)) (or (not p6) p26 (not p17)) (or (not p15) (not p11) p19) (or p6 (not p12) (not p12)) (or p29 (not p29) (not p23)) (or (not p13) p4 p21) (or p4 p22 (not p1)) (or (not p27) (not p25) p6) (or p8 p23 (not p10)) (or (not p24) (not p3) (not p13)) (or (not p20) p17 (not p9)) (or p2 p14 (not p29)) (or p2 (not p7) p21) (or p10 (not p26) (not p23)) (or p1 (not p12) p24) (or p12 (not p29) p14) (or (not p11) (not p11) p23) (or p12 (not p6) p25) (or (not p1) p4 (not p29)) (or (not p2) (not p9) (not p1)) (or (not p10) (not p18) p15) (or (not p14) (not p6) (not p25)) (or (not p6) p3 p22) (or (not p5) (not p11) (not p24)) (or (not p29) (not p5) p13) (or p4 (not p29) (not p27)) (or p24 (not p28) (not p0)) (or p20 p18 p18) (or p4 p26 (not p9)) (or (not p16) (not p22) p21) (or (not p8) (not p25) (not p17)) (or p17 (not p29) p20) (or (not p23) p28 p10) (or (not p15) p2 (not p11)))
)
(check-sat)
(get-model)
