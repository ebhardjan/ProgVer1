(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p24() Bool)
(declare-fun p11() Bool)
(declare-fun p13() Bool)
(declare-fun p28() Bool)
(declare-fun p16() Bool)
(declare-fun p21() Bool)
(declare-fun p9() Bool)
(declare-fun p18() Bool)
(declare-fun p15() Bool)
(declare-fun p5() Bool)
(declare-fun p14() Bool)
(declare-fun p19() Bool)
(declare-fun p29() Bool)
(declare-fun p2() Bool)
(declare-fun p20() Bool)
(declare-fun p6() Bool)
(declare-fun p26() Bool)
(declare-fun p3() Bool)
(declare-fun p22() Bool)
(declare-fun p25() Bool)
(declare-fun p23() Bool)
(declare-fun p12() Bool)
(declare-fun p7() Bool)
(declare-fun p27() Bool)
(declare-fun p17() Bool)
(declare-fun p10() Bool)
(declare-fun p1() Bool)
(declare-fun p4() Bool)
(declare-fun p0() Bool)
(declare-fun p8() Bool)
(assert
(and (or p24 p24 p11) (or p13 (not p24) p28) (or p16 p21 p9) (or p18 p15 (not p13)) (or (not p5) p14 p19) (or p29 p2 p18) (or p20 (not p21) p19) (or (not p5) (not p6) (not p26)) (or p14 (not p18) p24) (or (not p13) (not p3) (not p2)) (or (not p9) (not p6) (not p18)) (or p18 (not p21) p22) (or p20 (not p24) (not p14)) (or p25 p19 (not p15)) (or p23 (not p12) p16) (or p7 (not p26) (not p24)) (or p13 p3 p23) (or p18 (not p27) p12) (or (not p23) (not p2) p27) (or (not p17) (not p10) p1) (or (not p4) (not p18) (not p22)) (or p21 (not p24) p25) (or (not p27) (not p27) p7) (or (not p21) p17 (not p13)) (or (not p13) (not p14) (not p23)) (or (not p23) p25 p5) (or (not p0) (not p29) p14) (or (not p29) (not p2) (not p5)) (or p17 (not p4) (not p1)) (or (not p6) (not p24) p12) (or p8 p12 p26) (or p10 (not p19) p10) (or (not p21) (not p25) (not p15)) (or (not p1) (not p9) (not p1)) (or (not p25) (not p16) p29) (or p11 p4 p8) (or (not p2) p6 (not p12)) (or (not p12) p8 p23) (or (not p25) p28 (not p14)) (or (not p5) (not p5) (not p12)) (or p14 p9 p17) (or p8 p25 p28) (or p1 (not p3) (not p23)) (or (not p20) (not p24) (not p15)) (or (not p23) p20 (not p17)) (or p16 p2 (not p13)) (or (not p0) (not p1) p19) (or p29 p10 (not p24)) (or (not p5) p20 p14) (or (not p21) (not p17) p20) (or (not p26) (not p27) (not p10)) (or (not p4) p17 (not p26)) (or (not p11) (not p22) (not p15)) (or p6 p7 p16) (or (not p14) p7 p9) (or p17 (not p17) (not p1)) (or (not p25) (not p28) p12) (or p15 p7 p17) (or (not p29) (not p7) (not p9)) (or p8 (not p4) (not p22)) (or (not p23) (not p9) p24) (or p22 p10 p6) (or (not p29) p27 p23) (or p29 (not p18) p29) (or p11 (not p8) (not p26)) (or (not p0) (not p18) (not p7)) (or (not p6) (not p26) p21) (or (not p13) (not p26) (not p8)) (or (not p2) (not p20) (not p19)) (or (not p5) p1 (not p8)) (or (not p15) (not p9) (not p13)) (or p17 (not p1) (not p25)) (or (not p6) (not p23) p5) (or p18 p12 (not p15)) (or p26 (not p14) p16) (or p17 p9 (not p22)) (or p20 p24 p11) (or p15 p21 p19) (or p17 p19 (not p15)) (or p6 (not p14) (not p2)) (or p17 p25 (not p18)) (or p22 (not p26) p20) (or p21 (not p20) (not p28)) (or p6 p22 (not p14)) (or p26 (not p16) (not p16)) (or p11 (not p15) p0) (or (not p9) (not p25) p7) (or p20 p2 p28) (or (not p3) p27 (not p14)) (or (not p13) (not p10) (not p8)) (or p10 (not p19) p13) (or p29 p23 (not p13)) (or p2 p23 p16) (or (not p3) p15 (not p19)) (or p7 (not p4) p18) (or p23 p4 p27) (or (not p12) p11 p9) (or p21 p13 (not p6)) (or (not p6) (not p15) p11) (or (not p4) (not p2) p13) (or (not p22) (not p28) p8) (or (not p15) (not p19) p18) (or (not p9) p27 (not p0)) (or (not p16) (not p15) p8) (or p5 (not p26) p29) (or p28 (not p5) (not p1)) (or p8 (not p7) p17) (or p11 (not p23) (not p11)) (or p7 p17 (not p15)) (or (not p4) p20 p27) (or (not p19) p19 (not p12)) (or (not p6) p24 (not p8)) (or p13 p7 (not p8)) (or p28 (not p9) p14) (or p23 (not p29) (not p23)) (or (not p18) (not p26) p5) (or p9 (not p6) (not p24)) (or (not p25) p20 p27) (or (not p3) p7 p9) (or p22 (not p7) (not p29)) (or p24 (not p22) p5) (or p26 p15 p3) (or p12 (not p24) (not p15)) (or p8 (not p16) (not p27)) (or (not p29) p16 (not p14)) (or p9 p11 (not p0)) (or (not p27) (not p5) p17) (or p4 (not p2) p22))
)
(check-sat)
(get-model)
