(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p15() Bool)
(declare-fun p16() Bool)
(declare-fun p27() Bool)
(declare-fun p11() Bool)
(declare-fun p25() Bool)
(declare-fun p12() Bool)
(declare-fun p10() Bool)
(declare-fun p9() Bool)
(declare-fun p4() Bool)
(declare-fun p18() Bool)
(declare-fun p29() Bool)
(declare-fun p1() Bool)
(declare-fun p23() Bool)
(declare-fun p7() Bool)
(declare-fun p6() Bool)
(declare-fun p22() Bool)
(declare-fun p3() Bool)
(declare-fun p13() Bool)
(declare-fun p20() Bool)
(declare-fun p17() Bool)
(declare-fun p8() Bool)
(declare-fun p24() Bool)
(declare-fun p2() Bool)
(declare-fun p19() Bool)
(declare-fun p0() Bool)
(declare-fun p5() Bool)
(declare-fun p28() Bool)
(declare-fun p14() Bool)
(declare-fun p21() Bool)
(declare-fun p26() Bool)
(assert
(and (or p15 (not p16) (not p27) (not p11) p25) (or p16 p27 p12 p10 (not p9)) (or (not p4) (not p18) (not p29) p1 (not p23)) (or p7 p6 p22 (not p3) (not p25)) (or (not p29) (not p13) (not p20) p17 (not p25)) (or (not p8) p20 (not p24) (not p9) p7) (or (not p4) p27 p23 p20 p20) (or (not p7) (not p27) p23 p10 p20) (or p15 (not p7) (not p29) p4 (not p25)) (or p9 p4 p11 p2 (not p3)) (or (not p13) p19 p3 (not p0) p3) (or p12 p9 (not p2) (not p8) p11) (or (not p3) p24 p3 (not p27) (not p5)) (or (not p11) (not p11) (not p10) (not p10) (not p22)) (or (not p6) p1 (not p0) p23 (not p27)) (or (not p2) p23 p13 p11 p8) (or (not p15) p29 p11 (not p4) p29) (or p3 (not p28) p4 (not p24) (not p22)) (or p9 p27 (not p27) p11 (not p13)) (or p27 (not p6) p18 (not p12) p28) (or (not p2) p23 p3 p11 (not p8)) (or (not p29) (not p2) p8 p5 p17) (or (not p9) (not p5) (not p22) p10 (not p29)) (or (not p3) (not p2) (not p20) p27 p15) (or (not p4) p14 p0 p12 p10) (or p15 (not p17) (not p7) p12 (not p14)) (or (not p1) p1 (not p23) (not p20) p24) (or p29 p6 p2 (not p23) p16) (or p17 p6 p15 (not p22) (not p7)) (or (not p6) p25 (not p0) p17 p19) (or p24 (not p24) (not p15) p27 p21) (or p3 p18 p17 p3 (not p18)) (or (not p15) (not p26) (not p14) (not p6) (not p23)) (or p4 (not p14) p10 (not p26) (not p25)) (or (not p8) (not p5) (not p0) p21 p24) (or (not p8) (not p23) (not p2) p7 p7) (or p4 p13 (not p1) (not p14) p15) (or p7 p5 p7 (not p28) p4) (or p13 p23 p25 p8 p9) (or (not p5) (not p22) p9 p26 (not p7)) (or p10 p16 p29 (not p9) p26) (or p26 (not p26) p10 (not p14) p21) (or (not p1) p20 (not p24) p23 p2) (or (not p28) (not p10) p29 (not p10) (not p3)) (or (not p21) (not p24) (not p7) (not p21) p12) (or p29 p8 (not p27) (not p8) p26) (or p9 p22 (not p19) (not p14) (not p28)) (or (not p22) p28 p18 (not p7) (not p0)) (or (not p3) (not p24) p19 p1 (not p28)) (or p29 p7 p0 p8 (not p16)) (or (not p27) p22 p15 (not p11) p0) (or p17 (not p9) (not p16) (not p26) (not p13)) (or (not p15) (not p27) (not p29) p27 p16) (or (not p23) p2 (not p20) p29 p10) (or p13 p4 (not p23) (not p10) (not p26)) (or p13 p18 p7 (not p14) p0) (or (not p13) (not p28) p23 p19 p7) (or (not p15) p15 (not p14) (not p10) (not p7)) (or p7 (not p16) (not p7) (not p20) (not p13)) (or (not p16) p27 p16 p1 p8) (or (not p18) (not p11) p17 (not p23) (not p1)) (or (not p2) (not p24) (not p29) p19 p7) (or (not p15) (not p13) p29 p9 p14) (or p23 (not p14) (not p15) (not p21) p12) (or (not p12) p27 p5 (not p18) p2) (or p24 (not p29) p25 (not p15) (not p5)) (or p28 (not p3) (not p17) p1 (not p5)) (or (not p14) (not p5) p20 p10 (not p12)) (or p5 (not p14) p21 p13 (not p6)) (or p20 p0 p5 p14 p13) (or (not p17) (not p26) p18 p18 (not p13)) (or p22 (not p26) p19 p22 (not p23)) (or (not p7) p14 (not p9) p18 (not p2)) (or (not p8) (not p24) p8 p21 p16) (or (not p23) (not p23) p5 (not p28) (not p13)) (or p18 p23 (not p18) p25 (not p14)) (or (not p22) (not p1) p24 (not p2) (not p23)) (or p20 (not p18) p15 (not p4) (not p25)) (or p2 (not p22) (not p7) p15 p19) (or (not p19) p21 (not p17) p8 (not p28)) (or p1 p9 p15 p7 (not p9)) (or p22 p6 p3 p0 (not p23)) (or (not p29) p5 (not p0) p20 p1) (or (not p16) p25 (not p27) p22 p16) (or (not p14) p13 p9 (not p13) (not p13)) (or (not p5) p4 p12 (not p11) p0) (or (not p0) (not p23) p19 (not p15) (not p9)) (or p26 (not p13) p10 p1 (not p16)) (or (not p9) p25 p2 (not p8) (not p11)) (or p10 (not p22) (not p28) (not p12) (not p2)) (or (not p29) (not p14) p7 (not p12) p5) (or (not p2) (not p18) (not p28) (not p28) p26) (or p13 (not p11) (not p5) p11 p2) (or p12 p26 p8 p27 (not p9)) (or p28 (not p14) (not p5) (not p9) (not p29)) (or (not p21) p5 p25 p12 p10) (or (not p9) (not p22) (not p7) p18 p0) (or p6 p27 p1 p2 p9) (or p17 (not p22) p14 (not p27) p4) (or (not p14) p6 p4 p23 p28) (or (not p21) (not p27) (not p18) p25 p20) (or p2 p17 p3 p4 (not p28)) (or p21 (not p19) (not p18) p8 p5) (or p16 p22 (not p27) (not p23) (not p12)) (or p3 (not p14) (not p26) (not p2) (not p21)) (or p3 (not p21) (not p28) (not p5) p6) (or p5 p7 p28 p3 p1) (or (not p29) p1 (not p11) p8 (not p5)) (or p25 (not p20) p11 p17 (not p1)) (or (not p25) p27 p24 p1 p29) (or (not p11) (not p12) p21 (not p22) (not p24)) (or (not p28) (not p28) (not p22) p24 (not p5)) (or p13 p22 (not p22) (not p17) (not p20)) (or (not p17) (not p15) (not p0) (not p8) (not p26)) (or p1 (not p12) (not p12) (not p19) p20) (or p12 p0 (not p29) p1 (not p11)) (or (not p22) (not p19) (not p19) (not p19) p26) (or (not p14) p11 (not p26) p18 (not p20)) (or (not p25) p22 (not p22) p27 p3) (or (not p15) p27 (not p14) (not p28) p9) (or (not p21) p29 (not p9) (not p18) p11) (or (not p9) (not p12) p20 p1 (not p4)) (or p25 p15 p13 p17 p26) (or (not p8) (not p21) (not p6) p7 (not p2)) (or (not p11) (not p29) p13 (not p29) (not p21)) (or (not p27) p29 (not p9) p2 (not p7)) (or p4 (not p18) (not p21) p21 (not p9)) (or (not p7) (not p12) p7 (not p16) p21) (or p16 (not p19) (not p14) (not p12) (not p21)) (or p27 p3 p23 p15 p7) (or (not p13) p23 (not p3) (not p28) (not p23)) (or (not p12) (not p7) (not p25) p4 (not p19)) (or (not p26) (not p29) p23 (not p27) p22) (or (not p9) p6 (not p17) p12 p26) (or p27 p13 (not p29) (not p19) p9) (or p17 (not p1) p20 (not p13) p6) (or (not p4) (not p21) (not p17) (not p7) (not p12)) (or (not p0) (not p25) (not p9) p18 (not p1)) (or (not p13) p8 p15 (not p27) p1) (or (not p1) (not p1) (not p5) (not p25) p7) (or (not p6) p2 (not p1) p6 (not p28)) (or p28 p3 p22 (not p27) p3) (or (not p10) (not p27) (not p26) (not p13) (not p8)) (or p12 p26 (not p2) (not p27) p14) (or p26 (not p5) (not p17) (not p11) p23) (or p26 (not p27) (not p18) p6 p16) (or (not p16) p5 p14 p28 p20) (or p8 p11 p15 (not p24) (not p11)) (or p16 p22 (not p16) p24 p14) (or (not p11) p14 (not p27) p2 (not p9)) (or p6 (not p1) p4 (not p1) (not p0)) (or p10 p25 p13 (not p5) p26) (or p21 p28 (not p9) (not p7) p25) (or (not p22) (not p22) p4 (not p16) (not p8)) (or p29 p5 p9 p5 p28) (or (not p28) p29 (not p23) (not p17) (not p26)) (or p6 p13 (not p23) p22 (not p4)) (or (not p22) (not p2) p20 p0 p10) (or p16 p16 p27 p20 (not p26)) (or (not p6) p22 p23 (not p25) p10) (or (not p25) p8 (not p29) (not p7) (not p15)) (or p1 p9 (not p15) (not p10) p25) (or (not p21) (not p26) p21 (not p25) p8) (or (not p20) p6 p16 p13 (not p6)) (or p16 (not p13) p28 p23 (not p9)) (or (not p4) p17 p13 (not p12) (not p3)) (or (not p15) p16 (not p6) p5 p15) (or (not p13) p28 (not p23) (not p13) (not p4)) (or (not p5) p27 (not p5) p13 (not p13)) (or (not p16) p19 (not p8) (not p18) p17) (or p29 p12 (not p20) p5 (not p29)) (or (not p8) p1 p11 (not p20) p9) (or (not p21) p5 (not p10) (not p13) p2) (or p16 (not p1) (not p13) p16 (not p0)) (or p5 p23 (not p7) (not p9) (not p19)) (or p29 p3 (not p16) (not p28) p10) (or p11 p5 p23 (not p21) (not p0)) (or p2 p23 (not p4) (not p19) p7) (or p7 p29 p10 (not p0) p18) (or (not p9) (not p13) (not p14) p20 (not p1)) (or p28 (not p19) (not p13) p8 (not p17)) (or p13 (not p9) (not p18) (not p10) (not p12)) (or (not p3) (not p16) (not p25) (not p26) (not p2)) (or (not p4) (not p26) (not p8) (not p3) p24) (or p5 p23 (not p9) p20 p6) (or p3 (not p0) p0 p1 (not p22)) (or (not p27) p20 p4 (not p21) p23) (or (not p28) (not p15) p15 (not p13) p9) (or (not p9) p5 (not p26) (not p8) p7) (or (not p12) p1 (not p29) p20 p26) (or p12 p11 (not p3) (not p10) p2) (or (not p25) (not p14) (not p27) (not p14) p23) (or (not p0) (not p20) p6 (not p28) (not p7)) (or (not p8) (not p15) p14 (not p8) p25) (or p4 p19 (not p14) p10 (not p3)) (or p23 p27 p6 p15 (not p28)) (or (not p4) (not p21) p5 (not p22) p10) (or (not p19) (not p17) p9 (not p9) p22) (or (not p2) (not p5) (not p9) (not p11) p23) (or (not p15) (not p16) p24 p28 p29) (or (not p25) p24 p3 p21 p18) (or (not p7) p12 p20 p19 (not p16)) (or p22 p7 p6 (not p18) p8) (or (not p27) (not p11) p0 p6 p10) (or (not p14) (not p16) p29 (not p0) (not p24)) (or p5 p20 p9 (not p25) (not p10)) (or (not p3) (not p12) p13 p16 (not p1)) (or (not p0) (not p22) p25 (not p15) (not p14)) (or (not p11) (not p8) (not p5) p20 p26) (or (not p25) (not p17) p4 (not p28) (not p4)) (or p2 (not p22) (not p0) p19 p24) (or p25 p16 p25 (not p7) (not p20)) (or (not p3) (not p11) p14 p25 (not p26)) (or p1 (not p10) p1 p27 (not p4)) (or (not p11) (not p20) p0 p10 p12) (or (not p18) p2 (not p23) (not p26) p5) (or p1 p14 (not p10) p5 p9) (or (not p12) p9 p15 (not p10) p13) (or (not p21) p14 (not p0) (not p6) (not p19)) (or p13 (not p19) p8 (not p7) p9) (or (not p10) p10 (not p28) p3 p2) (or (not p4) (not p18) p24 p23 (not p24)) (or (not p18) p22 p23 (not p7) p12) (or (not p11) p27 (not p6) (not p29) (not p4)) (or p19 (not p27) (not p15) p25 p23) (or (not p17) (not p12) p23 (not p14) (not p22)) (or (not p15) (not p0) p20 (not p4) (not p25)) (or p22 p25 p18 p19 p21) (or p1 (not p21) (not p8) (not p25) (not p6)) (or p10 p7 p22 (not p12) p27) (or p6 (not p9) (not p18) (not p6) (not p22)) (or p26 (not p10) p5 p4 p7) (or (not p5) p4 (not p19) (not p4) p9) (or p25 p20 (not p17) (not p22) p10) (or p4 p1 (not p1) p2 (not p3)) (or (not p24) p15 (not p23) p6 (not p2)) (or (not p29) (not p16) p22 p13 (not p4)) (or p8 p22 (not p14) (not p15) p27) (or (not p26) p1 (not p22) (not p24) p25) (or (not p16) (not p14) p8 (not p5) p11) (or (not p27) (not p18) (not p11) (not p11) p23) (or (not p11) p7 p11 p17 p25) (or (not p7) p10 (not p21) p25 (not p15)) (or (not p19) (not p25) p14 p10 p19) (or (not p9) p24 (not p19) (not p25) (not p26)) (or p14 (not p28) p0 (not p13) p12) (or (not p26) p13 p5 p22 (not p0)) (or p26 (not p6) (not p20) (not p5) (not p21)) (or p17 p24 (not p1) p29 p17) (or p17 (not p20) (not p6) p6 p7) (or (not p16) (not p17) p26 (not p21) p16) (or (not p7) p16 (not p28) (not p24) p20) (or (not p18) (not p1) (not p21) (not p14) p28) (or p15 p19 p17 (not p13) (not p12)) (or (not p0) (not p0) p23 (not p5) (not p6)) (or p24 p6 p2 p9 p14) (or (not p21) (not p27) (not p28) p3 p3) (or p16 (not p27) p11 (not p7) (not p26)) (or p16 p11 p18 p5 p23) (or (not p1) (not p0) (not p22) p28 (not p11)) (or (not p21) p15 p13 p4 p17) (or (not p18) (not p17) (not p23) (not p4) p29) (or (not p23) (not p10) (not p28) (not p8) (not p8)) (or (not p25) p1 (not p11) (not p29) (not p3)) (or (not p20) (not p1) p6 p4 p8) (or (not p22) p20 p22 p7 p19) (or (not p9) (not p7) (not p6) p6 (not p2)) (or p27 (not p12) (not p8) (not p9) (not p14)) (or (not p23) (not p10) (not p7) (not p19) p23) (or p29 (not p27) p15 (not p10) (not p24)) (or (not p2) p2 (not p10) p18 (not p23)) (or p7 (not p10) p26 (not p7) p26) (or (not p29) p7 p8 p8 p3) (or p12 (not p14) p21 p16 p3) (or p19 p8 p9 (not p6) (not p16)) (or p19 p6 p1 (not p5) (not p9)) (or p20 p29 p9 (not p29) p8) (or (not p13) (not p3) (not p5) p21 (not p3)) (or (not p12) (not p24) (not p10) (not p19) p6) (or (not p24) p0 p2 (not p10) p25) (or (not p23) p1 p15 p29 p19) (or (not p6) p28 (not p14) (not p19) p6) (or p1 p12 (not p14) (not p27) p22) (or (not p25) p5 (not p29) (not p24) (not p3)) (or (not p13) p5 p4 (not p15) p29) (or (not p12) (not p6) (not p2) (not p14) (not p3)) (or p17 p26 (not p11) p4 p20) (or p12 (not p8) p23 (not p3) (not p3)) (or p12 p21 (not p9) (not p9) p9) (or (not p22) p2 (not p3) p28 p24) (or p10 p6 (not p4) (not p18) (not p29)) (or (not p8) (not p27) p21 p13 (not p7)) (or (not p26) (not p24) (not p13) (not p19) (not p26)) (or (not p7) (not p19) (not p17) (not p21) (not p3)) (or (not p25) (not p5) p20 (not p25) p11) (or (not p14) (not p8) (not p18) p25 p28) (or p24 p8 (not p19) p22 p22) (or (not p22) (not p6) (not p25) p12 p27) (or p10 (not p23) (not p27) (not p25) (not p2)) (or p23 (not p27) p23 (not p2) p5) (or (not p2) (not p4) (not p5) (not p29) p24) (or p11 (not p14) (not p15) (not p16) (not p19)) (or p28 p0 (not p14) p24 (not p15)) (or (not p12) (not p17) p11 p19 p2) (or p18 p10 (not p23) p8 p16) (or (not p18) p11 p29 p12 p16) (or (not p16) p9 (not p16) p8 p2) (or p1 (not p19) p17 p16 p6) (or (not p27) (not p24) (not p6) p21 (not p3)) (or p17 p13 p20 (not p21) (not p28)) (or p22 (not p10) (not p28) (not p7) p17) (or (not p3) (not p23) (not p23) p27 p4) (or (not p26) (not p27) p19 (not p29) p5) (or p25 (not p16) p28 p15 p20) (or p1 (not p1) (not p8) p21 (not p19)) (or p8 (not p20) (not p9) p1 p22) (or p29 p25 p16 p21 (not p19)) (or p25 (not p9) p23 p13 (not p19)) (or (not p11) p26 (not p21) p25 (not p24)) (or p22 p17 (not p23) p13 (not p20)) (or p16 (not p7) (not p25) (not p24) p23) (or (not p21) p22 p4 p5 p9) (or (not p14) p16 (not p4) (not p29) (not p16)) (or p8 p7 p29 p18 p5) (or (not p11) (not p0) (not p4) p24 (not p12)) (or p23 (not p10) p3 p3 p24) (or (not p25) p28 p9 (not p26) (not p4)) (or (not p17) (not p21) (not p18) (not p24) (not p26)) (or (not p14) p28 p23 (not p7) p10) (or p7 p10 p7 (not p17) (not p4)) (or p14 (not p12) p12 (not p0) (not p2)) (or (not p17) p28 (not p10) (not p27) (not p26)) (or p25 p12 (not p12) p25 (not p3)) (or p23 (not p9) p14 p28 (not p16)) (or (not p24) (not p8) (not p21) (not p9) (not p18)) (or (not p3) (not p8) (not p9) p14 p5) (or (not p12) p21 p23 p14 p26) (or (not p14) p18 (not p15) p9 (not p13)) (or p4 p19 (not p23) p14 p5) (or (not p2) p2 (not p21) (not p12) p20) (or (not p17) (not p10) p6 (not p13) (not p27)) (or p6 p7 (not p12) (not p23) (not p23)) (or (not p26) (not p10) (not p14) (not p23) (not p1)) (or p0 (not p14) (not p4) p4 p20) (or p10 p12 (not p9) p7 (not p4)) (or p28 p24 (not p1) p21 (not p1)) (or (not p14) p10 (not p4) (not p3) (not p23)) (or p7 p0 p24 p28 (not p27)) (or (not p17) (not p19) (not p3) p26 p29) (or (not p6) (not p2) (not p19) p7 (not p7)) (or (not p8) (not p13) (not p7) p20 p22) (or (not p0) (not p18) p10 p13 (not p26)) (or (not p7) p29 (not p22) p12 p18) (or p21 (not p12) (not p7) (not p12) p5) (or p26 p25 p19 p10 (not p22)) (or (not p1) p10 p7 (not p11) p22) (or (not p12) p19 p2 (not p27) (not p18)) (or (not p27) p25 (not p17) p25 (not p6)) (or p17 p18 p14 p13 (not p3)) (or p22 p19 p10 (not p22) (not p17)) (or (not p10) p19 (not p7) (not p10) (not p2)) (or p25 p20 (not p28) p23 p5) (or p12 p15 (not p20) (not p0) p1) (or (not p5) (not p22) (not p8) p3 (not p10)) (or (not p3) p11 (not p5) (not p9) p1) (or p16 (not p20) (not p3) p17 p2) (or (not p19) (not p6) (not p6) (not p18) (not p26)) (or p2 (not p16) (not p6) p15 (not p24)) (or p7 p20 (not p25) p5 p22) (or (not p8) (not p13) p0 p0 (not p24)) (or (not p9) (not p5) (not p2) p6 p6) (or (not p12) (not p2) (not p14) (not p3) p5) (or (not p14) (not p20) (not p23) (not p12) (not p13)) (or p15 p12 p16 (not p4) p25) (or p12 p19 p5 p21 p16) (or p1 p26 p24 p7 p18) (or p11 (not p0) (not p12) p25 p22) (or p15 p26 (not p8) p4 p24) (or (not p13) p25 p4 p14 p10) (or p3 (not p27) (not p26) p3 (not p2)) (or (not p27) p4 (not p22) (not p22) (not p28)) (or p2 p25 p14 p12 (not p4)) (or (not p1) p28 p5 p15 (not p20)) (or p9 (not p23) p0 (not p23) p9) (or (not p25) (not p0) (not p23) p15 p8) (or (not p8) (not p6) (not p3) (not p21) p5) (or p25 (not p25) (not p17) p28 p18) (or p13 (not p20) (not p22) p5 p26) (or (not p3) (not p22) (not p5) p27 p19) (or p6 (not p17) (not p9) p12 p9) (or (not p29) (not p25) (not p15) p0 p21) (or p16 p20 p5 (not p1) (not p18)) (or p12 p26 p17 p10 (not p9)) (or p0 (not p22) p11 (not p29) (not p8)) (or (not p11) p3 p15 (not p16) p21) (or (not p17) (not p23) (not p28) (not p0) p8) (or (not p9) p22 (not p21) (not p1) p11) (or (not p27) p8 (not p26) p28 (not p1)) (or p23 p17 (not p16) p22 p27) (or p3 (not p18) (not p14) p8 p24) (or (not p0) (not p28) p29 (not p13) (not p24)) (or p9 p5 p11 p29 p25) (or (not p21) p5 p3 (not p21) p8) (or p24 (not p13) (not p16) (not p23) (not p3)) (or p11 p10 (not p11) p26 (not p11)) (or (not p2) p18 p28 p5 (not p14)) (or p3 p21 (not p4) (not p2) (not p5)) (or (not p22) (not p2) (not p29) p16 (not p29)) (or p23 p12 (not p7) p1 p17) (or p2 (not p0) (not p15) (not p12) (not p17)) (or p25 p26 (not p27) p25 p20) (or p19 (not p12) p13 (not p20) p23) (or p8 p18 (not p9) p12 (not p16)) (or p23 (not p8) p17 (not p18) p7) (or p10 p20 p8 (not p2) (not p22)) (or p10 (not p1) (not p7) (not p0) (not p16)) (or p16 (not p12) p11 p25 (not p20)) (or p22 (not p8) (not p25) p8 (not p16)) (or (not p24) (not p14) p8 (not p19) p23) (or (not p19) p15 p6 p9 p11) (or (not p17) p24 (not p21) p5 p29) (or p21 p9 (not p27) (not p18) (not p18)) (or (not p16) (not p21) p8 p3 (not p20)) (or p10 p13 (not p27) p18 p4) (or p26 (not p22) p8 (not p9) p22) (or (not p6) p6 (not p5) (not p17) (not p10)) (or p29 (not p16) (not p27) p9 p16) (or p10 (not p29) p29 (not p7) (not p23)) (or p0 p24 (not p24) (not p3) p18) (or (not p13) p6 (not p28) (not p1) p14) (or (not p2) (not p10) p5 p2 (not p3)) (or p28 p3 (not p19) (not p17) p27) (or p3 (not p21) p12 p19 p6) (or p24 p13 (not p17) p16 p10) (or p6 p12 p2 p28 p11) (or p26 (not p6) p19 p21 p22) (or (not p23) p24 (not p20) (not p20) p25) (or (not p18) p3 (not p7) p26 p7) (or p15 p27 p1 (not p3) p24) (or p24 (not p10) p13 p14 p19) (or p28 (not p25) (not p13) (not p29) (not p19)) (or (not p14) p1 p0 (not p7) (not p3)) (or p5 p2 (not p7) p17 (not p16)) (or (not p5) p1 (not p24) (not p0) (not p18)) (or (not p10) (not p28) p19 p14 (not p27)) (or (not p25) (not p27) p17 p8 (not p29)) (or p3 p0 (not p13) (not p15) p11) (or p7 (not p12) p24 p13 p27) (or p1 p5 p13 (not p17) p25) (or p5 (not p3) (not p3) (not p12) (not p2)) (or p17 (not p28) p21 (not p20) p16) (or p23 p9 (not p4) p20 p26) (or (not p20) (not p24) (not p17) (not p29) p11) (or (not p13) p16 (not p12) (not p13) (not p28)) (or p22 p3 p4 p22 (not p28)) (or (not p27) (not p8) p28 (not p0) (not p3)) (or (not p17) p18 (not p16) (not p1) p29) (or p4 (not p1) p21 (not p18) (not p8)) (or p10 p27 p24 (not p1) p22) (or p14 (not p26) (not p9) p0 (not p18)) (or p25 p0 p26 p28 (not p17)) (or (not p11) p10 p9 p24 p5) (or (not p16) (not p22) (not p12) (not p4) p1) (or p27 (not p16) p5 p16 p29) (or p17 p21 (not p27) p18 p8) (or p3 p10 p6 (not p0) (not p9)) (or p16 p26 p2 (not p16) (not p8)) (or (not p16) (not p3) (not p14) (not p18) (not p12)) (or (not p27) (not p0) (not p29) (not p8) (not p7)) (or (not p9) (not p20) (not p20) p20 (not p6)) (or p12 (not p28) p19 p7 (not p13)) (or p18 (not p26) p12 (not p25) (not p12)) (or (not p25) p19 (not p29) (not p11) (not p23)) (or (not p25) (not p27) p14 (not p16) (not p17)) (or p23 p7 p9 p19 (not p24)) (or (not p9) p5 (not p17) (not p6) p4) (or p1 p1 p14 (not p14) p27) (or (not p28) (not p0) p14 (not p13) (not p23)) (or (not p16) p24 p24 p17 p28) (or p27 p5 (not p12) p13 p15) (or p24 p19 (not p24) (not p15) p24) (or (not p1) p26 p2 (not p17) p29) (or p7 p22 p28 (not p15) p27) (or p23 p8 (not p4) p24 p2) (or (not p5) (not p4) (not p1) p22 (not p7)) (or p7 p18 (not p5) (not p24) p9) (or (not p15) (not p13) (not p28) (not p25) (not p9)) (or p3 p14 p11 (not p1) (not p10)) (or (not p11) p8 (not p13) p27 p9) (or p0 p14 p13 p16 p28) (or (not p13) (not p16) (not p15) p17 (not p2)) (or p15 (not p2) p8 p8 (not p28)) (or (not p12) (not p12) p6 (not p22) (not p13)) (or p8 p28 (not p20) p2 (not p11)) (or p27 (not p26) p16 p13 p22) (or p27 p8 (not p5) (not p5) p25) (or p21 (not p15) p3 (not p22) p12) (or (not p29) p14 (not p18) (not p5) (not p16)) (or (not p27) (not p24) p3 (not p20) p8) (or (not p13) (not p24) p10 (not p19) p14) (or p22 (not p18) p28 (not p20) p25) (or p14 p11 p11 (not p10) (not p26)) (or p16 p7 (not p6) (not p8) p2) (or (not p8) p19 p7 p2 (not p19)) (or p18 (not p16) p23 (not p18) p19) (or p12 (not p19) (not p20) (not p1) p3) (or (not p25) p9 p17 (not p23) p18) (or p22 (not p2) p27 p21 p1) (or (not p15) p21 p7 (not p22) p17) (or (not p3) (not p18) p13 (not p12) (not p20)) (or (not p6) (not p7) p18 (not p14) p8) (or (not p18) (not p15) p10 (not p15) p3) (or p7 p23 (not p0) (not p19) p6) (or p24 (not p15) (not p20) p26 p9) (or p13 (not p14) (not p3) (not p15) p13) (or p13 (not p16) p4 p0 p29) (or p2 (not p25) (not p13) (not p17) (not p29)) (or p10 (not p6) p6 p8 (not p13)) (or (not p26) (not p21) (not p17) p14 p1) (or (not p8) p18 p20 p14 (not p13)) (or p13 (not p24) (not p7) (not p23) p22) (or p3 (not p21) (not p17) p17 p23) (or (not p0) p6 p8 p22 p6) (or p11 p21 p8 (not p9) p21) (or (not p22) p23 p14 p29 (not p1)) (or (not p21) p7 p27 (not p4) p4) (or (not p6) (not p9) (not p13) (not p29) (not p3)) (or (not p25) (not p2) p5 (not p29) p8) (or (not p16) (not p5) p19 p0 p29) (or p20 p23 (not p22) (not p26) p1) (or (not p1) (not p13) (not p25) (not p26) (not p25)) (or (not p10) (not p8) (not p13) (not p13) (not p4)) (or (not p25) (not p28) p13 p22 (not p2)) (or p11 (not p13) p25 (not p20) (not p19)) (or (not p16) (not p3) p23 p17 p28) (or (not p1) (not p15) p24 (not p7) p5) (or p27 p0 p12 p11 p4) (or p7 (not p10) (not p3) (not p7) (not p9)) (or p1 (not p2) (not p21) (not p14) p19) (or p22 (not p2) p13 p16 p5) (or (not p9) (not p28) p3 (not p4) (not p10)) (or p22 p11 (not p25) (not p25) (not p20)) (or p11 p10 (not p7) p14 p14) (or p3 p25 (not p9) (not p27) p23) (or (not p10) p21 (not p26) (not p20) (not p29)) (or (not p13) p25 p12 (not p3) (not p14)) (or p29 (not p16) (not p2) (not p5) (not p13)) (or (not p13) p5 p20 p23 (not p5)) (or (not p26) (not p2) p25 (not p25) (not p1)) (or p21 p10 p28 (not p14) (not p13)) (or (not p1) p24 p26 (not p4) (not p17)) (or p16 p29 p17 p12 p28) (or (not p22) (not p20) (not p6) p3 p17) (or (not p23) (not p10) (not p6) (not p6) p16) (or p9 p20 p29 p22 p10) (or (not p8) p6 p19 p22 (not p20)) (or (not p18) p6 p16 p6 p25) (or (not p5) p21 (not p4) (not p29) p13) (or (not p8) (not p0) (not p24) p4 p4) (or (not p19) p21 p9 p3 (not p21)) (or p5 p17 p12 (not p11) p3) (or (not p14) p8 (not p3) (not p18) (not p14)) (or p10 (not p17) p15 p21 p2) (or p23 p25 (not p23) p3 p14) (or p14 p14 (not p2) (not p22) (not p14)) (or (not p14) (not p16) p12 p29 (not p23)) (or (not p18) (not p28) p7 p22 (not p9)) (or p18 p28 p10 p10 (not p24)) (or (not p0) (not p19) p11 (not p0) (not p29)) (or (not p5) p10 p9 (not p10) p13) (or (not p23) (not p22) p28 p25 p26) (or p29 p7 p9 (not p1) (not p4)) (or p15 p2 p0 p9 (not p22)) (or p1 (not p1) p4 p20 p16) (or p23 p8 (not p18) p1 p22) (or (not p16) (not p24) (not p12) p19 p13) (or p25 (not p17) p17 p13 (not p10)) (or (not p24) (not p27) (not p14) p22 p0) (or p0 p26 (not p18) (not p5) (not p13)) (or (not p23) (not p3) (not p12) p28 (not p5)) (or (not p26) (not p19) (not p5) p10 (not p19)) (or (not p7) p22 (not p21) (not p28) p14) (or p26 p19 p27 p23 (not p13)) (or p1 p14 p13 (not p3) p5) (or p15 (not p29) p3 (not p27) (not p13)) (or (not p25) p25 p3 (not p11) (not p19)) (or (not p22) (not p14) (not p5) (not p28) (not p29)) (or p2 (not p20) p17 (not p11) (not p3)) (or p8 p25 p6 p29 p8) (or (not p11) p5 p3 p27 (not p23)) (or p25 (not p9) p15 p26 p8) (or (not p27) p7 (not p18) (not p17) p6) (or p8 (not p4) (not p10) p3 p17) (or (not p1) p22 p4 p3 p21) (or p26 (not p10) (not p13) (not p6) p20) (or p26 p20 (not p27) p22 (not p5)) (or p18 (not p24) p21 (not p11) p6) (or p8 (not p24) (not p17) p17 p27) (or p28 (not p21) p12 (not p1) p7) (or p1 p19 p12 p18 (not p10)) (or (not p19) (not p20) p11 p20 (not p29)) (or (not p24) p9 (not p29) p26 p27) (or (not p18) p1 (not p2) p5 p18) (or (not p29) (not p29) (not p25) p14 (not p1)) (or p0 p19 p2 (not p18) p27) (or (not p23) (not p7) p14 p10 p24) (or p1 p16 p28 (not p13) (not p10)) (or p23 p3 (not p7) p21 p2) (or (not p23) p14 p1 (not p7) p1) (or p28 p19 p1 (not p2) (not p25)) (or (not p17) (not p28) p20 p4 p2) (or (not p4) p1 (not p20) (not p7) p29) (or p28 (not p13) (not p14) (not p4) (not p16)) (or p10 p10 (not p23) (not p20) p25) (or (not p6) (not p10) p19 p1 p18) (or p23 (not p18) (not p3) (not p18) p12) (or (not p15) (not p5) (not p20) p6 (not p29)) (or p3 (not p28) (not p2) p27 (not p20)) (or p2 p11 p10 (not p18) p24) (or p5 p4 p3 p15 p0))
)
(check-sat)
(get-model)