(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p11() Bool)
(declare-fun p33() Bool)
(declare-fun p3() Bool)
(declare-fun p31() Bool)
(declare-fun p24() Bool)
(declare-fun p2() Bool)
(declare-fun p19() Bool)
(declare-fun p18() Bool)
(declare-fun p4() Bool)
(declare-fun p32() Bool)
(declare-fun p12() Bool)
(declare-fun p1() Bool)
(declare-fun p16() Bool)
(declare-fun p37() Bool)
(declare-fun p22() Bool)
(declare-fun p23() Bool)
(declare-fun p34() Bool)
(declare-fun p17() Bool)
(declare-fun p29() Bool)
(declare-fun p13() Bool)
(declare-fun p21() Bool)
(declare-fun p25() Bool)
(declare-fun p39() Bool)
(declare-fun p27() Bool)
(declare-fun p36() Bool)
(declare-fun p8() Bool)
(declare-fun p35() Bool)
(declare-fun p6() Bool)
(declare-fun p26() Bool)
(declare-fun p30() Bool)
(declare-fun p0() Bool)
(declare-fun p14() Bool)
(declare-fun p10() Bool)
(declare-fun p5() Bool)
(declare-fun p9() Bool)
(declare-fun p20() Bool)
(declare-fun p38() Bool)
(declare-fun p28() Bool)
(declare-fun p15() Bool)
(declare-fun p7() Bool)
(assert
(and (or p11 (not p33) (not p3)) (or (not p31) (not p24) p2) (or p19 p18 (not p3)) (or p4 (not p32) p18) (or (not p11) p12 p1) (or p16 p37 p22) (or p23 (not p34) (not p17)) (or (not p2) (not p22) (not p17)) (or (not p29) p13 p34) (or (not p21) p25 (not p1)) (or p25 p1 (not p39)) (or (not p27) (not p37) p23) (or (not p12) p36 p3) (or p8 p35 (not p6)) (or (not p26) (not p31) (not p3)) (or p36 p17 (not p30)) (or p0 p23 (not p14)) (or (not p37) p35 p2) (or p33 (not p31) p35) (or p6 (not p10) p36) (or p5 p39 (not p2)) (or (not p9) p10 p13) (or (not p13) p33 p8) (or (not p20) p3 p24) (or (not p31) p8 (not p34)) (or (not p13) p39 (not p22)) (or p32 (not p12) p6) (or p37 p26 (not p39)) (or p35 (not p22) p18) (or (not p38) p8 p33) (or p24 p24 (not p32)) (or p10 p27 (not p35)) (or p17 (not p4) (not p28)) (or (not p37) (not p2) (not p32)) (or p32 p34 p8) (or (not p30) (not p32) p23) (or p14 (not p16) p33) (or p22 p37 p35) (or (not p5) p24 p26) (or (not p9) p33 (not p18)) (or p15 (not p10) p11) (or (not p23) p31 (not p29)) (or p4 p21 (not p10)) (or p32 p36 (not p7)) (or (not p32) p2 (not p23)) (or p0 (not p28) (not p21)) (or (not p8) (not p23) (not p11)) (or p3 p34 p14) (or p34 (not p33) (not p33)) (or p1 (not p0) p37) (or (not p9) (not p28) p24) (or (not p18) (not p37) p18) (or p27 (not p22) (not p16)) (or p4 p2 (not p10)) (or p23 (not p20) (not p23)) (or (not p38) (not p33) (not p30)) (or p24 (not p18) p16) (or (not p7) p22 p8) (or (not p37) (not p15) p16) (or (not p1) (not p37) p29) (or (not p5) p24 (not p39)) (or (not p29) p22 (not p34)) (or p2 p0 (not p29)) (or (not p3) p39 (not p14)) (or (not p14) p19 (not p15)) (or (not p10) p5 (not p27)) (or (not p2) (not p2) p34) (or p1 p2 (not p17)) (or (not p31) (not p27) (not p3)) (or (not p1) p0 (not p36)) (or (not p11) p38 (not p13)) (or (not p10) (not p4) p9) (or (not p6) p33 p30) (or p36 (not p2) (not p26)) (or p35 p7 p25) (or p0 p16 (not p7)) (or (not p0) (not p6) (not p28)) (or p24 p19 (not p10)) (or p29 (not p29) p36) (or p31 (not p12) p7) (or p1 (not p28) p12) (or (not p2) p5 (not p28)) (or p4 (not p31) (not p31)) (or (not p15) (not p16) (not p34)) (or (not p5) p12 p30) (or (not p2) p4 p33) (or p0 p36 (not p29)) (or (not p25) (not p19) (not p8)) (or p36 p21 (not p6)) (or p26 p0 (not p26)) (or p13 p16 p20) (or (not p16) p36 p33) (or p11 (not p21) (not p35)) (or (not p26) (not p1) p31) (or (not p26) p12 (not p16)) (or (not p23) (not p10) (not p7)) (or (not p37) (not p34) p18) (or (not p35) p30 p24) (or p5 (not p38) (not p6)) (or (not p9) (not p16) (not p26)) (or (not p17) p33 (not p20)) (or p21 (not p34) p4) (or (not p29) (not p8) p21) (or p26 p1 p33) (or p14 p31 (not p6)) (or p24 (not p14) (not p4)) (or (not p26) p5 (not p9)) (or p32 (not p4) (not p1)) (or p17 p12 p23) (or p20 (not p28) p34) (or (not p38) p31 (not p8)) (or p6 p27 p28) (or (not p39) (not p4) p12) (or p37 p38 p27) (or p22 (not p4) p15) (or p6 (not p38) (not p39)) (or (not p15) p2 p16) (or (not p22) p26 p5) (or p3 (not p22) p6) (or (not p1) (not p30) p9) (or p39 p13 p24) (or (not p17) p0 p31) (or (not p18) (not p26) p30) (or p21 p8 p36) (or (not p31) p19 (not p35)) (or p0 p27 p20) (or p35 p17 p37) (or p26 (not p25) (not p3)) (or (not p3) (not p36) (not p31)) (or (not p35) p25 p36) (or (not p22) (not p27) (not p32)) (or p34 p1 (not p15)) (or p25 p14 (not p23)) (or p6 (not p16) (not p23)) (or p32 p2 (not p38)) (or p28 p21 p38) (or (not p35) (not p15) p16) (or p30 p14 p30) (or (not p5) p10 (not p21)) (or (not p29) (not p12) (not p1)) (or p31 p33 (not p11)) (or (not p0) p3 (not p16)) (or (not p7) (not p39) (not p21)) (or (not p10) (not p5) (not p9)) (or (not p26) p39 p5) (or (not p13) (not p22) p5) (or p35 (not p39) (not p17)) (or (not p12) (not p35) (not p7)) (or p36 p13 p19) (or (not p22) (not p2) p11) (or (not p21) p14 p6) (or (not p9) p23 p21) (or p5 p13 p26) (or p0 (not p11) (not p11)) (or (not p33) (not p33) (not p19)) (or p36 (not p16) p29) (or (not p6) p29 (not p34)) (or p4 (not p26) p22) (or (not p18) p35 p32) (or p26 (not p25) p12) (or (not p18) p21 p35) (or p30 p9 (not p33)) (or (not p31) p28 p3) (or (not p17) (not p22) (not p20)) (or (not p35) (not p6) (not p24)) (or (not p32) p5 (not p34)) (or (not p8) p31 p28) (or (not p5) (not p12) (not p4)) (or p34 p30 (not p8)) (or (not p34) p22 p14) (or p15 (not p3) (not p2)))
)
(check-sat)
(get-model)