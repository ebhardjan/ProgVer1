(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p23() Bool)
(declare-fun p44() Bool)
(declare-fun p41() Bool)
(declare-fun p48() Bool)
(declare-fun p4() Bool)
(declare-fun p19() Bool)
(declare-fun p25() Bool)
(declare-fun p37() Bool)
(declare-fun p6() Bool)
(declare-fun p15() Bool)
(declare-fun p31() Bool)
(declare-fun p43() Bool)
(declare-fun p18() Bool)
(declare-fun p0() Bool)
(declare-fun p1() Bool)
(declare-fun p27() Bool)
(declare-fun p24() Bool)
(declare-fun p46() Bool)
(declare-fun p28() Bool)
(declare-fun p49() Bool)
(declare-fun p47() Bool)
(declare-fun p7() Bool)
(declare-fun p45() Bool)
(declare-fun p5() Bool)
(declare-fun p12() Bool)
(declare-fun p34() Bool)
(declare-fun p22() Bool)
(declare-fun p14() Bool)
(declare-fun p13() Bool)
(declare-fun p16() Bool)
(declare-fun p20() Bool)
(declare-fun p36() Bool)
(declare-fun p29() Bool)
(declare-fun p8() Bool)
(declare-fun p26() Bool)
(declare-fun p3() Bool)
(declare-fun p39() Bool)
(declare-fun p17() Bool)
(declare-fun p33() Bool)
(declare-fun p38() Bool)
(declare-fun p2() Bool)
(declare-fun p42() Bool)
(declare-fun p30() Bool)
(declare-fun p32() Bool)
(declare-fun p10() Bool)
(declare-fun p40() Bool)
(declare-fun p9() Bool)
(declare-fun p21() Bool)
(declare-fun p11() Bool)
(declare-fun p35() Bool)
(assert
(and (or (not p23) p44 p41) (or (not p48) p4 (not p19)) (or p25 (not p37) p6) (or p15 (not p31) p43) (or p18 (not p0) (not p23)) (or p1 (not p27) (not p24)) (or p46 p0 p41) (or (not p28) (not p49) (not p24)) (or (not p47) p7 (not p45)) (or p5 p49 p25) (or p12 (not p24) (not p34)) (or (not p7) p44 p7) (or p45 p22 (not p14)) (or (not p13) p28 (not p16)) (or p20 p36 p16) (or (not p24) (not p37) (not p29)) (or p8 p14 p26) (or (not p29) (not p24) (not p47)) (or (not p13) (not p3) p46) (or (not p24) (not p8) p44) (or (not p36) p0 p46) (or p16 (not p44) p6) (or (not p39) p34 (not p49)) (or (not p3) (not p36) (not p28)) (or (not p17) (not p13) (not p19)) (or p49 p0 (not p5)) (or (not p24) p44 p25) (or p20 p19 (not p8)) (or p33 p25 (not p44)) (or p6 p27 (not p0)) (or (not p17) (not p15) (not p44)) (or (not p22) p49 (not p7)) (or (not p38) p29 p36) (or (not p27) (not p2) (not p31)) (or (not p37) (not p1) (not p5)) (or (not p2) (not p23) (not p42)) (or (not p23) (not p30) p31) (or (not p42) (not p18) (not p37)) (or (not p47) (not p46) (not p18)) (or (not p5) p7 (not p0)) (or p48 (not p45) p43) (or p44 (not p17) p41) (or p13 p39 p20) (or (not p37) p6 p15) (or (not p12) (not p28) (not p49)) (or (not p44) (not p43) (not p7)) (or (not p20) p29 (not p33)) (or p24 p48 p26) (or p47 p37 (not p27)) (or p19 (not p38) (not p8)) (or (not p32) (not p14) p0) (or (not p5) p24 (not p10)) (or p49 p8 p24) (or p41 p44 p24) (or p43 p45 p8) (or (not p38) (not p24) (not p45)) (or (not p24) (not p31) (not p31)) (or p47 p4 (not p12)) (or p40 (not p31) (not p39)) (or (not p45) p36 (not p16)) (or (not p32) (not p34) (not p23)) (or p9 p39 p29) (or p32 p10 (not p10)) (or p3 (not p16) (not p26)) (or p7 (not p13) (not p17)) (or p44 (not p42) (not p29)) (or (not p13) (not p14) (not p8)) (or (not p15) (not p44) (not p28)) (or (not p45) p38 p8) (or p7 p3 p23) (or (not p37) (not p5) p24) (or p36 (not p33) p34) (or (not p34) (not p32) (not p2)) (or (not p49) p33 p21) (or (not p41) p16 p33) (or (not p39) p7 p41) (or p11 (not p8) (not p18)) (or (not p5) (not p2) (not p20)) (or (not p21) p41 (not p36)) (or p48 (not p2) (not p25)) (or (not p34) (not p1) (not p28)) (or p35 (not p40) (not p48)) (or (not p23) p32 (not p34)) (or (not p39) p44 p34) (or p35 p35 (not p39)) (or p39 p15 p12) (or (not p25) (not p3) (not p23)) (or (not p47) (not p1) (not p19)) (or (not p21) (not p40) p49) (or p12 (not p49) p32) (or (not p0) (not p10) p45) (or (not p22) (not p42) (not p10)) (or (not p10) p1 p14) (or (not p20) (not p16) p40) (or (not p20) (not p26) (not p4)) (or (not p27) (not p30) (not p29)) (or p7 (not p11) (not p11)) (or p38 p33 p28) (or (not p21) p14 p47) (or p39 (not p31) p28) (or p12 p39 p2) (or (not p28) p27 p1) (or p5 (not p25) (not p13)) (or p30 p29 (not p28)) (or p36 (not p32) p41) (or (not p28) (not p30) p11) (or (not p15) (not p11) p3) (or p9 (not p34) p10) (or (not p0) (not p22) (not p31)) (or p16 p15 (not p4)) (or p9 (not p45) p17) (or p3 p23 (not p20)) (or (not p10) p42 p10) (or p31 p40 p4) (or p45 p26 p37) (or p36 (not p7) (not p8)) (or (not p29) (not p40) (not p17)) (or (not p42) (not p11) (not p3)) (or p48 (not p46) (not p20)) (or (not p16) p3 p38) (or p9 (not p2) (not p29)) (or (not p49) p1 p17) (or p22 p38 p16) (or (not p29) (not p24) (not p44)) (or p42 p20 (not p27)) (or (not p22) p11 p36) (or (not p44) p49 p26) (or p46 p25 p27) (or (not p4) p35 p25) (or p46 p26 (not p30)) (or (not p19) p26 p48) (or p30 (not p47) p29) (or p43 p21 p10) (or (not p27) p34 p21) (or (not p34) (not p15) p49) (or (not p28) p3 p34) (or p18 p36 (not p26)) (or p6 p35 (not p49)) (or p19 p42 (not p1)) (or p28 (not p38) p21) (or (not p26) p7 p25) (or p14 p6 p1) (or p3 (not p49) p40) (or p27 p40 (not p23)) (or (not p47) p35 (not p49)) (or (not p0) (not p47) (not p8)) (or (not p24) p5 p42) (or (not p32) (not p11) (not p13)) (or (not p43) (not p8) p7) (or p0 (not p49) (not p26)) (or (not p14) (not p43) (not p26)) (or (not p2) p11 p10) (or (not p5) p17 p37) (or p35 (not p30) p20) (or p17 p6 (not p13)) (or p39 (not p48) p33) (or (not p38) (not p47) (not p44)) (or p3 p38 (not p39)) (or (not p11) p35 p11) (or (not p34) p2 p33) (or p46 (not p0) p41) (or (not p10) (not p12) p27) (or p33 p7 (not p19)) (or p8 (not p15) p41) (or (not p25) (not p5) (not p22)) (or p29 p21 p20) (or (not p47) p0 (not p36)) (or p36 p24 p4) (or (not p43) (not p39) p49) (or (not p3) p2 (not p14)) (or p8 (not p10) (not p42)) (or (not p40) (not p17) (not p46)) (or (not p11) (not p10) (not p17)) (or (not p39) (not p22) p8) (or (not p27) (not p11) (not p49)) (or p27 p41 p10) (or p42 p13 p47) (or p15 p23 p35) (or p29 (not p35) p38) (or (not p7) (not p24) p2) (or p19 (not p41) p38) (or (not p22) (not p9) (not p23)) (or p31 p15 p25) (or (not p38) (not p23) p14) (or (not p29) (not p26) p20) (or (not p4) (not p24) p5) (or p15 p11 p41) (or p8 (not p48) p8) (or p32 (not p26) p48) (or p21 p24 p24) (or (not p39) (not p43) (not p6)) (or p14 (not p17) p42) (or (not p43) (not p43) (not p12)) (or (not p19) p45 p45) (or p26 p22 (not p3)) (or p8 (not p20) (not p1)) (or (not p26) (not p11) (not p23)) (or (not p12) (not p35) p41) (or (not p25) p42 p18) (or (not p37) p26 (not p41)) (or p21 (not p20) p47) (or (not p42) (not p22) p48) (or p38 (not p28) p30) (or p7 p36 (not p12)) (or (not p35) (not p17) (not p43)) (or (not p39) p0 p34) (or p4 (not p40) (not p40)) (or p10 (not p44) (not p17)) (or p7 p25 p15) (or (not p40) p16 (not p18)))
)
(check-sat)
(get-model)
