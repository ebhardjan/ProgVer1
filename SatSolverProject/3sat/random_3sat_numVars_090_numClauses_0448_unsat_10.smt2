(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p50() Bool)
(declare-fun p49() Bool)
(declare-fun p23() Bool)
(declare-fun p86() Bool)
(declare-fun p87() Bool)
(declare-fun p22() Bool)
(declare-fun p0() Bool)
(declare-fun p31() Bool)
(declare-fun p61() Bool)
(declare-fun p83() Bool)
(declare-fun p63() Bool)
(declare-fun p24() Bool)
(declare-fun p64() Bool)
(declare-fun p45() Bool)
(declare-fun p51() Bool)
(declare-fun p35() Bool)
(declare-fun p2() Bool)
(declare-fun p70() Bool)
(declare-fun p9() Bool)
(declare-fun p21() Bool)
(declare-fun p37() Bool)
(declare-fun p38() Bool)
(declare-fun p75() Bool)
(declare-fun p29() Bool)
(declare-fun p34() Bool)
(declare-fun p40() Bool)
(declare-fun p80() Bool)
(declare-fun p19() Bool)
(declare-fun p6() Bool)
(declare-fun p11() Bool)
(declare-fun p30() Bool)
(declare-fun p15() Bool)
(declare-fun p72() Bool)
(declare-fun p1() Bool)
(declare-fun p27() Bool)
(declare-fun p41() Bool)
(declare-fun p58() Bool)
(declare-fun p39() Bool)
(declare-fun p28() Bool)
(declare-fun p52() Bool)
(declare-fun p43() Bool)
(declare-fun p82() Bool)
(declare-fun p53() Bool)
(declare-fun p10() Bool)
(declare-fun p32() Bool)
(declare-fun p44() Bool)
(declare-fun p62() Bool)
(declare-fun p55() Bool)
(declare-fun p65() Bool)
(declare-fun p7() Bool)
(declare-fun p12() Bool)
(declare-fun p74() Bool)
(declare-fun p57() Bool)
(declare-fun p69() Bool)
(declare-fun p5() Bool)
(declare-fun p47() Bool)
(declare-fun p54() Bool)
(declare-fun p48() Bool)
(declare-fun p14() Bool)
(declare-fun p84() Bool)
(declare-fun p4() Bool)
(declare-fun p42() Bool)
(declare-fun p26() Bool)
(declare-fun p81() Bool)
(declare-fun p68() Bool)
(declare-fun p46() Bool)
(declare-fun p3() Bool)
(declare-fun p17() Bool)
(declare-fun p77() Bool)
(declare-fun p88() Bool)
(declare-fun p56() Bool)
(declare-fun p59() Bool)
(declare-fun p67() Bool)
(declare-fun p78() Bool)
(declare-fun p60() Bool)
(declare-fun p76() Bool)
(declare-fun p8() Bool)
(declare-fun p36() Bool)
(declare-fun p13() Bool)
(declare-fun p33() Bool)
(declare-fun p66() Bool)
(declare-fun p71() Bool)
(declare-fun p16() Bool)
(declare-fun p18() Bool)
(declare-fun p25() Bool)
(declare-fun p89() Bool)
(declare-fun p79() Bool)
(declare-fun p20() Bool)
(declare-fun p73() Bool)
(declare-fun p85() Bool)
(assert
(and (or p50 p49 p23) (or p86 p87 (not p22)) (or p0 p31 (not p61)) (or p83 (not p63) p24) (or (not p64) (not p45) p51) (or (not p35) (not p2) (not p70)) (or (not p9) p21 p37) (or (not p2) p45 (not p2)) (or (not p9) (not p38) (not p75)) (or (not p0) p29 (not p34)) (or p40 p80 p19) (or (not p50) (not p6) p21) (or (not p11) p30 (not p22)) (or p15 (not p72) (not p24)) (or (not p1) (not p1) p27) (or (not p41) (not p58) (not p39)) (or p28 p52 p43) (or p21 p82 p53) (or (not p10) (not p61) p19) (or p32 p44 (not p62)) (or p55 p65 (not p7)) (or (not p83) (not p12) (not p74)) (or p1 p9 p57) (or p49 (not p69) (not p12)) (or p5 p47 (not p87)) (or (not p54) p58 p57) (or (not p72) (not p74) (not p69)) (or p35 p62 p48) (or (not p14) p84 p74) (or p38 (not p51) (not p61)) (or (not p4) p23 p32) (or p42 (not p30) (not p26)) (or p28 p63 p24) (or p53 (not p38) p41) (or p81 (not p24) (not p23)) (or (not p48) p10 (not p27)) (or (not p42) p41 p34) (or (not p49) (not p68) (not p54)) (or p69 p46 (not p3)) (or (not p46) (not p10) (not p3)) (or (not p86) (not p40) p17) (or p50 p77 p30) (or (not p9) (not p88) (not p21)) (or (not p82) p12 (not p77)) (or (not p10) p56 (not p7)) (or p11 (not p59) (not p67)) (or (not p80) p78 p35) (or p60 (not p76) (not p82)) (or p53 p11 p11) (or p6 (not p77) (not p86)) (or p5 (not p82) p8) (or p49 (not p86) p31) (or p35 p23 p36) (or p13 (not p33) p74) (or p12 p66 (not p48)) (or p8 (not p65) (not p67)) (or (not p2) p71 (not p32)) (or (not p11) (not p19) p14) (or (not p46) p60 (not p51)) (or p40 p14 (not p10)) (or p49 (not p54) (not p16)) (or p67 (not p41) p81) (or (not p53) p32 (not p45)) (or p51 (not p71) (not p74)) (or (not p47) p1 (not p23)) (or p87 (not p65) (not p82)) (or p87 p76 (not p38)) (or p41 p6 p72) (or (not p12) p54 (not p13)) (or (not p18) (not p62) (not p31)) (or p6 p66 p65) (or p25 p18 p32) (or p1 p36 (not p7)) (or p25 (not p35) p56) (or p47 p89 (not p81)) (or p83 (not p9) (not p66)) (or p42 p2 (not p15)) (or p55 (not p35) p89) (or p70 (not p30) p82) (or p62 (not p64) (not p79)) (or (not p24) p89 p74) (or (not p3) p77 (not p69)) (or p84 (not p59) p13) (or (not p16) (not p29) (not p3)) (or p58 p40 p32) (or (not p51) p66 p22) (or (not p36) (not p84) (not p2)) (or p7 p23 p39) (or p12 p59 p26) (or p45 p14 p58) (or p37 (not p25) (not p2)) (or (not p33) p31 p46) (or (not p48) (not p34) (not p15)) (or (not p39) p38 (not p37)) (or p74 (not p4) p4) (or p9 (not p2) p43) (or (not p67) p19 (not p64)) (or (not p88) p53 p17) (or p14 (not p61) (not p13)) (or p47 p29 (not p4)) (or (not p22) (not p60) (not p84)) (or (not p86) p27 (not p34)) (or (not p62) (not p8) p23) (or (not p52) p63 (not p2)) (or p32 p15 p49) (or (not p17) (not p68) (not p2)) (or (not p50) (not p54) (not p72)) (or p55 p58 (not p79)) (or p87 p45 p82) (or (not p55) (not p10) (not p42)) (or (not p21) p43 (not p26)) (or (not p20) (not p39) p30) (or (not p11) (not p57) p6) (or p80 (not p26) (not p88)) (or (not p24) (not p67) p35) (or p35 (not p6) (not p49)) (or p12 (not p8) p65) (or p6 p7 p23) (or (not p38) p77 p19) (or (not p64) p80 p34) (or p24 p35 p83) (or (not p13) (not p79) (not p16)) (or p4 (not p76) p66) (or p18 p13 (not p5)) (or (not p75) (not p75) (not p31)) (or (not p69) (not p3) (not p53)) (or p87 (not p44) (not p77)) (or (not p26) (not p55) p3) (or (not p3) (not p34) p43) (or (not p73) p42 p87) (or (not p57) p8 p56) (or (not p71) (not p80) p47) (or p18 (not p19) p38) (or (not p64) (not p80) (not p68)) (or (not p88) p73 (not p38)) (or (not p32) (not p11) p58) (or p68 p10 (not p17)) (or (not p50) p61 p8) (or (not p67) (not p23) (not p3)) (or (not p74) (not p45) (not p87)) (or p58 p59 (not p84)) (or (not p73) (not p24) (not p21)) (or (not p54) p19 (not p69)) (or p7 (not p85) p62) (or p87 p42 (not p36)) (or (not p73) p84 (not p22)) (or p43 p1 (not p73)) (or p32 (not p34) (not p84)) (or p71 (not p7) (not p8)) (or (not p35) (not p15) (not p87)) (or (not p77) p3 (not p65)) (or (not p29) p64 (not p40)) (or (not p31) p30 (not p60)) (or (not p39) p25 (not p57)) (or p52 (not p18) (not p74)) (or p66 (not p77) p45) (or (not p30) (not p57) p59) (or (not p39) p38 p57) (or (not p39) (not p72) (not p88)) (or (not p7) p8 (not p59)) (or p19 (not p58) p13) (or p83 (not p31) p52) (or (not p2) p18 (not p24)) (or p36 (not p71) (not p37)) (or p78 (not p83) p55) (or p32 (not p56) (not p76)) (or (not p43) p24 p70) (or p22 (not p52) p18) (or p35 (not p44) (not p37)) (or p55 (not p25) p38) (or p86 (not p18) (not p35)) (or (not p39) (not p54) (not p10)) (or (not p23) p7 (not p26)) (or (not p46) p61 (not p70)) (or p63 p61 p27) (or (not p76) p61 p61) (or (not p57) (not p77) (not p64)) (or p74 (not p15) (not p73)) (or p66 (not p57) (not p2)) (or (not p58) (not p76) p78) (or p14 (not p89) (not p83)) (or (not p76) (not p79) p63) (or p44 (not p57) (not p60)) (or p64 p27 (not p15)) (or (not p82) (not p48) p65) (or (not p60) p48 (not p10)) (or p57 p27 p7) (or (not p21) (not p63) p34) (or p75 (not p87) (not p14)) (or (not p47) (not p48) p84) (or (not p24) (not p40) (not p27)) (or p25 p45 p49) (or (not p46) (not p40) (not p1)) (or p29 (not p73) p44) (or (not p24) p34 p14) (or (not p46) p3 (not p62)) (or (not p77) (not p15) p81) (or p10 p40 (not p59)) (or (not p10) (not p51) (not p15)) (or p35 (not p72) (not p6)) (or p17 (not p32) p72) (or (not p64) p2 p30) (or (not p63) p52 p63) (or p24 p31 (not p79)) (or p46 (not p54) (not p71)) (or (not p58) p13 p72) (or p86 p84 (not p22)) (or (not p15) (not p48) (not p85)) (or p65 p21 p58) (or (not p16) p16 (not p25)) (or (not p3) (not p69) p25) (or (not p3) (not p83) (not p44)) (or (not p55) p27 (not p51)) (or p83 (not p81) p8) (or p66 (not p78) p10) (or (not p79) p6 (not p86)) (or (not p87) p64 (not p44)) (or p3 (not p86) p35) (or (not p23) p57 (not p28)) (or (not p61) (not p74) p83) (or (not p41) (not p31) (not p84)) (or p61 p51 (not p0)) (or p49 p57 (not p44)) (or p3 p36 p18) (or (not p47) (not p68) p68) (or p10 (not p16) (not p65)) (or p89 (not p40) p9) (or p1 p26 (not p17)) (or (not p0) p77 (not p86)) (or p72 p23 p13) (or p2 (not p73) p2) (or (not p69) (not p13) (not p2)) (or p19 (not p87) p27) (or (not p20) p15 p33) (or p74 (not p22) p28) (or p43 p17 (not p59)) (or (not p44) (not p34) p47) (or (not p33) p15 (not p34)) (or (not p16) p8 p33) (or (not p1) p79 p37) (or p56 p25 (not p75)) (or p39 p0 p87) (or p63 p56 (not p54)) (or (not p83) p17 (not p57)) (or (not p53) (not p51) (not p39)) (or p29 p60 (not p37)) (or (not p47) p53 (not p83)) (or p21 (not p5) (not p10)) (or p62 (not p34) p0) (or p28 p36 p55) (or p88 (not p7) p42) (or (not p59) (not p7) p41) (or p44 p3 p25) (or p39 p9 (not p6)) (or p75 p55 p57) (or p35 p73 (not p76)) (or (not p18) (not p35) (not p10)) (or (not p55) (not p59) p82) (or p70 (not p80) (not p68)) (or p49 (not p79) p78) (or p3 p2 p14) (or (not p30) p86 p53) (or p28 p68 (not p5)) (or p26 (not p25) p54) (or (not p8) p62 (not p24)) (or p80 (not p71) (not p25)) (or (not p67) (not p29) p62) (or p53 p4 p61) (or (not p10) (not p78) p51) (or (not p51) (not p68) (not p63)) (or (not p10) p81 p54) (or (not p25) p45 p65) (or p44 p46 (not p22)) (or (not p71) (not p84) p71) (or (not p83) (not p89) (not p68)) (or p75 (not p25) (not p82)) (or p39 (not p72) (not p27)) (or p39 p5 p83) (or (not p30) (not p23) p85) (or (not p15) (not p45) (not p7)) (or (not p42) (not p67) p3) (or p40 p17 (not p46)) (or p10 (not p58) p36) (or p77 p41 (not p51)) (or (not p21) (not p60) p72) (or p85 (not p83) p34) (or (not p87) p23 p59) (or p38 (not p50) (not p5)) (or p87 (not p43) p26) (or p43 (not p27) (not p11)) (or p1 (not p47) p68) (or p37 (not p57) p48) (or p56 p64 (not p31)) (or p27 p73 p50) (or (not p63) (not p62) (not p13)) (or (not p65) (not p51) (not p51)) (or (not p71) (not p28) p12) (or (not p75) p72 p78) (or (not p86) (not p19) p36) (or p20 (not p47) p65) (or (not p54) p37 p62) (or p56 (not p32) (not p40)) (or (not p9) p12 p64) (or p47 p82 (not p19)) (or p49 (not p59) p55) (or p6 p20 p51) (or p77 (not p80) p47) (or (not p25) (not p20) p39) (or (not p3) p34 (not p38)) (or (not p31) p49 (not p70)) (or (not p61) (not p46) (not p54)) (or (not p60) p9 p11) (or (not p26) p7 p7) (or p76 (not p31) (not p26)) (or p57 (not p16) (not p61)) (or (not p5) (not p69) p62) (or (not p36) p42 p34) (or p9 p1 p69) (or p61 (not p23) p66) (or p69 p52 p59) (or p65 (not p87) (not p83)) (or p60 (not p47) (not p89)) (or p14 (not p88) (not p48)) (or (not p53) p10 (not p63)) (or p68 (not p10) p34) (or p7 (not p10) p15) (or p46 p10 (not p44)) (or (not p63) (not p57) (not p81)) (or p78 (not p62) (not p39)) (or p60 p78 (not p86)) (or p33 (not p64) p54) (or (not p56) (not p71) (not p23)) (or p10 (not p23) (not p30)) (or (not p87) (not p16) (not p89)) (or (not p58) p86 (not p15)) (or p20 p73 p13) (or p55 p24 p74) (or p73 p52 (not p87)) (or p27 (not p32) p26) (or p63 (not p39) (not p36)) (or p15 p24 p63) (or (not p74) (not p26) p77) (or p35 p20 p5) (or (not p11) p23 (not p42)) (or p83 (not p1) (not p2)) (or (not p75) (not p19) p81) (or (not p13) (not p9) p80) (or (not p45) (not p63) p80) (or (not p59) p12 p79) (or (not p50) p7 p46) (or p74 p67 p11) (or p86 (not p13) p34) (or (not p27) p9 p22) (or p43 p57 p10) (or (not p1) p39 p75) (or (not p85) p17 (not p0)) (or p20 p38 (not p14)) (or (not p66) (not p8) (not p19)) (or (not p4) (not p7) p2) (or p31 p21 (not p49)) (or p21 (not p43) p73) (or p77 (not p76) (not p19)) (or (not p64) p35 (not p13)) (or p84 (not p81) p55) (or p29 (not p64) (not p29)) (or p82 p15 p19) (or p66 p19 (not p64)) (or p33 (not p14) (not p78)) (or (not p58) (not p14) (not p64)) (or p82 (not p35) (not p3)) (or (not p3) p66 p77) (or (not p54) (not p3) p82) (or (not p83) (not p32) p22) (or p26 p62 p77) (or p82 p50 (not p34)) (or p42 (not p31) p29) (or p17 p32 (not p44)) (or (not p22) (not p6) p11) (or p73 p68 p12) (or p70 (not p60) (not p10)) (or (not p65) (not p64) p42) (or p70 p17 p65) (or p23 p6 (not p61)) (or (not p42) (not p81) p15) (or p81 p6 p5) (or p41 (not p2) p44) (or (not p68) p40 (not p12)) (or (not p72) (not p45) (not p88)) (or (not p50) (not p57) (not p10)) (or p54 (not p46) p60) (or p34 p89 p3) (or p54 (not p32) p79) (or (not p53) (not p25) p69) (or (not p75) p66 p30) (or (not p49) (not p5) (not p1)) (or (not p34) (not p41) p56) (or (not p77) (not p28) (not p68)) (or (not p15) (not p44) p25) (or (not p26) (not p75) (not p51)) (or (not p16) p55 (not p35)) (or (not p26) (not p71) p49) (or (not p2) p3 p75) (or p71 p84 (not p11)) (or (not p73) p75 p52) (or p28 (not p27) p51) (or p12 (not p53) (not p69)) (or p87 (not p66) p56) (or (not p67) (not p44) (not p18)) (or (not p30) p31 p19) (or (not p34) (not p29) p59) (or (not p22) (not p6) (not p54)) (or p79 (not p21) (not p71)) (or (not p24) p44 p9) (or (not p71) (not p85) (not p43)) (or p31 (not p11) (not p36)) (or p68 (not p12) (not p43)) (or p68 (not p19) (not p16)) (or p14 p21 (not p62)) (or p17 p58 (not p6)) (or p18 p71 p75) (or (not p6) (not p14) p22) (or p21 p34 (not p67)) (or (not p27) (not p66) p56) (or p26 (not p10) p71) (or p79 (not p57) (not p11)) (or (not p81) p42 p55) (or p31 (not p66) (not p31)) (or p64 (not p16) (not p54)) (or p12 (not p63) p70) (or p32 p63 p87) (or (not p74) p8 (not p2)) (or p8 p74 (not p35)) (or p48 (not p4) (not p86)) (or p76 p22 p43) (or (not p31) p88 (not p23)) (or p21 (not p73) p19) (or p22 p30 p33) (or (not p26) (not p20) (not p36)) (or p53 p80 p1) (or p74 (not p16) (not p55)) (or (not p31) (not p77) p81) (or (not p24) (not p75) (not p47)) (or p30 (not p28) p5) (or p67 (not p25) (not p31)) (or (not p1) (not p56) p65) (or p23 p68 p31) (or p30 p2 p71) (or (not p22) (not p52) (not p84)))
)
(check-sat)
(get-model)