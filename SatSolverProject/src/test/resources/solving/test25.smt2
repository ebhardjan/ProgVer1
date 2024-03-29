(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p1() Bool)
(declare-fun p5() Bool)
(declare-fun p0() Bool)
(declare-fun p2() Bool)
(declare-fun p3() Bool)
(declare-fun p4() Bool)
(assert
(and
  (or p4 p5 (not p1))
  (or (not p4) p3 (not p2) p5)
  (or (not p5) p2)
  (or p2 p5)
  (or p0 (not p5))
  (or p1 p5)
  (or (not p0) p1 p5)
  (or (not p3) (not p1) p5)
))
(check-sat)
(get-model)
