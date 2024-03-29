(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p2() Bool)
(declare-fun p1() Bool)
(declare-fun p3() Bool)
(declare-fun p4() Bool)
(declare-fun p5() Bool)
(declare-fun p0() Bool)
(assert
(and
  (or p2 p1)
  (or (not p2) p5)
  (or (not p5) (not p2) (not p4))
  (or (not p1) (not p4) p2 (not p3))
  (or p3 (not p0) (not p0) p3)
  (or p2 (not p5))
  (or (not p1) p5)
  (or p4 (not p5) p0)
  (or p5 p3 (not p1))
))
(check-sat)
(get-model)
