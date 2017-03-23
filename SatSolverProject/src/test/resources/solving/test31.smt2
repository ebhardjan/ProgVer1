(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p3() Bool)
(declare-fun p5() Bool)
(declare-fun p0() Bool)
(declare-fun p4() Bool)
(declare-fun p1() Bool)
(declare-fun p2() Bool)
(assert
(and
  (or p3 (not p5) p3)
  (or p1 (not p5) p2 (not p5))
  (or (not p3) (not p5) (not p1))
  (or p5 p0)
  (or p2 (not p0) p1)
  (or p3 (not p4) (not p2))
  (or (not p2) p0)
  (or p2 (not p4))
  (or p4 p3)
  (or p1 p0)
))
(check-sat)
(get-model)
