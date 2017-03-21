(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p1() Bool)
(declare-fun p3() Bool)
(declare-fun p2() Bool)
(declare-fun p0() Bool)
(declare-fun p5() Bool)
(declare-fun p4() Bool)
(assert
(and
  (or p1 (not p3))
  (or p3 p1)
  (or (not p0) p2)
  (or (not p1) (not p4) (not p4))
  (or p0 p4)
  (or (not p2) (not p1))
  (or (not p4) p3 p1 (not p5))
  (or p0 p5 p1)
))
(check-sat)
(get-model)
