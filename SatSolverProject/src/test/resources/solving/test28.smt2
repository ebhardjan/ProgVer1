(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p2() Bool)
(declare-fun p3() Bool)
(declare-fun p5() Bool)
(declare-fun p0() Bool)
(declare-fun p4() Bool)
(declare-fun p1() Bool)
(assert
(and
  (or (not p2) (not p3))
  (or (not p5) p1 (not p3))
  (or (not p1) (not p5) p2)
  (or p3 p5 (not p0))
  (or p3 p3 (not p0) p4)
  (or p0 (not p4) p0)
  (or (not p0) p5 (not p1) (not p0))
  (or (not p5) (not p4))
  (or p0 p4)
))
(check-sat)
(get-model)