(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p0() Bool)
(declare-fun p2() Bool)
(declare-fun p1() Bool)
(declare-fun p4() Bool)
(declare-fun p5() Bool)
(declare-fun p3() Bool)
(assert
(and
  (or (not p0) (not p2) p1)
  (or (not p4) (not p2))
  (or p4 (not p2))
  (or p2 (not p3))
  (or p3 p0)
  (or p2 (not p1))
  (or p2 p1 p3)
))
(check-sat)
(get-model)
