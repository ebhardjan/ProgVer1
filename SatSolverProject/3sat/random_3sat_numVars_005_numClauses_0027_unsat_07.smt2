(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun p2() Bool)
(declare-fun p4() Bool)
(declare-fun p3() Bool)
(declare-fun p0() Bool)
(declare-fun p1() Bool)
(assert
(and (or p2 p4 p3) (or (not p3) (not p2) (not p4)) (or p0 p4 (not p3)) (or p0 p4 p2) (or (not p1) (not p3) p4) (or p1 p4 p3) (or p2 (not p2) (not p1)) (or p3 (not p0) (not p1)) (or p3 p2 p1) (or (not p1) p0 p3) (or (not p4) (not p3) p0) (or (not p4) p3 p2) (or (not p0) (not p0) (not p0)) (or p4 p4 (not p0)) (or p3 (not p0) (not p0)) (or (not p3) p2 (not p1)) (or (not p2) (not p1) p2) (or (not p2) p1 p2) (or (not p1) p0 p4) (or p2 p3 p2) (or p0 (not p3) (not p3)) (or (not p2) p1 (not p2)) (or (not p3) p3 (not p0)) (or (not p4) (not p4) (not p2)) (or p4 (not p4) (not p3)) (or (not p1) p4 p2) (or p0 p1 (not p4)))
)
(check-sat)
(get-model)
