(set-option :produce-models true)
(set-logic QF_UF)
(declare-fun d() Bool)
(declare-fun a() Bool)
(declare-fun e() Bool)
(declare-fun c() Bool)
(declare-fun f() Bool)
(declare-fun b() Bool)
(assert
(and
  (or e b c)
  (or (not e) (not a) (not b))
  (or (not d) a)
  (or e a (not f))
  (or (not a) (not c) b)
  (or e b (not d) e)
  (or e (not b) (not c))
  (or f d)
  (or (not f) (not e) (not e))
  )
)
(check-sat)
(get-model)