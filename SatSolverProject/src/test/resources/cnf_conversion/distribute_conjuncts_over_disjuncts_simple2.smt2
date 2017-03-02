( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun a () Bool )
( declare-fun b () Bool )
( declare-fun c () Bool )
( declare-fun d () Bool )
( declare-fun e () Bool )
( declare-fun f () Bool )

( assert (or
  ( or a (and c d) b (and e f) )
  a
))

( check-sat )
( get-model )
