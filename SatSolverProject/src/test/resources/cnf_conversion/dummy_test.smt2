( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun a () Bool )
( declare-fun b () Bool )
( declare-fun c () Bool )
( declare-fun d () Bool )
( declare-fun e () Bool )

( assert (or
 ( or a b )
 ( or c d )
 ( and e e)
))

( check-sat )
( get-model )
