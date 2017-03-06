( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun a () Bool )
( declare-fun b () Bool )
( declare-fun c () Bool )
( declare-fun d () Bool )

( assert (and
 ( and a b )
 ( or c d)
))

( check-sat )
( get-model )
