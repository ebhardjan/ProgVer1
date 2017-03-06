( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun a () Bool )
( declare-fun b () Bool )
( declare-fun c () Bool )
( declare-fun d () Bool )

( assert (or
 ( or a b )
 ( and c d)
))

( check-sat )
( get-model )
