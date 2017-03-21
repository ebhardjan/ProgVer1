( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun a () Bool )
( declare-fun b () Bool )
( declare-fun c () Bool )

( assert ( and
 ( or a (not b) )
 ( or a b )
 ( or (not a) b c )
 ( or (not a) (not b) (not c) )
))

( check-sat )
( get-model )
