( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun p1 () Bool )

( assert ( and
 ( or p1 p1 )
 ( or (not p1) (not p1) )
))

( check-sat )
( get-model )
