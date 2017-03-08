( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun p1 () Bool )

( assert ( and
 (or p1)
 (or (not p1) p2 p3 )
 (or p1 p2)
 (or p2 (not p3))
))

( check-sat )
( get-model )