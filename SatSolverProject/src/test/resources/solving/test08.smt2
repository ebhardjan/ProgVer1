( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun p1 () Bool )
( declare-fun p2 () Bool )
( declare-fun p3 () Bool )

( assert ( and
 (or p1 p2)
 (or p1 (not p2))
 (or (not p1) p2 (not p3))
 (or (not p1) (not p2) p3)
))

( check-sat )
( get-model )
