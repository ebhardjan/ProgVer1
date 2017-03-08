( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun p1 () Bool )
( declare-fun p2 () Bool )

( assert ( and
 (or p1 p2)
 (or p1 (not p2))
 (or (not p1) p2)
 (or (not p1) (not p2))
))

( check-sat )
( get-model )