( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun p1 () Bool )
( declare-fun p2 () Bool )
( declare-fun p3 () Bool )
( declare-fun p4 () Bool )

( assert
 ( or (or (or p1 p2) p3)  p4 )
)

( check-sat )
( get-model )
