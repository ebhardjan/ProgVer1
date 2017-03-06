( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun p1 () Bool )
( declare-fun p2 () Bool )
( declare-fun p3 () Bool )

( assert
  ( not (or (not (not (or p1 p2) ) ) p3 ) )
)

( check-sat )
( get-model )
