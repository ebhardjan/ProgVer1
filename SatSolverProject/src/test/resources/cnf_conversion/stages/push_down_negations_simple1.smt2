( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun p1 () Bool )

( assert
  ( not (not p1) )
)

( check-sat )
( get-model )
