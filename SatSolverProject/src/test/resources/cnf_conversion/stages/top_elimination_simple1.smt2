( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun p1 () Bool )

( assert
 ( or p1 true )
)

( check-sat )
( get-model )
