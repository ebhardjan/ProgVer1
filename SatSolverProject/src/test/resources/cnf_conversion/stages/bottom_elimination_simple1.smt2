( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun p1 () Bool )

( assert
 ( and p1 false )
)

( check-sat )
( get-model )
