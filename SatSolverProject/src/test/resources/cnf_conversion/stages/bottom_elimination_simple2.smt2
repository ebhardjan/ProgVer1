( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun p1 () Bool )
( declare-fun p2() Bool )

( assert
 ( and p1 ( and false p2 ) )
)

( check-sat )
( get-model )
