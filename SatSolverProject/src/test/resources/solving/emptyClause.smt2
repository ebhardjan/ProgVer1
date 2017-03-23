( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun p1 () Bool )

( assert (and (or true)))

( check-sat )
( get-model )
