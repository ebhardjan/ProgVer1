( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun p0 () Bool )
( declare-fun p1 () Bool )
( declare-fun p3 () Bool )
( declare-fun p4 () Bool )

( assert (not (or p3 p4 p3)) )

( check-sat )
( get-model )
