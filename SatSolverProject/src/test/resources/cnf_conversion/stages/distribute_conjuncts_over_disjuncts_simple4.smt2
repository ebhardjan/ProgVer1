( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun a () Bool )
( declare-fun b () Bool )
( declare-fun c () Bool )

( assert
    ( or a (or b c) )
)

( check-sat )
( get-model )
