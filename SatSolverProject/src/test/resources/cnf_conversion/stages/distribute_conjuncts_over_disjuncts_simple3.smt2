( set-option :produce-models true )
( set-logic QF_UF )

( declare-fun a () Bool )
( declare-fun b () Bool )
( declare-fun c () Bool )
( declare-fun d () Bool )

( assert
    ( or a (and (or b (and c d)) e) )
)

( check-sat )
( get-model )
