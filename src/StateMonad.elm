module StateMonad exposing (State, andThen, return, set)

--type alias


type alias State s a =
    s -> ( a, s )


andThen : State s a -> (a -> State s b) -> State s b
andThen h f =
    \s ->
        let
            ( a, newState ) =
                h s

            g =
                f a
        in
        g newState


set : b -> State s b
set b s =
    ( b, s )


return : a -> State s a
return x =
    \s -> ( x, s )
