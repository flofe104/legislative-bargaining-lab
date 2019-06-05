module StateMonad exposing (State, andThen, return)

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


return : a -> State s a
return =
    Tuple.pair
