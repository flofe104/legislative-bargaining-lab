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


set : State s a -> b -> State s b
set h b =
    \s ->
        let
            ( a, s2 ) =
                h s
        in
        ( b, s2 )


return : a -> State s a
return x =
    \s -> ( x, s )
