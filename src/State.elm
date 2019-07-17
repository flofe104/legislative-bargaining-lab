module State exposing (State, andThen, get, put, return)


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


get : State s s
get s =
    ( s, s )


put : s -> State s ()
put s =
    \_ -> ( (), s )


return : a -> State s a
return =
    Tuple.pair
