module Util exposing (..)

{-| -}


{-| <https://github.com/elm-community/list-extra/blob/8.7.0/src/List/Extra.elm#L298>

Find the first maximum element in a list using a comparable transformation

-}
maximumBy : { isBiggerThen : b -> b -> Bool, by : a -> b } -> List a -> Maybe a
maximumBy { isBiggerThen, by } ls =
    let
        maxBy x ( y, fy ) =
            let
                fx =
                    by x
            in
            if fx |> isBiggerThen fy then
                ( x, fx )

            else
                ( y, fy )
    in
    case ls of
        [ l_ ] ->
            Just l_

        l_ :: ls_ ->
            Just <| Tuple.first <| List.foldl maxBy ( l_, by l_ ) ls_

        _ ->
            Nothing


foldlWhileOk : (a -> b -> Result b b) -> b -> List a -> b
foldlWhileOk fun input list =
    case list of
        [] ->
            input

        head :: tail ->
            case fun head input of
                Ok output ->
                    foldlWhileOk fun output tail

                Err output ->
                    output
