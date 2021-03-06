module Compiler.Differ exposing
    ( diff
    , DiffRecord, range, rangeOfChunks
    )

{-| This module is used to speed up parsing-rendering by
comparing the old and new lists of paragraphs, noting the changes,
then parsing and rendering the changed paragraphs.


# API

@docs EditRecord, emptyStringRecord, emptyHtmlMsgRecord, isEmpty, init, diff, update, simpleDifferentialRender

-}

{- TYPES -}


type alias DiffRecord =
    { commonInitialSegment : List String
    , commonTerminalSegment : List String
    , middleSegmentInSource : List String
    , middleSegmentInTarget : List String
    }


range : DiffRecord -> { firstChange : Int, lengthInSource : Int, lengthInTarget : Int }
range dr =
    { firstChange = List.length dr.commonInitialSegment
    , lengthInSource = List.length dr.middleSegmentInSource
    , lengthInTarget = List.length dr.middleSegmentInTarget
    }


rangeOfChunks : DiffRecord -> List (List Int) -> Maybe ( Int, Int )
rangeOfChunks dr sourceMapIndex =
    let
        range_ =
            range dr

        first =
            range_.firstChange

        last =
            first + range_.lengthInSource
    in
    case ( getChunkOffset first sourceMapIndex, getChunkOffset last sourceMapIndex ) of
        ( Just j, Just k ) ->
            Just ( j, k )

        _ ->
            Nothing


getChunkOffset : Int -> List (List Int) -> Maybe Int
getChunkOffset lineNumber sourceMapIndex =
    let
        indexed =
            List.indexedMap (\k x -> ( k, x )) sourceMapIndex
    in
    List.filter (\( _, x ) -> List.member lineNumber x) indexed
        |> List.head
        |> Maybe.map Tuple.first


{-| Let u and v be two lists of strings. Write them as
u = axb, v = ayb, where a is the greatest common prefix
and b is the greatest common suffix. Return DiffRecord a b x y
-}
diff : List String -> List String -> DiffRecord
diff u v =
    let
        a =
            commonInitialSegment u v

        b_ =
            commonTerminalSegmentAux a u v

        la =
            List.length a

        lb =
            List.length b_

        x =
            u |> List.drop la |> dropLast lb

        y =
            v |> List.drop la |> dropLast lb

        b =
            if la == List.length u then
                []

            else
                b_
    in
    DiffRecord a b x y


commonInitialSegment : List String -> List String -> List String
commonInitialSegment x y =
    if x == [] then
        []

    else if y == [] then
        []

    else
        let
            a =
                List.take 1 x

            b =
                List.take 1 y
        in
        if a == b then
            a ++ commonInitialSegment (List.drop 1 x) (List.drop 1 y)

        else
            []


commonTerminalSegmentAux : List String -> List String -> List String -> List String
commonTerminalSegmentAux cis x y =
    let
        n =
            List.length cis

        xx =
            List.drop n x |> List.reverse

        yy =
            List.drop n y |> List.reverse
    in
    commonInitialSegment xx yy |> List.reverse


dropLast : Int -> List a -> List a
dropLast k x =
    x |> List.reverse |> List.drop k |> List.reverse
