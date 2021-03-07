module Compiler.Differ exposing
    ( diff
    , DiffRecord, blockAfter_, blocksBefore_, getBlockIndex, range, rangeOfBlocks, slice
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
    , deltaInSource : List String
    , deltaInTarget : List String
    }


range : DiffRecord -> { firstChange : Int, lengthInSource : Int, lengthInTarget : Int }
range dr =
    { firstChange = List.length dr.commonInitialSegment
    , lengthInSource = List.length dr.deltaInSource
    , lengthInTarget = List.length dr.deltaInTarget
    }


textInRange : Int -> Int -> List String -> List String
textInRange start end strings =
    slice start (end + 1) strings


{-| Attempt to return the offsets for the first and last chunks (blocks) in
the source text whose extent contains all the source text changes. The
extent is the set of lines that begins with the first line of the first block
and ends with the last line of the last block.
-}
rangeOfBlocks : DiffRecord -> List (List Int) -> Maybe ( Int, Int )
rangeOfBlocks dr sourceMapIndex =
    let
        range_ =
            range dr

        first =
            range_.firstChange

        last =
            first + range_.lengthInSource
    in
    case ( getBlockIndex first sourceMapIndex, getBlockIndex last sourceMapIndex ) of
        ( Just j, Just k ) ->
            Just ( j, k )

        _ ->
            Nothing


blocksBetween_ : Int -> Int -> List (List String) -> List (List String)
blocksBetween_ i j blocks =
    slice i (j + 1) blocks


blocksBefore_ : Int -> List (List String) -> List (List String)
blocksBefore_ i blocks =
    slice 0 i blocks


blockAfter_ : Int -> List (List String) -> List (List String)
blockAfter_ i blocks =
    slice i (1 + List.length blocks) blocks



--changedBlocks : DiffRecord -> List (List Int) -> List (List String) -> List (List String)
--changedBlocks dr sourceMapIndex blocks =
--    case rangeOfChunks dr sourceMapIndex of
--        Just ( i, j ) ->
--            changedBlocks_ i j blocks
--
--        _ ->
--            blocks
--
--unchangedBlocks : DiffRecord -> List (List Int) -> List (List String) -> (List (List String), ist (List String))
--unchangedBlocks dr sourceMapIndex blocks =
--    case rangeOfChunks dr sourceMapIndex of
--        Just ( i, j ) ->
--            changedBlocks_ i j blocks
--
--        _ ->
--            blocks


{-| The primary data is a list of strings. These strings are
grouped in consecutive groups of strings called blocks
by a function

     List String -> List Block

where Block: List String.

The function `getBlockIndex` returns the index in List Block of the
Block containing the string with index lineNumber.

-}
getBlockIndex : Int -> List (List Int) -> Maybe Int
getBlockIndex lineNumber sourceMapIndex =
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



-- HELPER


slice : Int -> Int -> List a -> List a
slice from to items =
    items
        |> List.drop from
        |> List.take (to - from)


dropLast : Int -> List a -> List a
dropLast k x =
    x |> List.reverse |> List.drop k |> List.reverse
