module Compiler.Differ exposing (blockAfter_, blocksBefore_, rangeOfBlocks, slice)

{-| This module is used to speed up parsing-rendering by
comparing the old and new lists of paragraphs, noting the changes,
then parsing and rendering the changed paragraphs.


# API

@docs blockAfter_, blocksBefore_, rangeOfBlocks, slice

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


blocksBetween_ : Int -> Int -> List (List a) -> List (List a)
blocksBetween_ i j blocks =
    slice i (j + 1) blocks


{-| -}
blocksBefore_ : Int -> List (List a) -> List (List a)
blocksBefore_ i blocks =
    slice 0 i blocks


numberOfinesBeforeBlockWithIndex_ : Int -> List (List a) -> Int
numberOfinesBeforeBlockWithIndex_ i blocks =
    blocksBefore_ i blocks |> List.concat |> List.length


{-| -}
blockAfter_ : Int -> List (List a) -> List (List a)
blockAfter_ i blocks =
    slice i (1 + List.length blocks) blocks


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



-- HELPER


{-| -}
slice : Int -> Int -> List a -> List a
slice from to items =
    items
        |> List.drop from
        |> List.take (to - from)
