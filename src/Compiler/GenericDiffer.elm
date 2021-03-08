module Compiler.GenericDiffer exposing (DiffRecord, diff)

{-| The function diff: List t -> List t -> DiffRecord t
takes two lists as input and produces a "DiffRecord" which
shows where the two lists agree and where they disagree.

The DiffRecord identifies the longest common initial segment
and the longest common terminal segment as well as what is left
over: a largest "middle" segment in each list such that

    List1 =
        commonPrefix ++ middle1 ++ commonSuffix

    List2 =
        commonPrefix ++ middle2 ++ commonSuffix

@docs DiffRecord, diff

-}


{-| -}
type alias DiffRecord a =
    { commonInitialSegment : List a
    , commonTerminalSegment : List a
    , deltaInSource : List a
    , deltaInTarget : List a
    }


{-| Let u and v be two lists of things of type t. Write them as
u = axb, v = ayb, where a is the greatest common prefix
and b is the greatest common suffix. Return DiffRecord a b x y
-}
diff : List t -> List t -> DiffRecord t
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


commonInitialSegment : List a -> List a -> List a
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


commonTerminalSegmentAux : List a -> List a -> List a -> List a
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
