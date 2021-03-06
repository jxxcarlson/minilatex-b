module Compiler.Differ exposing
    ( DiffRecord
    , diff
    , range
    , rangeOfChunks
    )

import Maybe.Extra
import Paragraph


{-| This module is used to speed up parsing-rendering by
comparing the old and new lists of paragraphs, noting the changes,
then parsing and rendering the changed paragraphs.


# API

@docs EditRecord, emptyStringRecord, emptyHtmlMsgRecord, isEmpty, init, diff, prefixer, update, simpleDifferentialRender

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
    List.filter (\( k, x ) -> List.member lineNumber x) indexed
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


commonTerminalSegment : List String -> List String -> List String
commonTerminalSegment x y =
    let
        cis =
            commonInitialSegment x y
    in
    commonTerminalSegmentAux cis x y


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


takeLast : Int -> List a -> List a
takeLast k x =
    x |> List.reverse |> List.take k |> List.reverse


part n =
    List.repeat n loremIpsum1 |> List.concat


text1 n =
    part n ++ loremIpsum1 ++ part n


text2 n =
    part n ++ loremIpsum2 ++ part n


test n =
    (diff (text1 n) (text2 n)).middleSegmentInTarget


loremIpsum1 =
    Paragraph.lines
        { maximumWidth = 50
        , optimalWidth = 45
        , stringWidth = String.length
        }
        loremIpsum1_


loremIpsum2 =
    Paragraph.lines
        { maximumWidth = 50
        , optimalWidth = 45
        , stringWidth = String.length
        }
        loremIpsum2_


loremIpsum1_ =
    """
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras finibus dolor ut blandit pellentesque. Sed nec nulla id mi facilisis tristique at sed ante. Morbi pharetra quis nibh vitae aliquam. Proin eget orci nec lectus ornare tincidunt. Ut accumsan est lacus, at congue arcu iaculis a. Morbi a dolor rhoncus nisl rutrum fringilla. Pellentesque diam nunc, suscipit in urna euismod, interdum egestas arcu. Sed tempor porttitor dolor vel malesuada. Nunc pellentesque dolor eget sapien viverra egestas. Fusce auctor rutrum dolor non convallis. Aenean vulputate magna eu mauris pellentesque, non dignissim magna posuere. Vivamus convallis aliquam lectus ultrices tincidunt. Nunc mollis nec arcu ut consequat. Quisque ut velit tincidunt sem ultrices semper.

Quisque quam lorem, accumsan eget congue rutrum, consectetur sit amet magna. Mauris sollicitudin varius justo, non maximus turpis commodo id. In posuere nisl placerat ipsum rutrum, eu venenatis sapien consectetur. Donec faucibus consequat nibh sed euismod. In fermentum metus metus, vitae blandit ex aliquet commodo. Proin rutrum velit non cursus volutpat. Fusce eget lectus non elit mattis finibus at in lectus. Pellentesque finibus efficitur feugiat.

Etiam imperdiet finibus augue, eu elementum mi scelerisque a. Phasellus ut ipsum fermentum, cursus orci id, commodo felis. Maecenas et arcu iaculis erat tristique fringilla in eget mi. Nunc ligula risus, lobortis non blandit id, commodo dapibus justo. Vivamus faucibus facilisis nisl, dictum suscipit sem tincidunt nec. Fusce cursus tellus consectetur enim sodales tristique. Sed ultrices arcu sed arcu faucibus laoreet ac quis justo.

Curabitur sapien massa, convallis quis tortor quis, ullamcorper porta dui. Aliquam erat volutpat. In a justo aliquam, sagittis eros vel, mattis tortor. Suspendisse sed enim porta, scelerisque eros in, porttitor neque. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Donec a mauris posuere, pretium massa a, blandit tortor. Morbi porttitor ullamcorper rhoncus. Sed iaculis leo id porta pharetra. Praesent eget nulla ante. Quisque rutrum suscipit erat, quis facilisis sapien aliquet non. Integer neque nibh, varius sed accumsan sagittis, tempus sit amet tortor. Quisque auctor ullamcorper justo, elementum placerat nisl efficitur at. Suspendisse condimentum lectus et egestas tempus. Aenean consectetur mi convallis tincidunt posuere. Proin id leo eget sapien lacinia tempus.

Pellentesque pulvinar orci at nisl porttitor dapibus. Nullam augue leo, sodales ac risus in, malesuada vulputate velit. Etiam et urna suscipit, semper quam in, tincidunt ipsum. In hac habitasse platea dictumst. Phasellus tempus sem lorem, non mollis dolor facilisis ac. Proin sagittis semper nunc et aliquet. Phasellus purus metus, porta a est eu, tempor faucibus quam. Ut congue ipsum id nisi rhoncus, vel interdum tellus ultricies. Curabitur in neque at velit convallis sollicitudin sed in est. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aliquam vel erat mauris. Sed eget gravida arcu. Donec orci dui, viverra sit amet leo at, interdum imperdiet libero.

Mauris posuere lorem justo, ac pretium elit rutrum id. Donec et mollis magna. Etiam orci ligula, viverra dignissim facilisis nec, convallis non lacus. Aliquam varius pretium mauris et tempus. Duis et finibus augue, a consectetur massa. Suspendisse potenti. Vestibulum viverra, enim et vehicula auctor, mauris lectus pharetra ligula, nec mollis libero tellus in elit.

Vestibulum iaculis, mi eget molestie feugiat, arcu lectus fringilla magna, sit amet porta ligula libero ac lacus. Mauris vehicula mollis metus sed finibus. Cras arcu nibh, venenatis sit amet imperdiet at, sagittis sit amet odio. Sed eu nibh justo. Cras tristique gravida pretium. Donec eget maximus enim. Praesent sit amet convallis diam. Vestibulum vel ante vel est dignissim interdum. Aliquam dignissim purus ut dolor suscipit hendrerit. Morbi a libero commodo, facilisis lectus elementum, ultrices dui. Proin interdum ut odio nec ornare. Fusce at tincidunt nulla. Sed vulputate odio sed lacus tempor, vel consectetur lectus laoreet. In efficitur ex non tellus pulvinar, in dapibus purus vehicula. Duis sed enim at libero sollicitudin efficitur. Suspendisse tempor nibh lorem, non pharetra dolor placerat eu.

Duis eget nunc imperdiet libero fringilla faucibus. Morbi at bibendum metus. Aenean ac nibh neque. Cras dapibus viverra metus, quis eleifend justo egestas facilisis. Nam odio turpis, viverra sit amet felis a, vestibulum accumsan purus. Mauris vel quam vel justo varius accumsan. Praesent scelerisque nisl vel ipsum pretium, non tempor dui auctor. Sed eget gravida elit, at tincidunt quam. Sed rutrum sed nibh vitae interdum.

Ut luctus sodales erat, non fringilla elit dignissim sed. Ut sodales orci risus, id dignissim velit tincidunt id. Donec laoreet ultricies ex, sed faucibus orci dapibus in. Fusce dignissim orci erat, vitae vulputate lacus congue eget. Suspendisse eget ipsum ac ipsum rhoncus pharetra. Curabitur gravida turpis eget metus luctus iaculis. Cras aliquet mauris orci, sed lobortis lacus sagittis vel. Fusce hendrerit turpis non risus hendrerit accumsan. Aenean porta faucibus eros aliquet semper.

Mauris iaculis non justo laoreet tincidunt. Phasellus pellentesque ornare purus, quis elementum arcu consectetur quis. Morbi tempor sed orci eleifend hendrerit. Curabitur tempus vehicula turpis dapibus dignissim. Aenean varius dapibus eros, eu aliquet libero ultricies ac. Nullam pretium, felis vitae sollicitudin volutpat, urna arcu vulputate lacus, scelerisque eleifend nisi eros eu dolor. Nulla mollis sit amet turpis ut ornare.
"""


loremIpsum2_ =
    """
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras finibus dolor ut blandit pellentesque. Sed nec nulla id mi facilisis tristique at sed ante. Morbi pharetra quis nibh vitae aliquam. Proin eget orci nec lectus ornare tincidunt. Ut accumsan est lacus, at congue arcu iaculis a. Morbi a dolor rhoncus nisl rutrum fringilla. Pellentesque diam nunc, suscipit in urna euismod, interdum egestas arcu. Sed tempor porttitor dolor vel malesuada. Nunc pellentesque dolor eget sapien viverra egestas. Fusce auctor rutrum dolor non convallis. Aenean vulputate magna eu mauris pellentesque, non dignissim magna posuere. Vivamus convallis aliquam lectus ultrices tincidunt. Nunc mollis nec arcu ut consequat. Quisque ut velit tincidunt sem ultrices semper.

Quisque quam lorem, accumsan eget congue rutrum, consectetur sit amet magna. Mauris sollicitudin varius justo, non maximus turpis commodo id. In posuere nisl placerat ipsum rutrum, eu venenatis sapien consectetur. Donec faucibus consequat nibh sed euismod. In fermentum metus metus, vitae blandit ex aliquet commodo. Proin rutrum velit non cursus volutpat. Fusce eget lectus non elit mattis finibus at in lectus. Pellentesque finibus efficitur feugiat.

Etiam imperdiet finibus augue, eu elementum mi scelerisque a. Phasellus ut ipsum fermentum, cursus orci id, commodo felis. Maecenas et arcu iaculis erat tristique fringilla in eget mi. Nunc ligula risus, lobortis non blandit id, commodo dapibus justo. Vivamus faucibus facilisis nisl, dictum suscipit sem tincidunt nec. Fusce cursus tellus consectetur enim sodales tristique. Sed ultrices arcu sed arcu faucibus laoreet ac quis justo.

Curabitur sapien massa, convallis quis tortor quis, ullamcorper porta dui. Aliquam erat volutpat. In a justo aliquam, sagittis eros vel, mattis tortor. Suspendisse sed enim porta, scelerisque eros in, porttitor neque. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Donec a mauris posuere, pretium massa a, blandit tortor. Morbi porttitor ullamcorper rhoncus. Sed iaculis leo id porta pharetra. Praesent eget nulla ante. Quisque rutrum suscipit erat, quis facilisis sapien aliquet non. Integer neque nibh, varius sed accumsan sagittis, tempus sit amet tortor. Quisque auctor ullamcorper justo, elementum placerat nisl efficitur at. Suspendisse condimentum lectus et egestas tempus. Aenean consectetur mi convallis tincidunt posuere. Proin id leo eget sapien lacinia tempus.

Pellentesque pulvinar orci at nisl porttitor dapibus. Nullam augue leo, sodales ac risus in, malesuada vulputate velit. Etiam et urna suscipit, semper quam in, tincidunt ipsum. In hac habitasse platea dictumst. Phasellus tempus sem lorem, non mollis dolor facilisis ac. Proin sagittis semper nunc et aliquet. Phasellus purus metus, porta a est eu, tempor faucibus quam. Ut congue ipsum id nisi rhoncus, vel interdum tellus ultricies. Curabitur in neque at velit convallis sollicitudin sed in est. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aliquam vel erat mauris. Sed eget gravida arcu. Donec orci dui, viverra sit amet leo at, interdum imperdiet libero.

Mauris posuere lorem justo, ac pretium elit rutrum idx. Donec et mollis magna. Etiam orci ligula, viverra dignissim facilisis nec, convallis non lacus. Aliquam varius pretium mauris et tempus. Duis et finibus augue, a consectetur massa. Suspendisse potenti. Vestibulum viverra, enim et vehicula auctor, mauris lectus pharetra ligula, nec mollis libero tellus in elit.

Vestibulum iaculis, mi eget molestie feugiat, arcu lectus fringilla magna, sit amet porta ligula libero ac lacus. Mauris vehicula mollis metus sed finibus. Cras arcu nibh, venenatis sit amet imperdiet at, sagittis sit amet odio. Sed eu nibh justo. Cras tristique gravida pretium. Donec eget maximus enim. Praesent sit amet convallis diam. Vestibulum vel ante vel est dignissim interdum. Aliquam dignissim purus ut dolor suscipit hendrerit. Morbi a libero commodo, facilisis lectus elementum, ultrices dui. Proin interdum ut odio nec ornare. Fusce at tincidunt nulla. Sed vulputate odio sed lacus tempor, vel consectetur lectus laoreet. In efficitur ex non tellus pulvinar, in dapibus purus vehicula. Duis sed enim at libero sollicitudin efficitur. Suspendisse tempor nibh lorem, non pharetra dolor placerat eu.

Duis eget nunc imperdiet libero fringilla faucibus. Morbi at bibendum metus. Aenean ac nibh neque. Cras dapibus viverra metus, quis eleifend justo egestas facilisis. Nam odio turpis, viverra sit amet felis a, vestibulum accumsan purus. Mauris vel quam vel justo varius accumsan. Praesent scelerisque nisl vel ipsum pretium, non tempor dui auctor. Sed eget gravida elit, at tincidunt quam. Sed rutrum sed nibh vitae interdum.

Ut luctus sodales erat, non fringilla elit dignissim sed. Ut sodales orci risus, id dignissim velit tincidunt id. Donec laoreet ultricies ex, sed faucibus orci dapibus in. Fusce dignissim orci erat, vitae vulputate lacus congue eget. Suspendisse eget ipsum ac ipsum rhoncus pharetra. Curabitur gravida turpis eget metus luctus iaculis. Cras aliquet mauris orci, sed lobortis lacus sagittis vel. Fusce hendrerit turpis non risus hendrerit accumsan. Aenean porta faucibus eros aliquet semper.

Mauris iaculis non justo laoreet tincidunt. Phasellus pellentesque ornare purus, quis elementum arcu consectetur quis. Morbi tempor sed orci eleifend hendrerit. Curabitur tempus vehicula turpis dapibus dignissim. Aenean varius dapibus eros, eu aliquet libero ultricies ac. Nullam pretium, felis vitae sollicitudin volutpat, urna arcu vulputate lacus, scelerisque eleifend nisi eros eu dolor. Nulla mollis sit amet turpis ut ornare.
"""
