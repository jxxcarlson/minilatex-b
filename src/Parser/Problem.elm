module Parser.Problem exposing (RecoveryData, getErrors, getRecoveryData)

{-|

@docs RecoveryData, getErrors, getRecoveryData

-}

import Parser.Expression as Expression exposing (Expression(..), Problem(..), SourceMap)
import Parser.TextCursor exposing (TextCursor)


{-| -}
type alias RecoveryData =
    { problem : Problem
    , deltaOffset : Int
    , textTruncation : Int
    , parseSubstitute : Expression
    }


{-| -}
getRecoveryData : TextCursor -> Problem -> Maybe RecoveryData
getRecoveryData tc_ problem =
    let
        oldSourceMap =
            Expression.dummySourceMap

        newSourceMap =
            { oldSourceMap | blockOffset = tc_.blockIndex }
    in
    getRecoveryData_ problem
        |> Maybe.map (\r -> { r | parseSubstitute = Expression.setSourceMap newSourceMap r.parseSubstitute })


getRecoveryData_ : Problem -> Maybe RecoveryData
getRecoveryData_ problem =
    List.filter (\r -> Expression.equivalentProblem r.problem problem) recoveryData |> List.head


recoveryData : List RecoveryData
recoveryData =
    [ problemWithInlineMath, problemWithInlineMathParen, problemWithDisplayMath, problemWithEnvironment, problemWithMacro ]


problemWithInlineMath : RecoveryData
problemWithInlineMath =
    { problem = ExpectingTrailingDollarSign
    , deltaOffset = 1
    , textTruncation = 1
    , parseSubstitute =
        LXList
            [ Macro "red"
                Nothing
                [ Text (String.fromChar '⚠' ++ " unmatched $ in" ++ String.fromChar '\u{00A0}') Expression.dummySourceMap
                ]
                Expression.dummySourceMap
            ]
    }


problemWithInlineMathParen : RecoveryData
problemWithInlineMathParen =
    { problem = ExpectingTrailingBackslashParen
    , deltaOffset = 2
    , textTruncation = 2
    , parseSubstitute =
        LXList
            [ Macro "red"
                Nothing
                [ Text (String.fromChar '⚠' ++ " unmatched \\( in" ++ String.fromChar '\u{00A0}') Expression.dummySourceMap
                ]
                Expression.dummySourceMap
            ]
    }


problemWithDisplayMath : RecoveryData
problemWithDisplayMath =
    { problem = ExpectingTrailingDoubleDollarSign
    , deltaOffset = 2
    , textTruncation = 2 -- corresponds to "$$"
    , parseSubstitute =
        LXList
            [ Macro "red"
                Nothing
                [ Text (String.fromChar '⚠' ++ " unmatched $$ in" ++ String.fromChar '\u{00A0}') Expression.dummySourceMap
                ]
                Expression.dummySourceMap
            ]
    }


problemWithMacro : RecoveryData
problemWithMacro =
    { problem = ExpectingRightBrace
    , deltaOffset = 0
    , textTruncation = 1
    , parseSubstitute =
        LXList
            [ Macro "red"
                Nothing
                [ Text "!! missing right brace in \\"
                    Expression.dummySourceMap
                ]
                Expression.dummySourceMap
            ]
    }


problemWithEnvironment : RecoveryData
problemWithEnvironment =
    { problem = ExpectingEndWord "dummy"
    , deltaOffset = 3
    , textTruncation = 2
    , parseSubstitute =
        LXList
            [ Macro "red"
                Nothing
                [ DisplayMath "!! unmatched \\begin .. \\end: " Expression.dummySourceMap
                ]
                Expression.dummySourceMap
            ]
    }



-- ERRORS


{-| getErrors takes parsed text as input and produces a list of errors as output.
-}
getErrors : List (List Expression) -> List Expression
getErrors list =
    list
        |> List.map getErrors_
        |> List.concat


getErrors_ : List Expression -> List Expression
getErrors_ list =
    List.filter (\e -> isError e) list


isError : Expression -> Bool
isError expr =
    case expr of
        LXError _ _ _ ->
            True

        _ ->
            False
