module Yaml.Parser.Util exposing
    ( doubleQuotes
    , indented
    , isColon
    , isComma
    , isListEnd
    , isListStart
    , isNewLine
    , isRecordEnd
    , isRecordStart
    , isSpace
    , multiline
    , neither
    , neither3
    , postProcessFoldedString
    , postProcessString
    , remaining
    , singleQuotes
    , spaces
    , threeDashes
    , threeDots
    , whitespace
    )

import Parser as P exposing ((|.), (|=))
import Parser.Workaround
import Regex exposing (Regex)



-- QUESTIONS


{-| -}
isColon : Char -> Bool
isColon x =
    x == ':'


{-| -}
isComma : Char -> Bool
isComma x =
    x == ','


{-| -}
isSpace : Char -> Bool
isSpace x =
    x == ' '


{-| -}
isNewLine : Char -> Bool
isNewLine x =
    x == '\n'


{-| -}
isListStart : Char -> Bool
isListStart x =
    x == '['


{-| -}
isListEnd : Char -> Bool
isListEnd x =
    x == ']'


{-| -}
isRecordStart : Char -> Bool
isRecordStart x =
    x == '{'


{-| -}
isRecordEnd : Char -> Bool
isRecordEnd x =
    x == '}'


{-| -}
isSingleQuote : Char -> Bool
isSingleQuote x =
    x == '\''


{-| -}
isDoubleQuote : Char -> Bool
isDoubleQuote x =
    x == '"'


{-| -}
neither : (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
neither f1 f2 char =
    not (f1 char) && not (f2 char)


{-| -}
neither3 : (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
neither3 f1 f2 f3 char =
    not (f1 char) && not (f2 char) && not (f3 char)



--


{-| -}
threeDashes : P.Parser ()
threeDashes =
    P.symbol "---"


{-| -}
threeDots : P.Parser ()
threeDots =
    P.symbol "..."


{-| -}
spaces : P.Parser ()
spaces =
    P.chompWhile isSpace


{-| -}
whitespace : P.Parser ()
whitespace =
    let
        step : a -> P.Parser (P.Step () ())
        step _ =
            P.oneOf
                [ P.succeed (P.Loop ())
                    |. comment
                , P.succeed (P.Loop ())
                    |. P.symbol " "
                , P.succeed (P.Loop ())
                    |. P.symbol "\n"
                , P.succeed (P.Done ())
                ]
    in
    P.loop () step


{-| -}
comment : P.Parser ()
comment =
    Parser.Workaround.lineCommentBefore "#"



-- STRINGS


{-| -}
multiline : Int -> P.Parser String
multiline indent =
    P.loop [] (multilineStep indent)


multilineStep : Int -> List String -> P.Parser (P.Step (List String) String)
multilineStep indent lines =
    let
        multilineString : List String -> String
        multilineString lines_ =
            String.join "\n" (List.reverse lines_)

        conclusion : String -> Maybe ( Int, Int ) -> P.Step (List String) String
        conclusion line next =
            case next of
                Just ( emptyLineCount, indent_ ) ->
                    if indent_ > indent then
                        P.Loop
                            ((line ++ String.repeat emptyLineCount "\n")
                                :: lines
                            )

                    else
                        P.Done (multilineString (line :: lines))

                Nothing ->
                    P.Done (multilineString (line :: lines))
    in
    P.oneOf
        [ P.succeed conclusion
            |= characters (\c -> c /= '\n')
            |= P.oneOf
                [ P.succeed (\e i -> Just ( e, i ))
                    |. P.chompIf isNewLine
                    |. spaces
                    |= emptyLines
                    |= P.getCol
                , P.succeed Nothing
                    |. P.end
                ]
        , P.succeed (P.Done <| multilineString lines)
        ]


emptyLines : P.Parser Int
emptyLines =
    P.loop 0 emptyLinesStep


emptyLinesStep : Int -> P.Parser (P.Step Int Int)
emptyLinesStep count =
    P.oneOf
        [ P.succeed (P.Loop (count + 1))
            |. P.chompIf isNewLine
            |. spaces
        , P.succeed (P.Done count)
        ]


{-| -}
characters : (Char -> Bool) -> P.Parser String
characters isOk =
    let
        done : List String -> P.Step state String
        done chars =
            chars
                |> List.reverse
                |> String.concat
                |> P.Done

        more : List b -> b -> P.Step (List b) a
        more chars char =
            char
                :: chars
                |> P.Loop

        step : List String -> P.Parser (P.Step (List String) String)
        step chars =
            P.oneOf
                [ P.succeed (done chars)
                    |. comment
                , P.chompIf isOk
                    |> P.getChompedString
                    |> P.map (more chars)
                , P.succeed (done chars)
                ]
    in
    P.loop [] step


{-| -}
characters_ : (Char -> Bool) -> P.Parser String
characters_ isOk =
    P.chompWhile isOk
        |> P.getChompedString


{-| -}
singleQuotes : P.Parser String
singleQuotes =
    P.succeed (String.replace "\\" "\\\\")
        |. P.symbol "'"
        |= characters_ (\c -> not (isSingleQuote c))
        |. P.symbol "'"
        |. spaces


{-| -}
doubleQuotes : P.Parser String
doubleQuotes =
    P.succeed identity
        |. P.symbol "\""
        |= characters_ (\c -> not (isDoubleQuote c))
        |. P.symbol "\""
        |. spaces


{-| -}
remaining : P.Parser String
remaining =
    Parser.Workaround.chompUntilEndOrBefore "\n...\n"
        |> P.getChompedString


postProcessString : String -> String
postProcessString str =
    if isLiteralString str then
        postProcessLiteralString str

    else
        str
            |> String.replace "\n" " "
            |> postProcessFoldedString


postProcessFoldedString : String -> String
postProcessFoldedString str =
    str
        |> Regex.replace multipleSpacesRegex
            (\match ->
                if String.contains "\n\n" match.match then
                    "\n"

                else
                    " "
            )


multipleSpacesRegex : Regex
multipleSpacesRegex =
    Regex.fromString "\\s{2,}"
        |> Maybe.withDefault Regex.never


isLiteralString : String -> Bool
isLiteralString str =
    case String.split "\n" str of
        "|" :: _ ->
            True

        _ ->
            False


postProcessLiteralString : String -> String
postProcessLiteralString str =
    if String.startsWith "|\n" str then
        let
            content : String
            content =
                String.dropLeft 2 str

            split : List String
            split =
                String.split "\n" content

            leadingSpaces : Int
            leadingSpaces =
                countLeadingSpacesInMultiline split
        in
        removeLeadingSpaces leadingSpaces split

    else
        str


removeLeadingSpaces : Int -> List String -> String
removeLeadingSpaces count str =
    str
        |> List.map (String.dropLeft count)
        |> String.join "\n"


countLeadingSpacesInMultiline : List String -> Int
countLeadingSpacesInMultiline str =
    case str of
        head :: _ ->
            countLeadingSpacesInString head

        [] ->
            0


countLeadingSpacesInString : String -> Int
countLeadingSpacesInString str =
    let
        countHelper : String -> Int -> Int
        countHelper s count =
            if String.startsWith " " s then
                countHelper (String.dropLeft 1 s) (count + 1)

            else
                count
    in
    countHelper str 0



-- INDENT


{-| -}
indented : Int -> { smaller : P.Parser a, exactly : P.Parser a, larger : Int -> P.Parser a, ending : P.Parser a } -> P.Parser a
indented indent next =
    let
        check : Int -> P.Parser a
        check actual =
            P.oneOf
                [ P.andThen (\_ -> next.ending) P.end
                , P.andThen (\_ -> next.ending) (P.symbol "\n...\n")
                , if actual == indent then
                    next.exactly

                  else if actual > indent then
                    next.larger actual

                  else
                    next.smaller
                ]
    in
    P.succeed identity
        |. whitespace
        |= P.getCol
        |> P.andThen check
