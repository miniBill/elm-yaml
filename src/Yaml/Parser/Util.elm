module Yaml.Parser.Util exposing
    ( doubleQuotes
    , indented
    , multiline
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
    P.chompWhile (\c -> c == ' ')


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
                    |. P.symbol "\n"
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
            |. P.symbol "\n"
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
singleQuotes : P.Parser String
singleQuotes =
    P.succeed identity
        |. P.symbol "'"
        |= P.loop [] singleQuotesHelp
        |. spaces


{-| -}
doubleQuotes : P.Parser String
doubleQuotes =
    P.succeed identity
        |. P.symbol "\""
        |= P.loop [] doubleQuotesHelp
        |. spaces


singleQuotesHelp : List String -> P.Parser (P.Step (List String) String)
singleQuotesHelp revChunks =
    P.oneOf
        [ P.succeed (P.Loop ("'" :: revChunks))
            |. P.symbol "''"
        , P.symbol "'"
            |> P.map (\() -> P.Done (String.concat (List.reverse revChunks)))
        , P.succeed (\e -> P.Loop (e :: revChunks))
            |= P.getChompedString
                (P.succeed ()
                    |. P.chompIf (\c -> c /= '\'' && c /= '\n')
                    |. P.chompWhile (\c -> c /= '\'' && c /= '\n')
                )
        , P.succeed (P.Loop ("\n" :: mapFirst String.trimRight revChunks))
            |. P.symbol "\n\n"
            |. P.chompWhile (\c -> c == ' ')
        , P.succeed (P.Loop (" " :: revChunks))
            |. P.symbol "\n"
            |. P.chompWhile (\c -> c == ' ')
        ]


mapFirst : (a -> a) -> List a -> List a
mapFirst f ls =
    case ls of
        [] ->
            ls

        h :: t ->
            f h :: t


doubleQuotesHelp : List String -> P.Parser (P.Step (List String) String)
doubleQuotesHelp revChunks =
    P.oneOf
        [ P.symbol "\""
            |> P.map (\() -> P.Done (String.concat (List.reverse revChunks)))
        , P.succeed (\e -> P.Loop (String.fromChar e :: revChunks))
            |. P.symbol "\\"
            |= escapeParser
        , P.succeed (\e -> P.Loop (e :: revChunks))
            |= P.getChompedString
                (P.succeed ()
                    |. P.chompIf (\c -> c /= '\\' && c /= '"' && c /= '\n')
                    |. P.chompWhile (\c -> c /= '\\' && c /= '"' && c /= '\n')
                )
        , P.succeed (P.Loop ("\n" :: revChunks))
            |. P.symbol "\n\n"
            |. P.chompWhile (\c -> c == ' ')
        , P.succeed (P.Loop (" " :: revChunks))
            |. P.symbol "\n"
            |. P.chompWhile (\c -> c == ' ')
        ]


escapeParser : P.Parser Char
escapeParser =
    P.oneOf
        [ P.succeed '"' |. P.token "\""
        , P.succeed '\\' |. P.token "\\"
        , P.succeed '/' |. P.token "/"
        , P.succeed '\u{0008}' |. P.token "b"
        , P.succeed '\u{000C}' |. P.token "f"
        , P.succeed '\n' |. P.token "n"
        , P.succeed '\u{000D}' |. P.token "r"
        , P.succeed '\t' |. P.token "t"
        , P.succeed hexChar
            |. P.token "u"
            |= P.getChompedString unicodeHexCode
        ]


{-| Parser for a Unicode hexadecimal code.

E.g. "AbCd" or "1234" or "000D".

It will match exactly 4 hex digits, case-insensitive.

-}
unicodeHexCode : P.Parser ()
unicodeHexCode =
    P.succeed ()
        |. P.chompIf Char.isHexDigit
        |. P.chompIf Char.isHexDigit
        |. P.chompIf Char.isHexDigit
        |. P.chompIf Char.isHexDigit


{-| Converts an hex string into the corresponding char.
-}
hexChar : String -> Char
hexChar s =
    Char.fromCode (String.foldl hexAcc 0 s)


hexAcc : Char -> Int -> Int
hexAcc char total =
    -- https://github.com/allenap/elm-json-decode-broken/blob/3.0.2/src/Json/Decode/Broken.elm
    let
        code : Int
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= 0x39 then
        16 * total + (code - 0x30)

    else if 0x41 <= code && code <= 0x46 then
        16 * total + (10 + code - 0x41)

    else
        16 * total + (10 + code - 0x61)


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
