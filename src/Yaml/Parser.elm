module Yaml.Parser exposing (Value, parse, parser)

import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=))
import Set exposing (Set)
import Yaml.Parser.Ast as Ast
import Yaml.Parser.Document
import Yaml.Parser.String
import Yaml.Parser.Util as U


{-| -}
type alias Value =
    Ast.Value



-- PARSER


{-| -}
parse : String -> Result (List P.DeadEnd) Ast.Value
parse input =
    P.run parser input
        |> Result.map deref


{-| -}
parser : P.Parser Ast.Value
parser =
    P.succeed identity
        |. Yaml.Parser.Document.begins
        |. U.whitespace
        |= value
        |. Yaml.Parser.Document.ends



-- YAML / VALUE


value : P.Parser Ast.Value
value =
    P.oneOf
        [ Yaml.Parser.String.exceptions
        , recordInline
        , listInline
        , P.andThen list P.getCol
        , P.andThen (recordOrString 0) P.getCol
        ]



-- LIST / TOP LEVEL


list : Int -> P.Parser Ast.Value
list indent =
    let
        confirmed : Ast.Value -> P.Parser Ast.Value
        confirmed value_ =
            P.succeed Ast.List_
                |= P.loop [ value_ ] (listStep indent)
    in
    listElement indent
        |> P.andThen confirmed


listStep : Int -> List Ast.Value -> P.Parser (P.Step (List Ast.Value) (List Ast.Value))
listStep indent values =
    let
        finish : P.Step state (List Ast.Value)
        finish =
            P.Done (List.reverse values)

        next : Ast.Value -> P.Step (List Ast.Value) a
        next value_ =
            P.Loop (value_ :: values)
    in
    U.indented indent
        { smaller =
            P.succeed finish
        , exactly =
            P.oneOf
                [ P.succeed next
                    |= listElement indent
                , P.succeed finish -- for lists on the same indentation level as the parent record
                ]
        , larger =
            \_ ->
                P.problem "I was looking for the next element but didn't find one."
        , ending =
            P.succeed finish
        }


listElement : Int -> P.Parser Ast.Value
listElement indent =
    P.succeed identity
        |. listElementBegin
        |= listElementValue indent


listElementBegin : P.Parser ()
listElementBegin =
    P.oneOf
        [ P.symbol "- "
        , P.symbol "-\n"
        ]


listElementValue : Int -> P.Parser Ast.Value
listElementValue indent =
    let
        elVal : Int -> P.Parser Ast.Value
        elVal indent_ =
            P.oneOf
                [ listInline
                , recordInline
                , list indent_
                , recordOrString indent indent_
                ]
    in
    U.indented indent
        { smaller =
            P.succeed Ast.Null_
        , exactly =
            P.succeed Ast.Null_
        , larger =
            \indent_ ->
                P.oneOf
                    [ anchor elVal
                    , reference
                    , elVal indent_
                    ]
        , ending =
            P.succeed Ast.Null_
        }



-- LIST / INLINE


{-| -}
listInline : P.Parser Ast.Value
listInline =
    P.succeed Ast.List_
        |. P.symbol "["
        |. U.whitespace
        |= listInlineStepOne


listInlineStepOne : P.Parser (List Ast.Value)
listInlineStepOne =
    P.oneOf
        [ P.succeed []
            |. P.symbol "]"
        , P.succeed identity
            |= P.loop [] listInlineStep
        ]


listInlineStep : List Ast.Value -> P.Parser (P.Step (List Ast.Value) (List Ast.Value))
listInlineStep elements =
    P.succeed identity
        |. U.whitespace
        |= listInlineValue
        |. U.whitespace
        |> P.andThen (listInlineNext elements)


listInlineValue : P.Parser Ast.Value
listInlineValue =
    let
        inlineVal : P.Parser Ast.Value
        inlineVal =
            P.oneOf
                [ listInline
                , recordInline
                , quotedString 0
                , listInlineString
                ]
    in
    P.oneOf
        [ anchor (always inlineVal)
        , reference
        , inlineVal
        ]


listInlineString : P.Parser Ast.Value
listInlineString =
    P.chompWhile (\c -> c /= ',' && c /= ']')
        |> P.getChompedString
        |> P.map (\l -> Ast.fromString (String.replace "\\" "\\\\" l))


listInlineNext : List Ast.Value -> Ast.Value -> P.Parser (P.Step (List Ast.Value) (List Ast.Value))
listInlineNext elements element =
    P.oneOf
        [ P.succeed identity
            |. P.symbol ","
            |. U.whitespace
            |= P.oneOf
                [ P.succeed (listInlineOnDone elements element)
                    |. P.symbol "]"
                , P.succeed (listInlineOnMore elements element)
                ]
        , P.succeed (listInlineOnDone elements element)
            |. P.symbol "]"
        ]


listInlineOnMore : List Ast.Value -> Ast.Value -> P.Step (List Ast.Value) (List Ast.Value)
listInlineOnMore elements element =
    element
        :: elements
        |> P.Loop


listInlineOnDone : List Ast.Value -> Ast.Value -> P.Step (List Ast.Value) (List Ast.Value)
listInlineOnDone elements element =
    element
        :: elements
        |> List.reverse
        |> P.Done



-- RECORD / TOPLEVEL


recordOrString : Int -> Int -> P.Parser Ast.Value
recordOrString indent indent_ =
    let
        withString : String -> P.Parser Ast.Value
        withString string =
            P.oneOf
                [ P.succeed (Ast.fromString string)
                    |. P.end
                , recordProperty indent_ string
                , P.succeed (addRemaining string)
                    |= (if indent == 0 then
                            U.remaining

                        else
                            U.multiline indent
                       )
                ]

        addRemaining : String -> String -> Ast.Value
        addRemaining string remaining =
            Ast.fromString <| U.postProcessString (removeComment string ++ remaining)

        removeComment : String -> String
        removeComment string =
            string
                |> String.split "#"
                |> List.head
                |> Maybe.withDefault ""
    in
    P.oneOf
        [ quotedString indent_
        , P.succeed identity
            |. P.chompIf (\c -> c /= ':' && c /= '\n')
            |. P.chompWhile (\c -> c /= ':' && c /= '\n')
            |> P.getChompedString
            |> P.andThen withString
        , P.succeed identity
            |. P.chompWhile (\c -> c == ':')
            |> P.getChompedString
            |> P.andThen withString
        ]


quotedString : Int -> P.Parser Ast.Value
quotedString indent =
    let
        withQuote : String -> P.Parser Ast.Value
        withQuote quote =
            P.oneOf
                [ recordProperty indent quote
                , P.succeed (Ast.String_ quote)
                ]
    in
    P.succeed identity
        |= P.oneOf
            [ U.singleQuotes
            , U.doubleQuotes
            ]
        |. U.spaces
        |> P.andThen withQuote


recordProperty : Int -> String -> P.Parser Ast.Value
recordProperty indent name =
    P.succeed (record indent name)
        |. P.oneOf
            [ P.token ": "
            , P.token ":\n"
            , P.backtrackable (P.token ":") |. P.end
            ]
        |> P.andThen identity


record : Int -> String -> P.Parser Ast.Value
record indent property =
    let
        confirmed : Ast.Value -> P.Parser Ast.Value
        confirmed value_ =
            P.succeed identity
                |= P.loop [ ( property, value_ ) ] (recordStep indent)
                |> P.andThen duplicatedPropertyKeysCheck
                |> P.map (\l -> Ast.Record_ (Dict.fromList l))
    in
    recordElementValue indent
        |> P.andThen confirmed


recordStep : Int -> List Ast.Property -> P.Parser (P.Step (List Ast.Property) (List Ast.Property))
recordStep indent values =
    let
        finish : P.Step state (List Ast.Property)
        finish =
            P.Done (List.reverse values)

        next : Ast.Property -> P.Step (List Ast.Property) a
        next value_ =
            P.Loop (value_ :: values)
    in
    U.indented indent
        { smaller =
            P.succeed finish
        , exactly =
            P.succeed next
                |= recordElement indent
        , larger =
            \_ ->
                P.problem "I was looking for the next property but didn't find one."
        , ending =
            P.succeed finish
        }


recordElement : Int -> P.Parser Ast.Property
recordElement indent =
    let
        property : P.Parser String
        property =
            P.oneOf
                [ U.singleQuotes
                , U.doubleQuotes
                , P.chompWhile (\c -> c /= ':' && c /= '\n')
                    |> P.getChompedString
                ]
    in
    P.succeed Tuple.pair
        |= property
        |. U.spaces
        |. P.symbol ":"
        |= recordElementValue indent


recordElementValue : Int -> P.Parser Ast.Value
recordElementValue indent =
    let
        elVal : Int -> P.Parser Ast.Value
        elVal indent_ =
            P.oneOf
                [ listInline
                , recordInline
                , list indent_
                , recordOrString indent indent_
                ]
    in
    U.indented indent
        { smaller =
            P.succeed Ast.Null_
        , exactly =
            P.oneOf
                [ list indent
                , P.succeed Ast.Null_
                ]
        , larger =
            \indent_ ->
                P.oneOf
                    [ anchor elVal
                    , reference
                    , elVal indent_
                    ]
        , ending =
            P.succeed Ast.Null_
        }



-- RECORD / INLINE


{-| -}
recordInline : P.Parser Ast.Value
recordInline =
    P.succeed (\l -> Ast.Record_ (Dict.fromList l))
        |. P.symbol "{"
        |. U.whitespace
        |= recordInlineStepOne


recordInlineStepOne : P.Parser (List Ast.Property)
recordInlineStepOne =
    P.oneOf
        [ P.succeed []
            |. P.symbol "}"
        , P.succeed identity
            |= P.loop [] recordInlineStep
        ]
        |> P.andThen duplicatedPropertyKeysCheck


{-| YAML mappings require key uniqueness (<https://yaml.org/spec/1.2.2/#node-comparison>)
-}
duplicatedPropertyKeysCheck : List Ast.Property -> P.Parser (List Ast.Property)
duplicatedPropertyKeysCheck properties =
    let
        duplicates : List String
        duplicates =
            duplicatedPropertyKeys properties
    in
    if List.isEmpty duplicates then
        P.succeed properties

    else
        P.problem ("Non-unique keys in record: " ++ String.join ", " duplicates)


duplicatedPropertyKeys : List Ast.Property -> List String
duplicatedPropertyKeys properties =
    let
        keys : List String
        keys =
            List.map Tuple.first properties

        duplicated : ( Set String, Set String )
        duplicated =
            List.foldl
                (\x ( obs, dup ) ->
                    if Set.member x obs then
                        -- if this item has already been observed, add to duplicates
                        ( obs, Set.insert x dup )

                    else
                        -- otherwise add to observed items
                        ( Set.insert x obs, dup )
                )
                ( Set.empty, Set.empty )
                keys
    in
    Tuple.second duplicated
        |> Set.toList


recordInlineStep : List Ast.Property -> P.Parser (P.Step (List Ast.Property) (List Ast.Property))
recordInlineStep elements =
    P.succeed identity
        |. U.whitespace
        |= recordInlineValue
        |. U.whitespace
        |> P.andThen (recordInlineNext elements)


recordInlineValue : P.Parser Ast.Property
recordInlineValue =
    P.succeed Tuple.pair
        |= recordInlinePropertyName
        |. U.whitespace
        |= recordInlinePropertyValue


recordInlinePropertyName : P.Parser String
recordInlinePropertyName =
    P.succeed identity
        |= P.oneOf
            [ U.singleQuotes
            , U.doubleQuotes
            , recordInlinePropertyNameString
            ]
        |. P.chompWhile (\c -> c == ' ')
        |. P.oneOf
            [ P.symbol ":"
            , P.problem "I was parsing an inline record, when I ran into an invalid property. It is missing the \":\"!"
            ]
        |. P.oneOf
            [ P.symbol "\n"
            , P.symbol " "
            , P.problem "I was parsing an inline record, but missing a space or a new line between the \":\" and the value!"
            ]


recordInlinePropertyNameString : P.Parser String
recordInlinePropertyNameString =
    -- TODO allow numeric name
    P.succeed ()
        |. P.chompWhile (\c -> c /= ':' && c /= ',' && c /= '}')
        |. U.whitespace
        |> P.getChompedString
        |> P.map String.trim


recordInlinePropertyValue : P.Parser Ast.Value
recordInlinePropertyValue =
    let
        propVal : P.Parser Ast.Value
        propVal =
            P.oneOf
                [ listInline
                , recordInline
                , recordInlineString
                ]
    in
    P.oneOf
        [ anchor (always propVal)
        , reference
        , propVal
        ]


recordInlineString : P.Parser Ast.Value
recordInlineString =
    P.succeed ()
        |. P.chompWhile (\c -> c /= ',' && c /= '}')
        |> P.getChompedString
        |> P.map Ast.fromString


recordInlineNext : List Ast.Property -> Ast.Property -> P.Parser (P.Step (List Ast.Property) (List Ast.Property))
recordInlineNext elements element =
    P.oneOf
        [ P.succeed (recordInlineOnMore elements element)
            |. P.symbol ","
        , P.succeed (recordInlineOnDone elements element)
            |. P.symbol "}"
        ]


recordInlineOnMore : List Ast.Property -> Ast.Property -> P.Step (List Ast.Property) (List Ast.Property)
recordInlineOnMore elements element =
    element
        :: elements
        |> P.Loop


recordInlineOnDone : List Ast.Property -> Ast.Property -> P.Step (List Ast.Property) (List Ast.Property)
recordInlineOnDone elements element =
    element
        :: elements
        |> List.reverse
        |> P.Done



-- ANCHOR and REFERENCE


anchor : (Int -> P.Parser Ast.Value) -> P.Parser Ast.Value
anchor valParser =
    P.succeed Ast.Anchor_
        |. P.symbol "&"
        |= refName
        |. U.whitespace
        |= (P.getCol |> P.andThen valParser)


reference : P.Parser Ast.Value
reference =
    P.succeed Ast.Alias_
        |. P.symbol "*"
        |= refName


refName : P.Parser String
refName =
    P.succeed identity
        |= (P.getChompedString <|
                P.chompWhile
                    (\c ->
                        not
                            (List.member c
                                [ '\u{000D}'
                                , '\n'
                                , ' '
                                , '\t'
                                , ','
                                , '['
                                , ']'
                                , '{'
                                , '}'
                                ]
                            )
                    )
           )


deref : Ast.Value -> Ast.Value
deref ast =
    let
        anchorMap : Dict String Ast.Value
        anchorMap =
            Ast.fold
                (\node d ->
                    case node of
                        Ast.Anchor_ name v ->
                            Dict.insert name v d

                        _ ->
                            d
                )
                ast
                Dict.empty

        replaceAnchors : Ast.Value -> Ast.Value
        replaceAnchors v =
            case v of
                Ast.Alias_ name ->
                    Dict.get name anchorMap |> Maybe.withDefault v

                Ast.Anchor_ _ node ->
                    node

                _ ->
                    v
    in
    Ast.map replaceAnchors ast
