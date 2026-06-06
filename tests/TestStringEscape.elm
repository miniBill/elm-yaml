module TestStringEscape exposing (fuzzed, suite)

import Ansi.Color
import Expect
import Fuzz
import Json.Encode
import Parser exposing (DeadEnd)
import Parser.Error
import Test exposing (Test)
import TestParser
import Yaml.Parser
import Yaml.Parser.Ast as Ast


suite : Test
suite =
    [ ""
    , "foo"
    , "\\d"
    , "\""
    , "'"
    ]
        |> List.map
            (\value ->
                Test.test value <|
                    \_ ->
                        checkEncodedStringRoundtrips value
            )
        |> Test.describe "The string parser"


fuzzed : Test
fuzzed =
    Test.fuzz Fuzz.string
        "Fuzzing string escaping/parsing"
        checkEncodedStringRoundtrips


checkEncodedStringRoundtrips : String -> Expect.Expectation
checkEncodedStringRoundtrips input =
    let
        encoded : String
        encoded =
            input
                |> Json.Encode.string
                |> Json.Encode.encode 0
    in
    case Yaml.Parser.parse encoded of
        Err e ->
            Expect.fail (errorToString encoded e)

        Ok p ->
            p
                |> Expect.equal (Ast.String_ input)


errorToString : String -> List DeadEnd -> String
errorToString src deadEnds =
    Parser.Error.renderError
        { text = identity
        , formatContext = Ansi.Color.fontColor Ansi.Color.cyan
        , formatCaret = Ansi.Color.fontColor Ansi.Color.red
        , newline = "\n"
        , linesOfExtraContext = 3
        }
        Parser.Error.forParser
        -- or Parser.Error.forParserAdvanced
        src
        deadEnds
        |> String.concat
