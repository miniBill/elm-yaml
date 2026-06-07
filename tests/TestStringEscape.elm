module TestStringEscape exposing (fuzzed, specificStringParseTest, specificStringsRoundtripTest)

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


testCases : List ( String, String )
testCases =
    [ ( "\"What is this feature for?\"", "What is this feature for?" )
    , ( "'{\"food\": \"bacon\"}'", "{\"food\": \"bacon\"}" )
    , ( "'/bin/bash -c ''echo \"$VALUE\"'''", "/bin/bash -c 'echo \"$VALUE\"'" )
    , ( "Multiline \\nstring", "Multiline\nstring" )
    , ( "|\n  literal\n  block\n", "literal\nblock\n" )
    , ( ">\n  first\n  second\n", "first second\n" )
    , ( "|-\n  literal\n  block\n", "literal\nblock" )
    , ( ">-\n  first\n  second\n", "first second" )
    , ( "\"\"", "" )
    , ( "\"foo\"", "foo" )
    , ( "\"\\\\d\"", "\\d" )
    , ( "\"\\\"\"", "\"" )
    , ( "\"'\"", "'" )
    ]


specificStringParseTest : Test
specificStringParseTest =
    testCases
        |> List.map
            (\( input, output ) ->
                Test.test ("Parsing check " ++ input) <|
                    \_ ->
                        checkIsParsedAs output input
            )
        |> Test.describe "Strings get correctly parsed"


specificStringsRoundtripTest : Test
specificStringsRoundtripTest =
    testCases
        |> List.map
            (\( _, output ) ->
                Test.test ("Roundtrip " ++ escape output) <|
                    \_ ->
                        checkEncodedStringRoundtrips output
            )
        |> Test.describe "Specific strings roundtrip"


fuzzed : Test
fuzzed =
    Test.fuzz Fuzz.string
        "Fuzzing string escaping/parsing"
        checkEncodedStringRoundtrips


checkEncodedStringRoundtrips : String -> Expect.Expectation
checkEncodedStringRoundtrips input =
    input
        |> escape
        |> checkIsParsedAs input


escape : String -> String
escape input =
    input
        |> Json.Encode.string
        |> Json.Encode.encode 0


checkIsParsedAs : String -> String -> Expect.Expectation
checkIsParsedAs expected input =
    case Yaml.Parser.parse input of
        Err e ->
            Expect.fail (errorToString input e)

        Ok p ->
            p
                |> Expect.equal (Ast.String_ expected)


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
