module Yaml.Parser.String exposing (exceptions)

import Parser as P exposing ((|.), (|=))
import Yaml.Parser.Ast as Ast
import Yaml.Parser.Util as U


{-| -}
exceptions : P.Parser Ast.Value
exceptions =
    P.oneOf
        [ P.succeed Ast.Null_
            -- TODO
            |. P.end
        , P.succeed (\s -> Ast.String_ ("---" ++ s))
            |. U.threeDashes
            |= U.remaining
        , P.succeed Ast.Null_
            |. U.threeDots
            |. U.remaining
        ]
