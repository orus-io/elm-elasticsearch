module Tests exposing (..)

import Json.Encode
import Test exposing (..)
import Expect
import ElasticSearch as ES


testQuery : String -> ES.Query -> Expect.Expectation
testQuery expects query =
    ES.encode query
        |> Json.Encode.encode 0
        |> Expect.equal expects


all : Test
all =
    describe "ElasticSearch"
        [ test "term should create a `term` query" <|
            \() ->
                testQuery "{\"term\":{\"a_field\":\"a value\"}}"
                    (ES.term "a_field" <| ES.string "a value")
        , describe "bool"
            [ test "bool should create a `bool` query" <|
                \() ->
                    testQuery "{\"bool\":{\"must\":{\"type\":{\"value\":\"Page\"}}}}"
                        (ES.bool
                            [ ES.must
                                [ ES.type_ "Page"
                                ]
                            ]
                        )
            ]
        ]
