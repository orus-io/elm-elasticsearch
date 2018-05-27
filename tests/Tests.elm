module Tests exposing (..)

import Json.Encode
import Test exposing (..)
import Expect
import ElasticSearch as ES


testQuery : String -> ES.Query -> Expect.Expectation
testQuery expects query =
    ES.encodeQuery query
        |> Json.Encode.encode 0
        |> Expect.equal expects


testSearchRequest : String -> ES.SearchRequest -> Expect.Expectation
testSearchRequest expects request =
    ES.encodeSearchRequest request
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
                    testQuery
                        "{\"bool\":{\"must\":{\"type\":{\"value\":\"Page\"}}}}"
                        (ES.bool
                            [ ES.must
                                [ ES.type_ "Page"
                                ]
                            ]
                        )
            ]
        , describe "boost"
            [ test "on a term" <|
                \() ->
                    testQuery
                        "{\"term\":{\"a_field\":{\"value\":\"a value\",\"boost\":1.2}}}"
                        (ES.string "a value"
                            |> ES.term "a_field"
                            |> ES.boost 1.2
                        )
            ]
        , describe "request"
            [ test "simple with sort" <|
                \() ->
                    testSearchRequest
                        "{\"query\":{\"term\":{\"a_field\":\"a value\"}},\"sort\":[{\"a_field\":\"desc\"}]}"
                        (ES.searchRequest
                            [ ES.sortBy "a_field" ES.desc ]
                            (ES.term "a_field" <|
                                ES.string "a value"
                            )
                        )
            ]
        ]
