module ElasticSearch
    exposing
        ( Query
        , SearchRequest
        , searchRequest
          -- value types
        , Value
        , date
        , float
        , int
        , string
          -- cmp operators
        , gt
        , gtDate
        , gte
        , gteDate
        , gtn
        , lt
        , ltDate
        , lte
        , lteDate
        , ltn
          -- leaf queries
        , range
        , term
        , type_
          -- compound queries
        , bool
        , must
        , mustNot
        , should
        , filter
          -- params
        , boost
          -- sort
        , asc
        , desc
        , sortBy
          -- encode
        , encodeQuery
        , encodeSearchRequest
        )

{-| Easily generate typesafe elasticsearch queries

@docs Query, SearchRequest, searchRequest


# Value types

@docs Value, date, float, int, string


# Comparison operators

These operators are to be used with the range query

@docs gt, gte, gtn, lt, lte, ltn, gtDate, gteDate, ltDate, lteDate


# Queries


## Leaf queries

@docs range, term, type_


## Compound queries

@docs bool, must, mustNot, should, filter


## Common params

@docs boost


# Sort

@docs sortBy, asc, desc


# Encode

@docs encodeQuery, encodeSearchRequest

-}

import Json.Encode as JE
import Time.Date as Date exposing (Date)


type alias AttrName =
    String


encodeObject : List ( String, Maybe JE.Value ) -> JE.Value
encodeObject list =
    list
        |> List.filterMap
            (\( name, value ) ->
                case value of
                    Just value ->
                        Just ( name, value )

                    Nothing ->
                        Nothing
            )
        |> JE.object


encodeSubObject : List String -> List ( String, Maybe JE.Value ) -> JE.Value
encodeSubObject path attrs =
    List.foldr (\name obj -> JE.object [ ( name, obj ) ])
        (encodeObject attrs)
        path


{-| An opaque type for wrapping values
-}
type Value
    = IntValue Int
    | StringValue String
    | FloatValue Float
    | DateValue Date


{-| Build a integer value
-}
int : Int -> Value
int =
    IntValue


{-| Build a string value
-}
string : String -> Value
string =
    StringValue


{-| Build a float value
-}
float : Float -> Value
float =
    FloatValue


{-| Build a date value
-}
date : Date -> Value
date =
    DateValue


encodeValue : Value -> JE.Value
encodeValue value =
    case value of
        IntValue value ->
            JE.int value

        StringValue value ->
            JE.string value

        FloatValue value ->
            JE.float value

        DateValue value ->
            Date.toISO8601 value
                |> JE.string


type alias TermQuery =
    { name : AttrName
    , value : Value
    , boost : Maybe Float
    , queryName : Maybe String
    }


encodeTermQuery : TermQuery -> JE.Value
encodeTermQuery q =
    JE.object
        [ ( "term"
          , JE.object
                [ ( q.name
                  , case ( q.boost, q.queryName ) of
                        ( Nothing, Nothing ) ->
                            encodeValue q.value

                        ( boost, qname ) ->
                            encodeObject
                                [ ( "value", Just <| encodeValue q.value )
                                , encodeBoost boost
                                , encodeQueryName qname
                                ]
                  )
                ]
          )
        ]


type Relation
    = Within
    | Contains
    | Intersects
    | Disjoint


relationToString : Relation -> String
relationToString rel =
    case rel of
        Within ->
            "within"

        Contains ->
            "contains"

        Intersects ->
            "intersects"

        Disjoint ->
            "disjoint"


encodeRelation : Maybe Relation -> ( String, Maybe JE.Value )
encodeRelation rel =
    ( "relation"
    , Maybe.map (relationToString >> JE.string) rel
    )


type LowerBound
    = NoLowerBound
    | Gte Value
    | Gt Value


type UpperBound
    = NoUpperBound
    | Lte Value
    | Lt Value


{-| Greater Than Nothing

Use for a range with no lower bound

-}
gtn : LowerBound
gtn =
    NoLowerBound


{-| Greater Than or Equal
-}
gte : Value -> LowerBound
gte =
    Gte


{-| Greater Than
-}
gt : Value -> LowerBound
gt =
    Gt


{-| Lesser Than Nothing

Use for a range with no upper bound

-}
ltn : UpperBound
ltn =
    NoUpperBound


{-| Lesser Than or Equal
-}
lte : Value -> UpperBound
lte =
    Lte


{-| Lesser Than
-}
lt : Value -> UpperBound
lt =
    Lt


{-| Greater Than or Equal to a date
-}
gteDate : Date -> LowerBound
gteDate =
    date >> gte


{-| Greater Than a date
-}
gtDate : Date -> LowerBound
gtDate =
    date >> gt


{-| Lesser Than or Equal to a date
-}
lteDate : Date -> UpperBound
lteDate =
    date >> lte


{-| Lesser Than a date
-}
ltDate : Date -> UpperBound
ltDate =
    date >> lt


encodeUpperBound : UpperBound -> ( String, Maybe JE.Value )
encodeUpperBound bound =
    case bound of
        NoUpperBound ->
            ( "", Nothing )

        Lte value ->
            ( "lte", Just <| encodeValue value )

        Lt value ->
            ( "lt", Just <| encodeValue value )


encodeLowerBound : LowerBound -> ( String, Maybe JE.Value )
encodeLowerBound bound =
    case bound of
        NoLowerBound ->
            ( "", Nothing )

        Gte value ->
            ( "gte", Just <| encodeValue value )

        Gt value ->
            ( "gt", Just <| encodeValue value )


encodeBoost : Maybe Float -> ( String, Maybe JE.Value )
encodeBoost boost =
    ( "boost", Maybe.map JE.float boost )


encodeQueryName : Maybe String -> ( String, Maybe JE.Value )
encodeQueryName name =
    ( "_name", Maybe.map JE.string name )


encodeMinimumShoudMatch : Maybe Int -> ( String, Maybe JE.Value )
encodeMinimumShoudMatch value =
    ( "minimum_should_match", Maybe.map JE.int value )


type alias RangeQuery =
    { name : AttrName
    , from : LowerBound
    , to : UpperBound
    , boost : Maybe Float
    , queryName : Maybe String
    , relation : Maybe Relation

    -- , timezone : TimeZone
    }


encodeRangeQuery : RangeQuery -> JE.Value
encodeRangeQuery q =
    encodeSubObject [ "range", q.name ]
        [ encodeLowerBound q.from
        , encodeUpperBound q.to
        , encodeBoost q.boost
        , encodeRelation q.relation
        ]


type alias BoolQuery =
    { must : List Query
    , mustNot : List Query
    , filter : List Query
    , should : List Query
    , boost : Maybe Float
    , minimumShouldMatch : Maybe Int
    , queryName : Maybe String
    }


emptyBoolQuery : BoolQuery
emptyBoolQuery =
    { must = []
    , mustNot = []
    , filter = []
    , should = []
    , boost = Nothing
    , minimumShouldMatch = Nothing
    , queryName = Nothing
    }


encodeBoolQuery : BoolQuery -> JE.Value
encodeBoolQuery q =
    encodeSubObject [ "bool" ]
        [ ( "must", encodeQueryList q.must )
        , ( "must_not", encodeQueryList q.mustNot )
        , ( "filter", encodeQueryList q.filter )
        , ( "should", encodeQueryList q.should )
        , encodeBoost q.boost
        , encodeQueryName q.queryName
        ]


type TypeQuery
    = TypeQuery String


encodeTypeQuery : TypeQuery -> JE.Value
encodeTypeQuery q =
    case q of
        TypeQuery tname ->
            encodeSubObject [ "type" ]
                [ ( "value", Just <| JE.string tname ) ]


{-| A query
-}
type Query
    = Term TermQuery
    | Range RangeQuery
    | Type TypeQuery
    | Bool BoolQuery


{-| Encode a query to a Json.Encode.Value
-}
encodeQuery : Query -> JE.Value
encodeQuery q =
    case q of
        Term q ->
            encodeTermQuery q

        Range q ->
            encodeRangeQuery q

        Type q ->
            encodeTypeQuery q

        Bool q ->
            encodeBoolQuery q


encodeQueryList : List Query -> Maybe JE.Value
encodeQueryList qlist =
    case qlist of
        [] ->
            Nothing

        [ single ] ->
            Just <| encodeQuery single

        qlist ->
            qlist
                |> List.map encodeQuery
                |> JE.list
                |> Just


{-| A `term` query

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-term-query.html>

-}
term : AttrName -> Value -> Query
term name value =
    Term <| TermQuery name value Nothing Nothing


{-| A `range` query

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-range-query.html>

-}
range : AttrName -> LowerBound -> UpperBound -> Query
range name from to =
    Range <| RangeQuery name from to Nothing Nothing Nothing


{-| A `type` query

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-type-query.html>

-}
type_ : String -> Query
type_ typename =
    Type <| TypeQuery typename


maybeCombine : Maybe a -> Maybe a -> Maybe a
maybeCombine a b =
    case a of
        Just a ->
            Just a

        _ ->
            b


type BoolClause
    = Must (List Query)
    | Filter (List Query)
    | Should (List Query) (Maybe Int)
    | MustNot (List Query)


{-| A `bool` query
-}
bool : List BoolClause -> Query
bool =
    List.foldl
        (\clause q ->
            case clause of
                Must list ->
                    { q
                        | must = q.must ++ list
                    }

                Filter list ->
                    { q
                        | filter = q.filter ++ list
                    }

                Should list minShouldMatch ->
                    { q
                        | should = q.should ++ list
                        , minimumShouldMatch = minShouldMatch
                    }

                MustNot list ->
                    { q
                        | mustNot = q.mustNot ++ list
                    }
        )
        emptyBoolQuery
        >> Bool


{-| A `must` clause for the `bool` constructor
-}
must : List Query -> BoolClause
must =
    Must


{-| A `mustNot` clause for the `bool` constructor
-}
mustNot : List Query -> BoolClause
mustNot =
    MustNot


{-| A `should` clause for the `bool` constructor
-}
should : Maybe Int -> List Query -> BoolClause
should minShouldMatch list =
    Should list minShouldMatch


{-| A `filter` clause for the `bool` constructor
-}
filter : List Query -> BoolClause
filter =
    Filter


{-| Set the query boost

For some queries, like `term`, it is a no-op

-}
boost : Float -> Query -> Query
boost b q =
    case q of
        Term q ->
            Term { q | boost = Just b }

        Range q ->
            Range { q | boost = Just b }

        Type q ->
            Type q

        Bool q ->
            Bool { q | boost = Just b }


type SortOrder
    = Asc
    | Desc


sortOrderToString : SortOrder -> String
sortOrderToString order =
    case order of
        Asc ->
            "asc"

        Desc ->
            "desc"


encodeSortOrder : SortOrder -> JE.Value
encodeSortOrder =
    sortOrderToString
        >> JE.string


{-| Sort in ascending order
-}
asc : SortOrder
asc =
    Asc


{-| Sort in descending order
-}
desc : SortOrder
desc =
    Desc


type SortMode
    = SortModeMin
    | SortModeMax
    | SortModeSum
    | SortModeAvg
    | SortModeMedian


sortModeToString : SortMode -> String
sortModeToString mode =
    case mode of
        SortModeMin ->
            "min"

        SortModeMax ->
            "max"

        SortModeSum ->
            "sum"

        SortModeAvg ->
            "avg"

        SortModeMedian ->
            "median"


encodeSortMode : SortMode -> JE.Value
encodeSortMode =
    sortModeToString
        >> JE.string


sortModeMin : SortMode
sortModeMin =
    SortModeMin


sortModeMax : SortMode
sortModeMax =
    SortModeMax


sortModeSum : SortMode
sortModeSum =
    SortModeSum


sortModeAvg : SortMode
sortModeAvg =
    SortModeAvg


sortModeMedian : SortMode
sortModeMedian =
    SortModeMedian


type Sort
    = SortByField String SortOrder
    | SortByArrayField String SortOrder SortMode


encodeSort : Sort -> JE.Value
encodeSort sort =
    case sort of
        SortByField name order ->
            JE.object
                [ ( name, encodeSortOrder order )
                ]

        SortByArrayField name order mode ->
            JE.object
                [ ( name
                  , JE.object
                        [ ( "order", encodeSortOrder order )
                        , ( "mode", encodeSortMode mode )
                        ]
                  )
                ]


{-| Sort on a field
-}
sortBy : String -> SortOrder -> Sort
sortBy =
    SortByField


sortByArray : String -> SortOrder -> SortMode -> Sort
sortByArray =
    SortByArrayField


{-| A search request
-}
type alias SearchRequest =
    { query : Query
    , sort : List Sort
    }


{-| search Request
-}
searchRequest : List Sort -> Query -> SearchRequest
searchRequest sort query =
    SearchRequest query sort


{-| encode a SearchRequest
-}
encodeSearchRequest : SearchRequest -> JE.Value
encodeSearchRequest request =
    JE.object <|
        [ ( "query", encodeQuery request.query ) ]
            ++ case request.sort of
                [] ->
                    []

                list ->
                    [ ( "sort"
                      , List.map encodeSort list
                            |> JE.list
                      )
                    ]
