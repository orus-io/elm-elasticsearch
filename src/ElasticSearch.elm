module ElasticSearch
    exposing
        ( Query
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
        , must
        , mustNot
        , should
        , filter
        , andMust
          -- encode
        , encode
        )

{-| Easily generate typesafe elasticsearch queries

@docs Query


# Value types

@docs Value, date, float, int, string


# Comparison operators

These operators are to be used with the range query

@docs gt, gte, gtn, lt, lte, ltn, gtDate, gteDate, ltDate, lteDate


# Queries


## Leaf queries

@docs range, term, type_


## Compound queries

@docs must, mustNot, should, filter, andMust


# Encode

@docs encode

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
encode : Query -> JE.Value
encode q =
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
            Just <| encode single

        qlist ->
            qlist
                |> List.map encode
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


{-| A `bool` query with a `must` clause
-}
must : List Query -> Query
must qlist =
    Bool { emptyBoolQuery | must = qlist }


{-| A `bool` query with a `must_not` clause
-}
mustNot : List Query -> Query
mustNot qlist =
    Bool { emptyBoolQuery | mustNot = qlist }


{-| A `bool` query with a `should` clause
-}
should : List Query -> Query
should qlist =
    Bool { emptyBoolQuery | should = qlist }


{-| A `bool` query with a `filter` clause
-}
filter : List Query -> Query
filter qlist =
    Bool { emptyBoolQuery | filter = qlist }


{-| Add a `must` clause to a query
-}
andMust : Query -> Query -> Query
andMust q intoQ =
    case intoQ of
        Bool bq ->
            Bool { bq | must = q :: bq.must }

        intoQ ->
            must [ q, intoQ ]
