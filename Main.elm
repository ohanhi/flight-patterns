module Main exposing (..)

import Curve
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import SubPath
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (on)


type alias Vec2 =
    ( Float, Float )


type Feet
    = Feet Float


type Meters
    = Meters Float


toMeters : Feet -> Meters
toMeters (Feet ft) =
    Meters (ft * 0.3048)


toFeet : Meters -> Feet
toFeet (Meters m) =
    Feet (m / 0.3048)


type alias Model =
    { throwerMaxDist : Meters
    , dist : Meters
    , hst : Float
    , lsf : Float
    , turnsign : Float
    , discSelected : PresetDiscType
    , throwerSelected : PresetThrower
    }


type Msg
    = MaxDistanceChanged Meters
    | HstChanged Float
    | LsfChanged Float
    | DistanceChanged Meters
    | PresetThrowerSelected PresetThrower
    | PresetDiscSelected PresetDiscType


c : Float
c =
    toFloat h / 150


xscale : Float
xscale =
    c * 0.2


yscale : Float
yscale =
    c * 0.3


h : Int
h =
    600


w : Int
w =
    400


model : Model
model =
    { throwerMaxDist = Meters 70
    , dist = Meters 75
    , hst = -38
    , lsf = 24
    , turnsign = 1
    , discSelected = Midrange
    , throwerSelected = Recreational
    }


type PresetThrower
    = Beginner
    | Recreational
    | Advanced
    | Pro


type PresetDiscType
    = Putter
    | Midrange
    | FairwayDriverStable
    | FairwayDriverUnderstable
    | DistanceDriverStable
    | DistanceDriverUnderstable


type alias DiscData =
    { name : String
    , dist : Meters
    , hst : Float
    , lsf : Float
    , speed : Int
    , glide : Int
    , turn : Int
    , fade : Int
    }


pathPoints : Model -> { points : List Vec2, endPosition : Vec2 }
pathPoints { turnsign, dist, hst, lsf, throwerMaxDist } =
    let
        toCoords =
            toPathCoordinates turnsign

        (Meters meters) =
            dist

        turnStartY =
            meters * 0.2

        turnEndY =
            meters * 0.85

        end =
            ( hst + lsf, meters )
    in
    { points =
        [ ( 0, 0 )
        , ( 0, turnStartY )
        , ( hst, turnEndY )
        , end
        ]
            |> List.map toCoords
    , endPosition = toCoords end
    }


toPathCoordinates : Float -> Vec2 -> Vec2
toPathCoordinates turnsign ( deviation, distance ) =
    ( negate turnsign * xscale * deviation + toFloat w / 2
    , toFloat h - (c * distance)
    )


view : Model -> Html Msg
view model =
    let
        { points, endPosition } =
            pathPoints model
    in
    Html.div []
        [ svg
            [ viewBox ("0 0 " ++ toString w ++ " " ++ toString h)
            , width (toString w)
            , height (toString h)
            ]
            [ rect
                [ width (toString w)
                , height (toString h)
                , x "0"
                , y "0"
                , fill "none"
                , stroke "#DDDDDD"
                , strokeWidth "1"
                ]
                []
            , distanceLines { until = 150, diff = 25 }
            , hairline <| "M" ++ toString (w // 2) ++ " 0 V" ++ toString h
            , SubPath.element (Curve.catmullRom 1.0 points)
                [ fill "none"
                , stroke "blue"
                , strokeWidth "2"
                ]
            , dot { color = "blue", position = endPosition }
            ]
        , sliderForDistance model.dist
        , sliderForHst model.hst
        , sliderForLsf model.lsf
        , sliderForMaxDistance model.throwerMaxDist
        , throwerSelectButton model Beginner
        , throwerSelectButton model Recreational
        , throwerSelectButton model Advanced
        , throwerSelectButton model Pro
        , Html.br [] []
        , discSelectButton model Putter
        , discSelectButton model Midrange
        , discSelectButton model FairwayDriverUnderstable
        , discSelectButton model FairwayDriverStable
        , discSelectButton model DistanceDriverUnderstable
        , discSelectButton model DistanceDriverStable
        ]


throwerSelectButton : { a | throwerSelected : PresetThrower } -> PresetThrower -> Html Msg
throwerSelectButton { throwerSelected } presetThrower =
    Html.button
        [ Html.Attributes.disabled (throwerSelected == presetThrower)
        , Html.Events.onClick (PresetThrowerSelected presetThrower)
        ]
        [ Html.text (toString presetThrower) ]


discSelectButton : { a | discSelected : PresetDiscType } -> PresetDiscType -> Html Msg
discSelectButton { discSelected } presetDiscType =
    Html.button
        [ Html.Attributes.disabled (discSelected == presetDiscType)
        , Html.Events.onClick (PresetDiscSelected presetDiscType)
        ]
        [ Html.text (toString presetDiscType) ]


sliderForDistance : Meters -> Html Msg
sliderForDistance (Meters current) =
    Html.div []
        [ Html.text "Disc distance"
        , Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "50"
            , Html.Attributes.max "150"
            , Html.Attributes.step "5"
            , Html.Attributes.value (toString current)
            , onNumberInput (DistanceChanged << Meters)
            ]
            []
        , Html.text (toString (round current) ++ "m")
        ]


sliderForMaxDistance : Meters -> Html Msg
sliderForMaxDistance (Meters current) =
    Html.div []
        [ Html.text "Thrower maximum distance"
        , Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "50"
            , Html.Attributes.max "150"
            , Html.Attributes.step "5"
            , Html.Attributes.value (toString current)
            , onNumberInput (MaxDistanceChanged << Meters)
            ]
            []
        , Html.text (toString (round current) ++ "m")
        ]


sliderForHst : Float -> Html Msg
sliderForHst current =
    Html.div []
        [ Html.text "High speed Turn"
        , Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "-15"
            , Html.Attributes.max "100"
            , Html.Attributes.step "1"
            , Html.Attributes.value (toString (-1 * current))
            , onNumberInput (\n -> HstChanged (-1 * n))
            ]
            []
        , Html.text (toString (round current) ++ "%")
        ]


sliderForLsf : Float -> Html Msg
sliderForLsf current =
    Html.div []
        [ Html.text "Low speed Fade"
        , Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "100"
            , Html.Attributes.step "1"
            , Html.Attributes.value (toString current)
            , onNumberInput LsfChanged
            ]
            []
        , Html.text (toString (round current) ++ "%")
        ]


distanceLines : { until : Int, diff : Int } -> Svg msg
distanceLines { until, diff } =
    List.range 0 (until // diff)
        |> List.map (\i -> distanceLine (i * diff))
        |> g []


distanceLine : Int -> Svg msg
distanceLine dist =
    let
        px =
            round (toFloat h - toFloat dist * c)
    in
    g []
        [ hairline <| "M0 " ++ toString px ++ "H" ++ toString w
        , text_
            [ x "10"
            , y (toString px)
            , fontFamily "Arial"
            , fontSize "10"
            , fill "#AAAAAA"
            ]
            [ text (toString dist) ]
        ]


hairline : String -> Svg msg
hairline pathD =
    Svg.path
        [ stroke "#DDDDDD"
        , strokeWidth "0.5"
        , d pathD
        ]
        []


dot : { color : String, position : Vec2 } -> Svg Msg
dot { color, position } =
    let
        ( x, y ) =
            position
    in
    circle
        [ fill color
        , cx (toString x)
        , cy (toString y)
        , r "3"
        ]
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    pureUpdate msg model ! []


pureUpdate : Msg -> Model -> Model
pureUpdate msg model =
    case msg of
        MaxDistanceChanged throwerMaxDist ->
            { model | throwerMaxDist = throwerMaxDist }

        HstChanged hst ->
            { model | hst = hst }

        LsfChanged lsf ->
            { model | lsf = lsf }

        DistanceChanged dist ->
            { model | dist = dist }

        PresetDiscSelected presetDisc ->
            let
                discData =
                    getDiscData presetDisc
            in
            { model
                | discSelected = presetDisc
                , hst = discData.hst
                , lsf = discData.lsf
                , dist = discData.dist
            }

        PresetThrowerSelected presetThrower ->
            { model
                | throwerSelected = presetThrower
                , throwerMaxDist = getMaxDistance presetThrower
            }


onNumberInput : (Float -> Msg) -> Attribute Msg
onNumberInput tagger =
    Html.Events.onInput
        (\value ->
            case String.toFloat value of
                Ok number ->
                    tagger number

                Err err ->
                    Debug.crash "not a number" err
        )


main : Program Never Model Msg
main =
    Html.program
        { init = model ! []
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- DATA


getMaxDistance : PresetThrower -> Meters
getMaxDistance thrower =
    case thrower of
        Beginner ->
            Meters 50

        Recreational ->
            Meters 70

        Advanced ->
            Meters 120

        Pro ->
            Meters 150


getDiscData : PresetDiscType -> DiscData
getDiscData presetDiscType =
    case presetDiscType of
        Putter ->
            { name = "Discmania P2"
            , dist = toMeters (Feet 248)
            , hst = 0
            , lsf = 20
            , speed = 2
            , glide = 3
            , turn = 0
            , fade = 1
            }

        Midrange ->
            { name = "Discmania MD2"
            , dist = toMeters (Feet 286)
            , hst = -6
            , lsf = 32
            , speed = 4
            , glide = 5
            , turn = 0
            , fade = 2
            }

        FairwayDriverStable ->
            { name = "Discmania FD2"
            , dist = toMeters (Feet 331)
            , hst = 0
            , lsf = 44
            , speed = 7
            , glide = 4
            , turn = 0
            , fade = 2
            }

        FairwayDriverUnderstable ->
            { name = "Discmania FD"
            , dist = toMeters (Feet 362)
            , hst = -19
            , lsf = 28
            , speed = 7
            , glide = 6
            , turn = -1
            , fade = 1
            }

        DistanceDriverStable ->
            { name = "Discmania DD"
            , dist = toMeters (Feet 418)
            , hst = -2
            , lsf = 46
            , speed = 11
            , glide = 5
            , turn = 0
            , fade = 2
            }

        DistanceDriverUnderstable ->
            { name = "Discmania TD"
            , dist = toMeters (Feet 385)
            , hst = -38
            , lsf = 24
            , speed = 10
            , glide = 5
            , turn = -2
            , fade = 1
            }
