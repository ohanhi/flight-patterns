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
import Vector2 exposing (Vec2)


type alias Model =
    { airspeed : Float
    , dist : Float
    , hst : Float
    , lsf : Float
    , turnsign : Float
    }


type Msg
    = AirspeedChanged Float
    | HstChanged Float
    | LsfChanged Float
    | DistanceChanged Float


c : Int
c =
    h // 150


xscale : Float
xscale =
    toFloat c * 0.2


yscale : Float
yscale =
    toFloat c * 0.3


h : Int
h =
    600


w : Int
w =
    400


model : Model
model =
    { airspeed = 0.8, dist = 385, hst = -38, lsf = 24, turnsign = 1 }


pathPoints : { a | turnsign : Float, dist : Float, hst : Float, lsf : Float, airspeed : Float } -> { points : List (Vec2 Float), endPosition : Vec2 Float }
pathPoints { turnsign, dist, hst, lsf, airspeed } =
    let
        fadestart =
            0.4 + (1.0 - airspeed ^ 2) * 0.3

        impact =
            (1.0 - airspeed) / 5

        turnend =
            0.8 - airspeed ^ 2 * 0.36

        deltav =
            yscale / (yscale * dist)

        -- calculate effective HST and LSF
        op =
            if airspeed > 0.8 then
                ((airspeed - 0.8) / 0.4) ^ 3
            else
                1

        -- emphasize high-speed turn on sub-350ft discs
        dc =
            Basics.max 0 (350 - dist) / 10

        ehst =
            (hst - op * dc) * airspeed ^ 4 / 14000

        elsf =
            lsf / (airspeed ^ 2) / 4000

        loop { airspeed, vx, vy, x, y, points } =
            if airspeed <= impact then
                { points = points, endPosition = ( x, y ) }
            else
                let
                    newY =
                        y + vy * yscale

                    newX =
                        x + vx * xscale

                    newAirspeed =
                        airspeed - deltav

                    vx_ =
                        if newAirspeed > turnend then
                            vx - turnsign * ehst * (turnend / newAirspeed)
                        else
                            vx

                    newVx =
                        if newAirspeed < fadestart then
                            vx_ - turnsign * elsf * (fadestart - newAirspeed) / fadestart
                        else
                            vx_
                in
                loop { airspeed = newAirspeed, vx = newVx, vy = vy, x = newX, y = newY, points = ( x, y ) :: points }
    in
    loop
        { airspeed = airspeed
        , x = toFloat w / 2
        , y = toFloat h
        , vx = 0.0
        , vy = -1.0
        , points = []
        }


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
            , distanceLines { until = 150, diff = 5 }
            , hairline <| "M" ++ toString (w // 2) ++ " 0 V" ++ toString h
            , SubPath.element (Curve.catmullRom 0.0 points)
                [ fill "none"
                , stroke "blue"
                , strokeWidth "2"
                ]
            , dot { color = "blue", position = endPosition }
            ]
        , sliderForDistance model.dist
        , sliderForHst model.hst
        , sliderForLsf model.lsf
        , sliderForAirspeed model.airspeed
        ]


sliderForDistance : Float -> Html Msg
sliderForDistance current =
    Html.div []
        [ Html.text "Distance"
        , Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "100"
            , Html.Attributes.max "500"
            , Html.Attributes.step "10"
            , Html.Attributes.value (toString current)
            , onNumberInput DistanceChanged
            ]
            []
        , Html.text (toString (round current) ++ "ft")
        ]


sliderForAirspeed : Float -> Html Msg
sliderForAirspeed current =
    Html.div []
        [ Html.text "Throw speed relative to \"disc speed\""
        , Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "0.5"
            , Html.Attributes.max "1.2"
            , Html.Attributes.step "0.01"
            , Html.Attributes.value (toString current)
            , onNumberInput AirspeedChanged
            ]
            []
        , Html.text (toString (round (100 * current)) ++ "%")
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
            h - dist * c
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


dot : { color : String, position : Vec2 Float } -> Svg Msg
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
        AirspeedChanged airspeed ->
            { model | airspeed = airspeed }

        HstChanged hst ->
            { model | hst = hst }

        LsfChanged lsf ->
            { model | lsf = lsf }

        DistanceChanged dist ->
            { model | dist = dist }


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



-- Decode.map tagger (Decode.at [ "target", "value" ] Decode.float)
--     |> Html.Events.on "input"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = model ! []
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
