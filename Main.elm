module Main exposing (..)

import Curve
import Html exposing (Html)
import Json.Decode as Decode
import Mouse exposing (Position)
import SubPath
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (on)
import Vector2 exposing (Vec2)


type alias Model =
    { startPosition : Position
    , startControl : Position
    , endControl : Position
    , endPosition : Position
    , drag : Maybe Drag
    }


type alias Drag =
    { draggable : Draggable
    , start : Position
    , current : Position
    }


type Draggable
    = StartControl
    | EndControl
    | EndPosition


type Msg
    = DragStart Draggable Position
    | DragAt Draggable Position
    | DragEnd Draggable Position


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
    { startPosition = Position (w // 2) h
    , startControl = Position (w // 2) (h // 2)
    , endControl = Position (w // 2 + w // 8) (h // 2 - h // 8)
    , endPosition = Position (w // 2 - w // 8) (h // 4)
    , drag = Nothing
    }


pathPoints : { a | turnsign : Float, dist : Float, hst : Float, lsf : Float, airspeed : Float } -> List (Vec2 Float)
pathPoints { turnsign, dist, hst, lsf, airspeed } =
    let
        fadestart =
            0.4 + (1.0 - airspeed ^ 2) * 0.3

        impact =
            (1.0 - airspeed) / 5

        turnend =
            0.8 - airspeed ^ 2 * 0.36

        ht =
            yscale * dist

        deltav =
            yscale / ht

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
                points
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


posToString : Position -> String
posToString { x, y } =
    toString x ++ " " ++ toString y


view : Model -> Html Msg
view model =
    let
        points =
            pathPoints { airspeed = 0.8, dist = 435, hst = -1, lsf = 73, turnsign = 1 }
    in
    svg
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
        , distanceLine 25
        , distanceLine 50
        , distanceLine 75
        , distanceLine 100
        , distanceLine 125
        , distanceLine 150
        , hairline <| "M" ++ toString (w // 2) ++ " 0 V" ++ toString h

        -- , bezierControl
        --     { position = model.startPosition, control = model.startControl }
        --     StartControl
        -- , bezierControl { position = model.endPosition, control = model.endControl }
        --     EndControl
        -- , Svg.path
        --     [ d <|
        --         "M"
        --             ++ posToString model.startPosition
        --             ++ "C"
        --             ++ posToString model.startControl
        --             ++ ","
        --             ++ posToString model.endControl
        --             ++ ","
        --             ++ posToString model.endPosition
        --     , fill "none"
        --     , stroke "blue"
        --     , strokeWidth "1"
        --     ]
        --     []
        , SubPath.element (Curve.catmullRom 0.0 points)
            [ fill "none"
            , stroke "red"
            , strokeWidth "1"
            ]

        -- , dot { color = "blue", position = model.endPosition } EndPosition
        ]


bezierControl : { position : Position, control : Position } -> Draggable -> Svg Msg
bezierControl { position, control } draggable =
    g []
        [ Svg.path
            [ fill "none"
            , stroke "red"
            , strokeWidth "1"
            , strokeOpacity "0.3"
            , d <| "M" ++ posToString position ++ "L" ++ posToString control
            ]
            []
        , dot { color = "red", position = control } draggable
        ]


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
            , y (toString (px + 10))
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


dot : { color : String, position : Position } -> Draggable -> Svg Msg
dot { color, position } draggable =
    circle
        [ fill color
        , cx (toString position.x)
        , cy (toString position.y)
        , r "3"
        , onMouseDown draggable
        ]
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    pureUpdate msg model ! []


pureUpdate : Msg -> Model -> Model
pureUpdate msg model =
    case msg of
        DragStart draggable xy ->
            { model | drag = Just (Drag draggable xy xy) }

        DragAt draggable xy ->
            { model | drag = Maybe.map (\{ start } -> Drag draggable start xy) model.drag }
                |> updateDraggable draggable xy

        DragEnd draggable xy ->
            { model | drag = Nothing }
                |> updateDraggable draggable xy


updateDraggable : Draggable -> Position -> Model -> Model
updateDraggable draggable position model =
    case draggable of
        StartControl ->
            { model | startControl = position }

        EndControl ->
            { model | endControl = position }

        EndPosition ->
            { model | endPosition = position }


onMouseDown : Draggable -> Attribute Msg
onMouseDown draggable =
    on "mousedown" (Decode.map (DragStart draggable) Mouse.position)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just { draggable } ->
            Sub.batch [ Mouse.moves (DragAt draggable), Mouse.ups (DragEnd draggable) ]


main : Program Never Model Msg
main =
    Html.program
        { init = model ! []
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
