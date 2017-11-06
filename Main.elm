module Main exposing (..)

import Constants exposing (..)
import Data exposing (DiscData)
import Html exposing (Html, table, td, th, tr)
import Json.Decode as Decode
import Mouse exposing (Position)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (on)


main : Program Never Model Msg
main =
    Html.program
        { init = model ! []
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just { draggable } ->
            Sub.batch [ Mouse.moves (DragAt draggable), Mouse.ups (DragEnd draggable) ]


type alias Model =
    { startPosition : Position
    , startControl : Position
    , endControl : Position
    , endPosition : Position
    , drag : Maybe Drag
    , data : List DiscData
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


model : Model
model =
    { startPosition = Position (w // 2) h
    , startControl = Position (w // 2) (h // 2)
    , endControl = Position (w // 2 + w // 8) (h // 2 - h // 8)
    , endPosition = Position (w // 2 - w // 8) (h // 4)
    , drag = Nothing
    , data = Data.parseDiscData Data.prodigyJson |> Result.withDefault []
    }


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


view : Model -> Html Msg
view model =
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
            , rect
                [ width "10"
                , height "20"
                , x (toString (w // 2 - 5))
                , y (toString (h - 10))
                , fill "#DDDDDD"
                ]
                []
            , distanceLines
            , hairline <| "M" ++ toString (w // 2) ++ " 0 V" ++ toString h
            , bezierControl
                { position = model.startPosition, control = model.startControl }
                StartControl
            , bezierControl { position = model.endPosition, control = model.endControl }
                EndControl
            , Svg.path
                [ d <|
                    "M"
                        ++ posToString model.startPosition
                        ++ "C"
                        ++ posToString model.startControl
                        ++ ","
                        ++ posToString model.endControl
                        ++ ","
                        ++ posToString model.endPosition
                , fill "none"
                , stroke "blue"
                , strokeWidth "1"
                ]
                []
            , dot { color = "blue", position = model.endPosition } EndPosition
            , scrapedPoints model.data
            ]
        , dataTable model
        ]



-- View helpers


scrapedPoints : List DiscData -> Svg msg
scrapedPoints discDataList =
    discDataList
        |> List.head
        |> Maybe.map
            (\disc ->
                List.indexedMap
                    (\y x ->
                        circle
                            [ fill "rgba(0,255,0,0.2)"
                            , cx (toString (x + toFloat w / 2))
                            , cy (toString (h - y))
                            , r "1"
                            ]
                            []
                    )
                    disc.positions
                    |> g []
            )
        |> Maybe.withDefault (text "")


dataTable : { a | startControl : Position, endControl : Position, endPosition : Position } -> Html msg
dataTable { startControl, endControl, endPosition } =
    let
        cell pos =
            td [] [ Html.text (posToString pos) ]
    in
    table []
        [ tr []
            [ th [] [ Html.text "start control" ]
            , th [] [ Html.text "end control" ]
            , th [] [ Html.text "end position" ]
            ]
        , tr []
            [ cell startControl
            , cell endControl
            , cell endPosition
            ]
        ]


distanceLines : Svg msg
distanceLines =
    List.range 1 6
        |> List.map (\n -> distanceLine (25 * n))
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
            , y (toString (px + 10))
            , fontFamily "Arial"
            , fontSize "10"
            , fill "#AAAAAA"
            , Svg.Attributes.style "user-select: none"
            ]
            [ text (toString dist ++ "m") ]
        ]


hairline : String -> Svg msg
hairline pathD =
    Svg.path
        [ stroke "#DDDDDD"
        , strokeWidth "0.5"
        , d pathD
        ]
        []


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


dot : { color : String, position : Position } -> Draggable -> Svg Msg
dot { color, position } draggable =
    circle
        [ fill color
        , cx (toString position.x)
        , cy (toString position.y)
        , r "5"
        , onMouseDown draggable
        , Svg.Attributes.style "cursor: move"
        ]
        []


onMouseDown : Draggable -> Attribute Msg
onMouseDown draggable =
    on "mousedown" (Decode.map (DragStart draggable) Mouse.position)


posToString : Position -> String
posToString { x, y } =
    toString x ++ " " ++ toString y
