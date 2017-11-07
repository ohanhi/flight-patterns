module Main exposing (..)

import Constants exposing (..)
import Data exposing (DiscData)
import Html exposing (Html, table, td, th, tr)
import Html.Attributes
import Html.Events
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
    , currentDisc : Maybe DiscData
    , savedRows : List RowData
    }


type alias RowData =
    { startControl : Position
    , endControl : Position
    , endPosition : Position
    , model : String
    , speed : Float
    , glide : Float
    , turn : Float
    , fade : Float
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
    | SelectDisc DiscData
    | Save


model : Model
model =
    { startPosition = Position (w // 2) h
    , startControl = Position (w // 2) (h // 2)
    , endControl = Position (w // 2 + w // 8) (h // 2 - h // 8)
    , endPosition = Position (w // 2 - w // 8) (h // 4)
    , drag = Nothing
    , data = List.sortBy .model Data.prodigy
    , currentDisc = List.head Data.prodigy
    , savedRows = []
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

        SelectDisc disc ->
            { model | currentDisc = Just disc }

        Save ->
            case model.currentDisc of
                Just disc ->
                    { model
                        | savedRows =
                            { startControl = model.startControl
                            , endControl = model.endControl
                            , endPosition = model.endPosition
                            , model = disc.model
                            , speed = disc.speed
                            , glide = disc.glide
                            , turn = disc.turn
                            , fade = disc.fade
                            }
                                :: model.savedRows
                    }

                Nothing ->
                    Debug.crash "No disc selected"


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
    Html.div [ Html.Attributes.style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
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
            , scrapedPoints model.currentDisc
            ]
        , Html.div []
            [ discSelect model.data
            , Html.button [ Html.Events.onClick Save ] [ Html.text "Save" ]
            , dataTable model.savedRows
            ]
        ]



-- View helpers


discSelect : List DiscData -> Html Msg
discSelect discs =
    discs
        |> List.map (\disc -> Html.button [ Html.Events.onClick (SelectDisc disc) ] [ Html.text disc.model ])
        |> Html.div []


scrapedPoints : Maybe DiscData -> Svg msg
scrapedPoints discData =
    discData
        |> Maybe.map
            (\disc ->
                List.indexedMap
                    (\y x ->
                        circle
                            [ fill "rgba(0,0,0,0.2)"
                            , cx (toString (x + toFloat w / 2))
                            , cy (toString (h - y))
                            , r "2"
                            ]
                            []
                    )
                    disc.positions
                    |> g []
            )
        |> Maybe.withDefault (text "")


dataTable : List RowData -> Html msg
dataTable rows =
    let
        cell pos =
            td [] [ Html.text (posToRelativeString pos) ]

        row { model, speed, glide, turn, fade, startControl, endControl, endPosition } =
            tr []
                [ td [] [ Html.text model ]
                , td [] [ Html.text (toString speed) ]
                , td [] [ Html.text (toString glide) ]
                , td [] [ Html.text (toString turn) ]
                , td [] [ Html.text (toString fade) ]
                , cell startControl
                , cell endControl
                , cell endPosition
                ]
    in
    table [] <|
        [ tr []
            [ th [] [ Html.text "model" ]
            , th [] [ Html.text "speed" ]
            , th [] [ Html.text "glide" ]
            , th [] [ Html.text "turn" ]
            , th [] [ Html.text "fade" ]
            , th [] [ Html.text "start control" ]
            , th [] [ Html.text "end control" ]
            , th [] [ Html.text "end position" ]
            ]
        ]
            ++ List.map row rows


distanceLines : Svg msg
distanceLines =
    List.range 1 6
        |> List.map (\n -> distanceLine (25 * toFloat n))
        |> g []


distanceLine : Float -> Svg msg
distanceLine dist =
    let
        px =
            toFloat h - dist * c
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


posToRelativeString : Position -> String
posToRelativeString { x, y } =
    toString (toFloat x / toFloat w) ++ " " ++ toString (toFloat (h - y) / toFloat h)
