module Main exposing (..)

import Browser
import DateFormat
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Time



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Event
    = OneTime { name : String, date : String }
    | LongTime { name : String, startDate : String, endDate : String }


type alias Model =
    { events : List Event }


init : Model
init =
    Model
        [ OneTime { name = "Event1", date = "17/04/1991" }
        , LongTime { name = "Event2", startDate = "17/04/1991", endDate = "17/04/1992" }
        ]



-- UPDATE


type Msg
    = Draw


update : Msg -> Model -> Model
update msg model =
    case msg of
        Draw ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ drawInput ()
        , drawEvents model.events
        ]


drawInput : () -> Html Msg
drawInput () =
    textarea [] [ text "TODO: read input from here" ]


drawEvents : List Event -> Html Msg
drawEvents events =
    div []
        [ text "Events"
        , drawEventsList events
        ]


drawEventsList : List Event -> Html Msg
drawEventsList events =
    ol [] (events |> List.map drawEventListItem)


drawEventListItem : Event -> Html Msg
drawEventListItem event =
    case event of
        OneTime data ->
            li [] [ text data.name ]

        LongTime data ->
            li [] [ text data.name ]



-- li [] [ text event.name ]
