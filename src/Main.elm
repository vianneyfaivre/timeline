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


type alias Timeline =
    { startDate : String, endDate : String }


type TimelineDate
    = Start
    | End


type alias Model =
    { events : List Event }


init : Model
init =
    Model
        [ LongTime { name = "API v1", startDate = "01/01/2020", endDate = "01/05/2020" }
        , OneTime { name = "EOL", date = "01/05/2020" }
        , LongTime { name = "API v2", startDate = "01/05/2020", endDate = "31/12/2020" }
        ]


toTimeline : List Event -> Timeline
toTimeline events =
    { startDate = getTimelineDate events Start, endDate = getTimelineDate events End }


getTimelineDate : List Event -> TimelineDate -> String
getTimelineDate events timelineDate =
    case timelineDate of
        Start ->
            -- events |> List.sortWith eventComparison |> List.maximum
            "startTODO"

        End ->
            "endTODO"



-- eventComparison : Event -> Event -> Order
-- eventComparison e1 e2 =
--     GT
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
    -- do the business logic
    let
        timeline =
            toTimeline model.events
    in
    -- draw the HTML
    div []
        [ drawInput ()
        , drawEvents model.events
        , drawTimeline timeline
        ]


drawInput : () -> Html Msg
drawInput () =
    textarea [] [ text "TODO: When the submit button has been clicked, read events from this input, and update the view" ]


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


drawTimeline : Timeline -> Html Msg
drawTimeline timeline =
    timeline.startDate ++ " " ++ timeline.endDate |> text
