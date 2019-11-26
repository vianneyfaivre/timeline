module Main exposing (..)

import Browser
import Date exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Task exposing (..)
import Time exposing (Month(..))



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Event
    = OneTime { name : String, date : Date }
    | LongTime { name : String, startDate : Date, endDate : Date }


getName : Event -> String
getName e =
    case e of
        OneTime data ->
            data.name

        LongTime data ->
            data.name


type alias Timeline =
    { startDate : Date, endDate : Date }


type TimelineDateType
    = Start
    | End


type alias Model =
    { events : List Event }


init : Model
init =
    Model
        [ LongTime { name = "API v1", startDate = fromCalendarDate 2020 Jan 1, endDate = fromCalendarDate 2020 May 1 }
        , OneTime { name = "EOL", date = fromCalendarDate 2020 May 1 }
        , LongTime { name = "API v2", startDate = fromCalendarDate 2020 May 1, endDate = fromCalendarDate 2020 Dec 31 }
        ]


toTimeline : List Event -> Timeline
toTimeline events =
    { startDate = getTimelineDate events Start, endDate = getTimelineDate events End }


getTimelineDate : List Event -> TimelineDateType -> Date
getTimelineDate events timelineDateType =
    case ( events, timelineDateType ) of
        ( [], _ ) ->
            fromCalendarDate 1991 Apr 17

        ( _, Start ) ->
            case
                events
                    |> List.sortWith eventComparisonAsc
                    |> List.head
            of
                Nothing ->
                    fromCalendarDate 1991 Apr 19

                Just (OneTime data) ->
                    data.date

                Just (LongTime data) ->
                    data.startDate

        ( _, End ) ->
            case
                events
                    |> List.sortWith eventComparisonDesc
                    |> List.reverse
                    |> List.head
            of
                Nothing ->
                    fromCalendarDate 1991 Apr 20

                Just (OneTime data) ->
                    data.date

                Just (LongTime data) ->
                    data.endDate


eventComparison : Event -> Event -> TimelineDateType -> Order
eventComparison e1 e2 sortBy =
    case ( e1, e2, sortBy ) of
        ( OneTime data1, OneTime data2, _ ) ->
            Date.compare data1.date data2.date

        ( OneTime data1, LongTime data2, Start ) ->
            Date.compare data1.date data2.startDate

        ( OneTime data1, LongTime data2, End ) ->
            Date.compare data1.date data2.endDate

        ( LongTime data1, OneTime data2, Start ) ->
            Date.compare data1.startDate data2.date

        ( LongTime data1, OneTime data2, End ) ->
            Date.compare data1.endDate data2.date

        ( LongTime data1, LongTime data2, Start ) ->
            Date.compare data1.startDate data2.startDate

        ( LongTime data1, LongTime data2, End ) ->
            Date.compare data1.endDate data2.endDate


eventComparisonAsc : Event -> Event -> Order
eventComparisonAsc e1 e2 =
    eventComparison e1 e2 Start


eventComparisonDesc : Event -> Event -> Order
eventComparisonDesc e1 e2 =
    eventComparison e1 e2 End



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
            li [] [ data.name ++ " " ++ toIsoString data.date |> text ]

        LongTime data ->
            li [] [ data.name ++ " " ++ toIsoString data.startDate ++ " - " ++ toIsoString data.endDate |> text ]


drawTimeline : Timeline -> Html Msg
drawTimeline timeline =
    ol []
        [ li [] [ "Start:" ++ toIsoString timeline.startDate |> text ]
        , li [] [ "End:" ++ toIsoString timeline.endDate |> text ]
        ]
