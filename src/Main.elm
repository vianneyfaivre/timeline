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


type alias Event =
    { name : String, startDate : Date, endDate : Maybe Date }


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
        [ { name = "API v1", startDate = fromCalendarDate 2020 Jan 1, endDate = Just (fromCalendarDate 2020 May 1) }
        , { name = "EOL", startDate = fromCalendarDate 2020 May 1, endDate = Nothing }
        , { name = "API v2", startDate = fromCalendarDate 2020 May 1, endDate = Just (fromCalendarDate 2020 Dec 31) }
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

                Just event ->
                    event.startDate

        ( _, End ) ->
            case
                events
                    |> List.sortWith eventComparisonDesc
                    |> List.reverse
                    |> List.head
            of
                Nothing ->
                    fromCalendarDate 1991 Apr 20

                Just event ->
                    case event.endDate of
                        Nothing ->
                            event.startDate

                        Just endDate ->
                            endDate


eventComparison : Event -> Event -> TimelineDateType -> Order
eventComparison e1 e2 sortBy =
    case sortBy of
        Start ->
            Date.compare e1.startDate e2.startDate

        End ->
            case ( e1.endDate, e2.endDate ) of
                ( Nothing, Nothing ) ->
                    Date.compare e1.startDate e2.startDate

                ( Just endDate, Nothing ) ->
                    Date.compare endDate e2.startDate

                ( Nothing, Just endDate ) ->
                    Date.compare e1.startDate endDate

                ( Just endDate, Just endDate2 ) ->
                    Date.compare endDate endDate2


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
    case event.endDate of
        Nothing ->
            li [] [ event.name ++ " " ++ toIsoString event.startDate |> text ]

        Just endDate ->
            li [] [ event.name ++ " " ++ toIsoString event.startDate ++ " - " ++ toIsoString endDate |> text ]


drawTimeline : Timeline -> Html Msg
drawTimeline timeline =
    ol []
        [ li [] [ "Start:" ++ toIsoString timeline.startDate |> text ]
        , li [] [ "End:" ++ toIsoString timeline.endDate |> text ]
        ]
