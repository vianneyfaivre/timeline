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
    { name : String
    , startDate : Date
    , endDate : Maybe Date
    }


type alias Timeline =
    { startDate : Date
    , endDate : Date
    , events : List Event
    }


type TimelineDateType
    = TimelineStart
    | TimelineEnd


type EventType
    = Start -- beginning of an event
    | Between -- in between an event
    | End -- end of an event
    | Single -- start date == end date
    | None -- no event


type alias Model =
    { events : List Event }


init : Model
init =
    Model
        [ { name = "API v1", startDate = fromCalendarDate 2020 Jan 12, endDate = Just (fromCalendarDate 2020 May 9) }
        , { name = "EOL", startDate = fromCalendarDate 2020 May 10, endDate = Nothing }
        , { name = "API v2", startDate = fromCalendarDate 2020 May 11, endDate = Just (fromCalendarDate 2021 Feb 25) }
        ]


toTimeline : List Event -> Timeline
toTimeline events =
    { startDate = getTimelineDate events TimelineStart
    , endDate = getTimelineDate events TimelineEnd
    , events = events
    }


getTimelineDate : List Event -> TimelineDateType -> Date
getTimelineDate events timelineDateType =
    case ( events, timelineDateType ) of
        ( [], _ ) ->
            fromCalendarDate 1991 Apr 17

        ( _, TimelineStart ) ->
            case
                events
                    |> List.sortWith eventComparisonAsc
                    |> List.head
            of
                Nothing ->
                    fromCalendarDate 1991 Apr 19

                Just event ->
                    event.startDate

        ( _, TimelineEnd ) ->
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
        TimelineStart ->
            Date.compare e1.startDate e2.startDate

        TimelineEnd ->
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
    eventComparison e1 e2 TimelineStart


eventComparisonDesc : Event -> Event -> Order
eventComparisonDesc e1 e2 =
    eventComparison e1 e2 TimelineEnd



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
    let
        months =
            Date.diff Months timeline.startDate timeline.endDate

        cells =
            List.range 0 months
    in
    div []
        [ table []
            (List.concat
                [ tr [] (cells |> List.map (drawTableHeader timeline) |> (++) [ td [] [ text "Event" ] ]) |> List.singleton
                , List.map
                    (drawTableCells cells timeline)
                    timeline.events
                ]
            )
        ]


drawTableHeader : Timeline -> Int -> Html Msg
drawTableHeader timeline monthNumber =
    Date.add Months monthNumber timeline.startDate
        |> format "MM-yyyy"
        |> text
        |> List.singleton
        |> td []


drawTableCells : List Int -> Timeline -> Event -> Html Msg
drawTableCells cells timeline event =
    cells
        |> List.map (drawTableCell timeline event)
        |> (++) [ td [] [ text event.name ] ]
        |> tr []


drawTableCell : Timeline -> Event -> Int -> Html Msg
drawTableCell timeline event monthNumber =
    let
        currentDate =
            Date.add Months monthNumber timeline.startDate
    in
    case getEventType event currentDate of
        None ->
            td [] [ text "." ]

        Between ->
            td [] [ text "-" ]

        Start ->
            td [] [ text "|" ]

        End ->
            td [] [ text ">" ]

        Single ->
            td [] [ text "o" ]


getEventType : Event -> Date -> EventType
getEventType event currentDate =
    let
        currentMonth =
            Date.month currentDate

        currentYear =
            Date.year currentDate

        eventStartMonth =
            Date.month event.startDate

        eventStartYear =
            Date.year event.startDate
    in
    case event.endDate of
        Nothing ->
            if currentMonth == eventStartMonth && currentYear == eventStartYear then
                Single

            else
                None

        Just endDate ->
            if eventIsBetween event.startDate endDate currentDate then
                Between

            else if currentMonth == eventStartMonth && currentYear == eventStartYear then
                Start

            else if currentMonth == Date.month endDate && currentYear == Date.year endDate then
                End

            else
                None


eventIsBetween : Date -> Date -> Date -> Bool
eventIsBetween startDate endDate currentDate =
    let
        low =
            Date.fromCalendarDate (Date.year startDate) (Date.month startDate) 1 |> Date.add Months 1

        high =
            Date.fromCalendarDate (Date.year endDate) (Date.month endDate) 1
    in
    Date.isBetween low high currentDate
