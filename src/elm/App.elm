module Main exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (..)
import Html.App
import Html.Attributes exposing (style)
import Navigation
import Maybe exposing (andThen)
import Set exposing (Set)
import String
import Time exposing (Time)
import Fn
import Sidebar exposing (sidebar)
import Types exposing (..)


main =
    Navigation.program urlParser
        { init = init
        , update = update
        , view = view
        , urlUpdate = urlUpdate
        , subscriptions = \_ -> Sub.none
        }


dropHash : String -> String
dropHash =
    String.dropLeft 2


urlParser : Navigation.Parser Page
urlParser =
    Navigation.makeParser (.hash >> dropHash >> CoursePage)


toUrl : Maybe CourseId -> String
toUrl courseId =
    "#/" ++ Maybe.withDefault "App.elm" courseId


urlUpdate : Page -> Model -> ( Model, Cmd Msg )
urlUpdate page model =
    case page of
        CoursePage courseId ->
            if courseId == "App.elm" then
                model ! []
            else if courseId == "App.elm" || Dict.member courseId model.courses then
                ( { model | selectedCourse = Just courseId }
                , Cmd.none
                )
            else
                ( model
                , Navigation.modifyUrl <| toUrl model.selectedCourse
                )


init : Page -> ( Model, Cmd Msg )
init page =
    let
        users =
            Dict.fromList
                [ ( "spencer@ucsd.edu"
                  , Student
                        { name = "Spencer"
                        , id = "spencer@ucsd.edu"
                        , courses =
                            Dict.fromList
                                [ ( "cse120a", Enrolled (Just "session0") )
                                , ( "cse141a", Enrolled Nothing )
                                , ( "cse101abc", Tutoring Set.empty )
                                ]
                        }
                  )
                , ( "joetutor@ucsd.edu"
                  , Student
                        { name = "Joe Tutor"
                        , id = "joetutor@ucsd.edu"
                        , courses =
                            Dict.fromList
                                [ ( "cse120a", Tutoring (Set.fromList [ "session0" ]) )
                                , ( "cse141a", Tutoring (Set.fromList [ "session0" ]) )
                                ]
                        }
                  )
                ]

        courses =
            Dict.fromList
                [ ( "cse21ab", Course "cse21ab" "CSE 21" "Tiefenbruck" Dict.empty )
                , ( "cse101abc", Course "cse101abc" "CSE 101" "Jones" Dict.empty )
                , ( "cse120a"
                  , Course "cse120a"
                        "CSE 120"
                        "Voelker"
                        (Dict.fromList
                            [ ( "session0", Session "session0" (Date.fromTime 0) "joetutor@ucsd.edu" "CSE Basement" Registered )
                            ]
                        )
                  )
                , ( "cse141a"
                  , Course "cse141a"
                        "CSE 141"
                        "Porter"
                        (Dict.fromList
                            [ ( "session1", Session "session1" (Date.fromTime 0) "joetutor@ucsd.edu" "CSE Basement" Open )
                            ]
                        )
                  )
                ]

        initialModel =
            { me = Just "spencer@ucsd.edu"
            , users = users
            , courses = courses
            , selectedCourse = Nothing
            , currentWeek = 1
            }
    in
        urlUpdate page initialModel
            |> Fn.addCmdToPair Fn.getWeekNumber


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWeek week ->
            { model | currentWeek = week } ! []

        SelectCourse id ->
            ( { model | selectedCourse = Just id }
            , Navigation.modifyUrl <| toUrl (Just id)
            )


view : Model -> Html Msg
view { me, users, courses, selectedCourse, currentWeek } =
    case me of
        Just id ->
            case Dict.get id users of
                Just me ->
                    div [ style [ ( "display", "flex" ), ( "height", "100%" ) ] ]
                        [ sidebar me courses selectedCourse currentWeek
                        , courseContainer me users courses selectedCourse
                        ]

                Nothing ->
                    Debug.crash "currentUser lookup failed"

        Nothing ->
            text "Please reload the page to log in"


courseContainer : User -> Dict UserId User -> Dict CourseId Course -> Maybe CourseId -> Html Msg
courseContainer me users courses selectedCourse =
    let
        body =
            case selectedCourse of
                Just id ->
                    case Dict.get id courses of
                        Just course ->
                            courseBody me users course

                        Nothing ->
                            Debug.crash "selectedCourse lookup failed"

                Nothing ->
                    [ h1 [] [ text "Select a course on the left" ] ]
    in
        main' [ style [ ( "margin", "15px" ) ] ] body


courseBody : User -> Dict UserId User -> Course -> List (Html Msg)
courseBody me users course =
    case me of
        Student info ->
            studentCourseView info users course

        Faculty info ->
            --facultyCourseView info
            [ h1 [] [ text course.name ] ]

        Admin ->
            --adminCourseView info
            [ h1 [] [ text course.name ] ]


studentCourseView : StudentInfo -> Dict UserId User -> Course -> List (Html Msg)
studentCourseView me users course =
    case Dict.get course.id me.courses of
        Just (Enrolled sessionId) ->
            [ h1 [] [ text course.name ]
            , p [] <| enrolledStudentMessage course users sessionId
            ]

        Just (Tutoring sessions) ->
            [ h1 [] [ text "You're tutoring" ] ]

        Nothing ->
            [ text "You're not in this class!" ]


boldFont =
    style [ ( "font-weight", "bolder" ) ]


italicFont =
    style [ ( "font-style", "italic" ) ]


enrolledStudentMessage : Course -> Dict UserId User -> Maybe SessionId -> List (Html Msg)
enrolledStudentMessage course users sessionId =
    case sessionId of
        Just id ->
            let
                session =
                    Dict.get id course.sessions
                        |> Maybe.withDefault (Session id (Date.fromTime 0) "unknown" "unknown" Registered)

                tutor =
                    Dict.get session.tutor users

                tutorName =
                    case tutor of
                        Just (Student info) ->
                            info.name

                        Just (Faculty info) ->
                            info.name

                        Just Admin ->
                            "THE ADMIN"

                        Nothing ->
                            "unknown"
            in
                [ text "You are signed up for a 1:1..."
                , div [ style [ ( "margin-left", "1.2em" ) ] ]
                    [ span [ italicFont ] [ text "with " ]
                    , span [ boldFont ] [ text tutorName ]
                    , br [] []
                    , span [ italicFont ] [ text "on " ]
                    , span [ boldFont ] [ text <| Fn.formatDate session.time ]
                    , br [] []
                    , span [ italicFont ] [ text " at " ]
                    , span [ boldFont ] [ text <| Fn.formatTime session.time ]
                    , br [] []
                    , span [ italicFont ] [ text " in " ]
                    , span [ boldFont ] [ text session.location ]
                    ]
                , text " See you then!"
                ]

        Nothing ->
            [ text "You are "
            , span [ style [ ( "font-weight", "bolder" ) ] ] [ text "not" ]
            , text " signed up for a 1:1 session. Click a tutor's name below to sign up."
            , viewCourseSessions course users
            ]


viewCourseSessions : Course -> Dict UserId User -> Html Msg
viewCourseSessions course users =
    let
        openSessions =
            course.sessions
                |> Dict.filter (\_ { registration } -> registration == Open)

        pairs =
            pairSessionsWithTutors openSessions users
    in
        pairs
            |> aggregateByTime
            |> Dict.map viewSessionBlock
            |> Dict.values
            |> div []



--div [] List.map (viewSession users) course.sessions


aggregateByTime : List ( Session, Maybe User ) -> Dict Time (List ( Session, Maybe User ))
aggregateByTime =
    let
        upsert ( session, user ) dict =
            let
                time =
                    Date.toTime session.time
            in
                case Dict.get time dict of
                    Just list ->
                        Dict.insert time (( session, user ) :: list) dict

                    Nothing ->
                        Dict.insert time [ ( session, user ) ] dict
    in
        List.foldr upsert Dict.empty


pairSessionsWithTutors : Dict SessionId Session -> Dict UserId User -> List ( Session, Maybe User )
pairSessionsWithTutors sessions users =
    let
        getTutorForSession session =
            Dict.get session.tutor users
    in
        sessions
            |> Dict.values
            |> List.map (\s -> ( s, getTutorForSession s ))


viewSessionBlock : Time -> List ( Session, Maybe User ) -> Html Msg
viewSessionBlock time sessionTutorPairs =
    let
        timeAsDate = Date.fromTime time
        timeText = (Fn.formatDate timeAsDate) ++ " " ++ (Fn.formatTime timeAsDate)
        timeLabel =
            label [] [ text timeText ]

        sessions =
            List.map viewSessionEntry sessionTutorPairs
    in
        div [] (timeLabel :: sessions)


viewSessionEntry : ( Session, Maybe User ) -> Html Msg
viewSessionEntry ( session, user ) =
    li [] [ text <| toString ( session, user ) ]
