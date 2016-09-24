module Main exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (..)
import Html.App
import Html.Attributes exposing (style)
import Set exposing (Set)
import String
import Fn
import Sidebar exposing (sidebar)
import Types exposing (..)


main =
    Html.App.program
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : Model
init =
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
                , ( "cse141a", Course "cse141a" "CSE 141" "Porter" Dict.empty )
                ]
    in
        { me = Just "spencer@ucsd.edu"
        , users = users
        , courses = courses
        , selectedCourse = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectCourse id ->
            { model | selectedCourse = Just id } ! []


view : Model -> Html Msg
view { me, users, courses, selectedCourse } =
    case me of
        Just id ->
            case Dict.get id users of
                Just me ->
                    div [ style [ ( "display", "flex" ), ( "height", "100%" ) ] ]
                        [ sidebar me courses selectedCourse
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
            , enrolledStudentMessage sessionId course
            ]

        Just (Tutoring sessions) ->
            [ h1 [] [ text "You're tutoring" ] ]

        Nothing ->
            [ text "You're not in this class!" ]


enrolledStudentMessage : Maybe SessionId -> Course -> Html Msg
enrolledStudentMessage sessionId course =
    case sessionId of
        Just id ->
            text "Wow you're in!!!"

        Nothing ->
            p []
                [ text "You are "
                , span [ style [ ( "font-weight", "bolder" ) ] ] [ text "not" ]
                , text " registered for a 1:1 session. Click a tutor's name below to sign up."
                ]
