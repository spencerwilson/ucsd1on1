module Sidebar exposing (sidebar)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import String
import Fn
import Types exposing (..)


sidebar : User -> Dict CourseId Course -> Maybe CourseId -> Html Msg
sidebar user allCourses selectedCourse =
    let
        courseSections : List (Html Msg)
        courseSections =
            case user of
                Student s ->
                    getStudentSidebarItems s allCourses selectedCourse

                Faculty f ->
                    getFacultySidebarItems f allCourses selectedCourse

                Admin ->
                    []
    in
        nav [ style [ ( "border", "solid 1px" ), ( "padding", "15px" ) ] ]
            [ header [ style [ ( "margin-bottom", "15px" ) ] ]
                [ div [ style [ ( "margin-bottom", "10px" ) ] ]
                    [ h1 [ style [ ( "margin-bottom", "0px" ) ] ] [ text "UCSD 1:1" ]
                    , div [] [ text "Week 0" ]
                    ]
                , p [] [ text <| String.concat [ "Ahoy, ", Fn.displayName user ] ]
                , section [] courseSections
                ]
            ]


getStudentSidebarItems : StudentInfo -> Dict CourseId Course -> Maybe CourseId -> List (Html Msg)
getStudentSidebarItems student allCourses selectedCourse =
    [ Enrolled, Tutoring ]
        |> List.map (Fn.getStudentCourses allCourses student)
        |> List.map2 (,) [ Enrolled, Tutoring ]
        |> List.concatMap
            (\( relation, dict ) ->
                if Dict.size dict > 0 then
                    [ courseList (Fn.relationToString relation) dict selectedCourse ]
                else
                    []
            )


getFacultySidebarItems : FacultyInfo -> Dict CourseId Course -> Maybe CourseId -> List (Html Msg)
getFacultySidebarItems faculty allCourses selectedCourse =
    let
        courses : Dict CourseId Course
        courses =
            Fn.getFacultyCourses allCourses faculty
    in
        if Dict.size courses > 0 then
            [ courseList "teaching" courses selectedCourse ]
        else
            []


courseList : String -> Dict CourseId Course -> Maybe CourseId -> Html Msg
courseList relation courses selectedCourse =
    div []
        [ span []
            [ text "Courses you're "
            , span [ style [ ( "font-weight", "bolder" ) ] ] [ text relation ]
            ]
        , ul [] <| List.map (courseNavItem selectedCourse) (Dict.values courses)
        ]


courseNavItem : Maybe CourseId -> Course -> Html Msg
courseNavItem selectedCourse course =
    let
        activeStyle =
            case selectedCourse of
                Just id ->
                    if id == course.id then
                        [ ( "background-color", "gold" ) ]
                    else
                        []

                Nothing ->
                    []
    in
        li [ style activeStyle, onClick (SelectCourse course.id) ]
            [ a [] [ text <| String.concat [ course.name, " // ", course.instructor ] ] ]
