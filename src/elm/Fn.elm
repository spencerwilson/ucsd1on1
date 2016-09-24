module Fn exposing (..)

import Date
import Dict exposing (Dict)
import Set exposing (Set)
import String
import Task
import Time exposing (Time)
import Types exposing (..)


getWeekNumber : Cmd Msg
getWeekNumber =
    let
        oneWeek =
            24 * Time.hour * 7

        weekZeroStart =
            1474182000000

        computeWeek : Time -> Msg
        computeWeek now =
            SetWeek <| floor <| (now - weekZeroStart) / oneWeek
    in
        Time.now
            |> Task.perform (\_ -> SetWeek <| Debug.crash "") computeWeek



-- Get the Course models to which a User has a certain CourseRelation


getStudentCourses : Dict CourseId Course -> StudentInfo -> CourseRelation -> Dict CourseId Course
getStudentCourses allCourses student relation =
    let
        hasRelation first _ second =
            case ( first, second ) of
                ( Enrolled _, Enrolled _ ) ->
                    True

                ( Tutoring _, Tutoring _ ) ->
                    True

                _ ->
                    False
    in
        student.courses
            |> Dict.filter (hasRelation relation)
            |> Dict.keys
            |> List.filterMap (\id -> Dict.get id allCourses)
            |> List.map (\c -> ( c.id, c ))
            |> Dict.fromList


getFacultyCourses : Dict CourseId Course -> FacultyInfo -> Dict CourseId Course
getFacultyCourses allCourses faculty =
    faculty.courses
        |> Set.toList
        |> List.filterMap (\id -> Dict.get id allCourses)
        |> List.map (\c -> ( c.id, c ))
        |> Dict.fromList


relationToString : CourseRelation -> String
relationToString relation =
    case relation of
        Enrolled _ ->
            "taking"

        Tutoring _ ->
            "tutoring"


displayName : User -> String
displayName user =
    case user of
        Student s ->
            s.name

        Faculty f ->
            f.name

        Admin ->
            "admin"
