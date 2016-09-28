module Fn exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Set exposing (Set)
import String
import Task
import Time exposing (Time)
import Types exposing (..)


addCmdToPair : Cmd a -> ( b, Cmd a ) -> ( b, Cmd a )
addCmdToPair cmd ( first, other ) =
    ( first, Cmd.batch [ cmd, other ] )


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


formatDate : Date -> String
formatDate date =
    toString (Date.dayOfWeek date)
        ++ ", "
        ++ toString (Date.month date)
        ++ " "
        ++ toString (Date.day date)


formatTime : Date -> String
formatTime date =
    let
        hour =
            toString (Date.hour date % 13 + 1)

        minute =
            toString (Date.minute date)

        amPm =
            if (Date.hour date) >= 12 then
                "PM"
            else
                "AM"
    in
        hour
            ++ ":"
            ++ (if String.length minute == 1 then
                    "0"
                else
                    ""
               )
            ++ minute
            ++ " "
            ++ amPm
