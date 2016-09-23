module Fn exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import String

import Types exposing (..)


-- Get the Course models to which a User has a certain CourseRelation


getStudentCourses : Dict CourseId Course -> StudentInfo -> CourseRelation -> Dict CourseId Course
getStudentCourses allCourses student relation =
    let
        hasRelation : CourseRelation -> CourseId -> CourseRelation -> Bool
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
        Enrolled id ->
            "taking"

        Tutoring id ->
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
