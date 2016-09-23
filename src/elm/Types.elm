module Types exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Set exposing (Set)
import String


type User
    = Student StudentInfo
    | Faculty FacultyInfo
    | Admin


type Msg
    = SelectCourse CourseId


type CourseRelation
    = Enrolled Id
    | Tutoring Id


type Registration
    = Open
    | Registered Id


type alias Model =
    { me : Maybe UserId
    , users : Dict UserId User
    , courses : Dict CourseId Course
    , selectedCourse : Maybe CourseId
    }



-- Like cse100abc, cse20a, cse20b, etc.


type alias CourseId =
    String


type alias EmailAddress =
    String


type alias UserId =
    EmailAddress



-- get a Uuid up in here


type alias Id =
    String


type alias BasicInfo =
    { name : String
    , id : UserId
    }


type alias StudentSpecific a =
    { a
        | courses : Dict CourseId CourseRelation
    }


type alias StudentInfo =
    StudentSpecific BasicInfo


type alias FacultySpecific a =
    { a
        | courses : Set CourseId
    }


type alias FacultyInfo =
    FacultySpecific BasicInfo


type alias Course =
    { id : CourseId
    , name : String
    , instructor : UserId
    , sessions : Dict Id Session
    }


type alias Session =
    { id : Id
    , time : Date
    , tutor : Id
    , location : String
    , registration : Registration
    }