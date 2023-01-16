module Main exposing (main)

import Array as A
import Browser
import Dict
import Element exposing (..)
import Element.Input as Input
import Random exposing (..)



-- import String exposing (fromInt)
---------------------------------------------------------------------------------


type Msg
    = Show
    | Next
    | RandomGen
    | NewChord Int
    | NoOp



---------------------------------------------------------------------------------


type alias Model =
    { chordName : String
    , chordNotes : String
    , isNameVisible : Bool
    , buttonText : String
    }



---------------------------------------------------------------------------------


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



---------------------------------------------------------------------------------


init : () -> ( Model, Cmd Msg )
init () =
    ( { chordName = "C Major 7th"
      , chordNotes = "C E G B"
      , isNameVisible = False
      , buttonText = "Show Answer"
      }
    , -- override default values
      generateChordName
    )



---------------------------------------------------------------------------------


generateChordName : Cmd Msg
generateChordName =
    Random.generate NewChord
        (Random.int 1
            (Dict.size
                allChords
            )
        )


decide : Int -> String
decide id =
    A.get (id - 1) majorChordsNameArray |> Maybe.withDefault "Name not found"



---------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        RandomGen ->
            ( model
            , generateChordName
            )

        NewChord chordID ->
            ( { model | chordName = decide chordID, chordNotes = "test" }, Cmd.none )

        Show ->
            ( { model | isNameVisible = True, buttonText = "Show Next" }, Cmd.none )

        Next ->
            let
                ( model1, cmds ) =
                    update RandomGen model
            in
            ( { model1 | isNameVisible = False, buttonText = "Show Answer" }
            , cmds
            )



---------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "What notes make up the chord?"
    , body =
        [ allElements model ]
    }



---------------------------------------------------------------------------------


resultElement model =
    if model.isNameVisible then
        el
            [ width fill, centerX ]
            (text (getChordNotes model.chordName))

    else
        el [ width fill, centerX ] (text "")



---------------------------------------------------------------------------------


inputElement model =
    el
        [ width fill, centerX ]
        (text model.chordName)



---------------------------------------------------------------------------------


button model =
    Input.button
        [ width fill, centerX ]
        { onPress =
            if model.isNameVisible then
                Just Next

            else
                Just Show
        , label = el [ centerX ] (text model.buttonText)
        }



---------------------------------------------------------------------------------


allElements model =
    let
        finalElement =
            column [ centerX, spacing 30 ]
                [ inputElement model
                , resultElement model
                , button model
                ]
    in
    layout [ width fill, height fill ] <| finalElement



---------------------------------------------------------------------------------


getChordNotes : String -> String
getChordNotes chordName =
    allChords |> Dict.get chordName |> Maybe.withDefault "Chord not found"



---------------------------------------------------------------------------------


allChords =
    Dict.union majorChords minorChords



---------------------------------------------------------------------------------


majorChords : Dict.Dict String String
majorChords =
    Dict.fromList
        [ ( "A Major 7th", "A C# E G#" )
        , ( "A# Major 7th", "A# D F A" )
        , ( "B Major 7th", "B  D# F# A#" )
        , ( "C Major 7th", "C E G B" )
        , ( "C# Major 7th", "C# E# G# C" )
        , ( "D Major 7th", "D F# A C#" )
        , ( "D# Major 7th", "D# G A# D" )
        , ( "E Major 7th", "E G# B D#" )
        , ( "F Major 7th", "F A C E" )
        , ( "F# Major 7th", "F# A# C# F" )
        , ( "G Major 7th", "G B D F#" )
        , ( "G# Major 7th", "G# C D# G" )
        ]



---------------------------------------------------------------------------------


minorChords : Dict.Dict String String
minorChords =
    Dict.fromList
        [ ( "A Minor 7th", "A C E G#" )
        , ( "A# Minor 7th", "A# C F A" )
        , ( "B Minor 7th", "B  D F# A#" )
        , ( "C Minor 7th", "C D# G B" )
        , ( "C# Minor 7th", "C# E G# C" )
        , ( "D Minor 7th", "D F A C#" )
        , ( "D# Minor 7th", "D# F A# D" )
        , ( "E Minor 7th", "E G B D#" )
        , ( "F Minor 7th", "F G# C E" )
        , ( "F# Minor 7th", "F# A C# F" )
        , ( "G Minor 7th", "G A# D F#" )
        , ( "G# Minor 7th", "G# B D# G" )
        ]



---------------------------------------------------------------------------------


majorChordsNameArray =
    A.fromList (Dict.keys allChords)
