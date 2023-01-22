module Main exposing (main)

import Array as A
import Browser
import Dict
import Element exposing (..)
import Element.Input as Input
import Random exposing (..)



---------------------------------------------------------------------------------


type Msg
    = Show
    | Next
    | MajorChecked Bool
    | MinorChecked Bool
    | RandomGen
    | NewChord Int
    | NoOp



---------------------------------------------------------------------------------


type alias CheckedInfo =
    { majorChecked : Bool
    , minorChecked : Bool
    }



---------------------------------------------------------------------------------


type alias Model =
    { chordName : String
    , chordNotes : String
    , isNameVisible : Bool
    , buttonText : String
    , checkedInfo : CheckedInfo
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
    let
        defaultCheckedInfo =
            CheckedInfo True True
    in
    ( { chordName = "C Major 7th"
      , chordNotes = "C E G B"
      , isNameVisible = False
      , buttonText = "Show Answer"
      , checkedInfo = defaultCheckedInfo
      }
    , -- override default values
      generateChordName defaultCheckedInfo
    )



---------------------------------------------------------------------------------


generateChordName : CheckedInfo -> Cmd Msg
generateChordName checkedInfo =
    Random.generate NewChord
        (Random.int 1
            (Dict.size
                (allChords
                    checkedInfo
                )
            )
        )



---------------------------------------------------------------------------------


decide : CheckedInfo -> Int -> String
decide checkedInfo id =
    A.get (id - 1) (allChordsNameArray checkedInfo) |> Maybe.withDefault "Name not found"



---------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        RandomGen ->
            ( model
            , generateChordName model.checkedInfo
            )

        NewChord chordID ->
            ( { model | chordName = decide model.checkedInfo chordID, chordNotes = "test" }, Cmd.none )

        MajorChecked isChecked ->
            if not model.checkedInfo.minorChecked && model.checkedInfo.majorChecked then
                ( model, Cmd.none )

            else
                ( { model
                    | checkedInfo =
                        CheckedInfo
                            isChecked
                            model.checkedInfo.minorChecked
                    , isNameVisible = False
                  }
                , Cmd.none
                )

        MinorChecked isChecked ->
            if not model.checkedInfo.majorChecked && model.checkedInfo.minorChecked then
                ( model, Cmd.none )

            else
                ( { model
                    | checkedInfo =
                        CheckedInfo
                            model.checkedInfo.majorChecked
                            isChecked
                    , isNameVisible = False
                  }
                , Cmd.none
                )

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



---------------------------------------------------------------------------------


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
            (text (getChordNotes model))

    else
        el [ width fill, centerX ] (text "")



---------------------------------------------------------------------------------


inputElement model =
    el
        [ width fill, centerX ]
        (text model.chordName)



---------------------------------------------------------------------------------


checkBoxMajor checkedInfo =
    Input.checkbox [ width fill, centerX ]
        { onChange = MajorChecked
        , icon = Input.defaultCheckbox
        , checked = checkedInfo.majorChecked
        , label = Input.labelRight [] (text "Major chords")
        }



---------------------------------------------------------------------------------


checkBoxMinor checkedInfo =
    Input.checkbox [ width fill, centerX ]
        { onChange = MinorChecked
        , icon = Input.defaultCheckbox
        , checked = checkedInfo.minorChecked
        , label = Input.labelRight [] (text "Minor chords")
        }



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
        firstColumn =
            column
                [ spacing 30 ]
                [ Element.el [] none
                , inputElement model
                , resultElement model
                , button model
                ]

        secondColumn =
            column
                [ spacing 30 ]
                [ Element.el [] none
                , Element.el [] (checkBoxMajor model.checkedInfo)
                , Element.el [] (checkBoxMinor model.checkedInfo)
                ]

        firstRow =
            row
                [ centerX, spacing 30 ]
                [ firstColumn, secondColumn ]

        finalElement =
            layout [ width fill, height fill ] <| firstRow
    in
    layout [ width fill, height fill ] <| Element.html finalElement



---------------------------------------------------------------------------------


getChordNotes : Model -> String
getChordNotes model =
    allChords model.checkedInfo |> Dict.get model.chordName |> Maybe.withDefault "Chord not found"



---------------------------------------------------------------------------------


allChords checkedInfo =
    if checkedInfo.majorChecked && checkedInfo.minorChecked then
        Dict.union majorChords minorChords

    else if checkedInfo.majorChecked then
        majorChords

    else
        minorChords



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
        , ( "A# Minor 7th", "A# C# F A" )
        , ( "B Minor 7th", "B D F# A#" )
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


allChordsNameArray checkedInfo =
    A.fromList (Dict.keys (allChords checkedInfo))
