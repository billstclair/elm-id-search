---------------------------------------------------------------------
--
-- Main.elm
-- GabDecker top-level
-- Copyright (c) 2018-2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
-- Search for TODO to see remaining work.
-- Also see ../TODO.md
--
----------------------------------------------------------------------


module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import CustomElement.TextAreaTracker as Tracker
import Element
    exposing
        ( Attribute
        , Color
        , Element
        , alignBottom
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , height
        , image
        , link
        , padding
        , paddingEach
        , paragraph
        , px
        , row
        , spacing
        , text
        , textColumn
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html exposing (Html)
import Html.Attributes as Attributes exposing (class, href, rel)
import Html.Events exposing (onClick)
import Http
import IdSearch
import Json.Decode as JD exposing (Decoder)
import String.Extra as SE
import Task
import Url exposing (Url)


idAttribute : String -> Attribute msg
idAttribute id =
    Element.htmlAttribute <| Attributes.id id


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        , onUrlRequest = HandleUrlRequest
        , onUrlChange = HandleUrlChange
        }


type BoyGirl
    = Boy
    | Girl


type alias Names =
    { boys : List String
    , girls : List String
    }


emptyNames : Names
emptyNames =
    { boys = []
    , girls = []
    }


getNames : BoyGirl -> Names -> List String
getNames boygirl names =
    case boygirl of
        Boy ->
            names.boys

        Girl ->
            names.girls


setNames : BoyGirl -> List String -> Names -> Names
setNames boygirl nameList names =
    case boygirl of
        Boy ->
            { names | boys = nameList }

        Girl ->
            { names | girls = nameList }


type WhichTable
    = BoyTable
    | GirlTable
    | BothTable


whichTable : BoyGirl -> WhichTable
whichTable boygirl =
    case boygirl of
        Boy ->
            BoyTable

        Girl ->
            GirlTable


type alias Table =
    IdSearch.Table String


type alias Tables =
    { boys : Table
    , girls : Table
    , both : Table
    }


dictCount : Int
dictCount =
    3


makeTable : Table
makeTable =
    IdSearch.makeTable dictCount List.singleton


emptyTables : Tables
emptyTables =
    { boys = makeTable
    , girls = makeTable
    , both = makeTable
    }


getTable : WhichTable -> Tables -> Table
getTable which tables =
    case which of
        BoyTable ->
            tables.boys

        GirlTable ->
            tables.girls

        BothTable ->
            tables.both


setTable : WhichTable -> Table -> Tables -> Tables
setTable which table tables =
    case which of
        BoyTable ->
            { tables | boys = table }

        GirlTable ->
            { tables | girls = table }

        BothTable ->
            { tables | both = table }


type alias Model =
    { text : String
    , selection : Maybe Tracker.Selection
    , complete : String
    , completions : List String
    , completeWhich : WhichTable
    , triggerSelection : Int
    , names : Names
    , tables : Tables
    , receivedOne : Bool
    , error : Maybe Http.Error
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    { text = "I love my @Angel"
    , selection = Nothing
    , complete = ""
    , completions = []
    , completeWhich = BothTable
    , triggerSelection = 0
    , names = emptyNames
    , tables = emptyTables
    , receivedOne = False
    , error = Nothing
    }
        |> withCmds
            [ getNameFile boyNamesFile (ReceiveNames Boy)
            , getNameFile girlNamesFile (ReceiveNames Girl)
            ]


type Msg
    = Noop
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url
    | ReceiveNames BoyGirl (Result Http.Error (List String))
    | ChooseTable WhichTable
    | OnSelection Tracker.Selection
    | TextChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model |> withNoCmd

        HandleUrlRequest request ->
            ( model
            , case request of
                Internal url ->
                    Navigation.load <| Url.toString url

                External urlString ->
                    Navigation.load urlString
            )

        HandleUrlChange _ ->
            model |> withNoCmd

        ReceiveNames boygirl result ->
            receiveNames boygirl result model

        ChooseTable completeWhich ->
            { model | completeWhich = completeWhich } |> withNoCmd

        OnSelection selection ->
            onSelection selection model

        TextChanged text ->
            textChanged text model


textChanged : String -> Model -> ( Model, Cmd Msg )
textChanged text model =
    { model
        | text = text
        , triggerSelection =
            if String.contains "@" text then
                model.triggerSelection + 1

            else
                model.triggerSelection
    }
        |> withNoCmd


onSelection : Tracker.Selection -> Model -> ( Model, Cmd Msg )
onSelection selection mdl =
    let
        cursor =
            selection.selectionEnd

        model =
            { mdl
                | selection = Nothing
                , complete = ""
                , completions = []
            }
    in
    if cursor /= selection.selectionStart then
        model |> withNoCmd

    else
        case findAtName cursor model.text of
            Nothing ->
                model |> withNoCmd

            Just complete ->
                let
                    table =
                        getTable model.completeWhich model.tables

                    completions =
                        IdSearch.lookup complete table
                            |> sortNames complete
                in
                { model
                    | selection = Just selection
                    , complete = complete
                    , completions = completions
                }
                    |> withNoCmd


findAtName : Int -> String -> Maybe String
findAtName cursor string =
    let
        beginning =
            String.left cursor string
                |> String.toLower
                |> String.replace "\n" " "
    in
    if
        String.startsWith "@" beginning
            && not (String.contains " " beginning)
    then
        Just <| String.dropLeft 1 beginning

    else
        let
            res =
                SE.rightOfBack " @" beginning
        in
        if res /= "" && not (String.contains " " res) then
            Just res

        else
            Nothing


sortNames : String -> List String -> List String
sortNames prefix names =
    let
        tuples =
            List.map
                (\name ->
                    ( if String.startsWith prefix name then
                        0

                      else
                        1
                    , name
                    )
                )
                names
    in
    List.sort tuples
        |> List.map Tuple.second


receiveNames : BoyGirl -> Result Http.Error (List String) -> Model -> ( Model, Cmd Msg )
receiveNames boygirl result model =
    case result of
        Ok namelist ->
            let
                which =
                    whichTable boygirl

                tables =
                    model.tables

                lclist =
                    List.map String.toLower namelist

                newTable =
                    IdSearch.insertList lclist <|
                        getTable which tables

                newBoth =
                    IdSearch.insertList lclist <|
                        getTable BothTable tables
            in
            { model
                | names = setNames boygirl namelist model.names
                , receivedOne = True
                , tables =
                    setTable which newTable tables
                        |> setTable BothTable newBoth
            }
                |> withCmds
                    [ if model.receivedOne then
                        -- process initial @Angel
                        Task.perform TextChanged <| Task.succeed model.text

                      else
                        Cmd.none
                    ]

        Err error ->
            let
                err =
                    Debug.log
                        ("Error reading " ++ Debug.toString boygirl ++ " names: ")
                        error
            in
            { model | error = Just err } |> withNoCmd


pageTitle : String
pageTitle =
    "billstclair/elm-id-search Example"


view : Model -> Document Msg
view model =
    { title = pageTitle
    , body =
        [ Element.layout [] <|
            mainPage model
        , Tracker.textAreaTracker
            [ Tracker.textAreaId textAreaId
            , Tracker.triggerSelection model.triggerSelection
            , Tracker.onSelection OnSelection
            ]
            []
        ]
    }


scaled : Int -> Float
scaled =
    Element.modular 16 1.25


fontSize : Int -> Attribute Msg
fontSize scale =
    Font.size (round <| scaled scale)


pad : Int
pad =
    10


textAreaId : String
textAreaId =
    "textarea"


mainPage : Model -> Element Msg
mainPage model =
    column
        [ spacing pad
        , padding pad
        , fontSize 2
        ]
        [ row
            [ fontSize 3
            , Font.bold
            ]
            [ text pageTitle ]
        , paragraph
            []
            [ text <|
                "Type some text below."
                    ++ " Names beginning with \"@\" will show completions."
            ]
        , row []
            [ Input.radioRow [ spacing pad ]
                { onChange = ChooseTable
                , selected = Just model.completeWhich
                , label = Input.labelLeft [] (text "Use names: ")
                , options =
                    [ Input.option BoyTable (text "Boys")
                    , Input.option GirlTable (text "Girls")
                    , Input.option BothTable (text "Both")
                    ]
                }
            ]
        , Input.multiline
            [ width <| px 600
            , height <| px 300
            , idAttribute textAreaId
            ]
            { onChange = TextChanged
            , text = model.text
            , placeholder = Nothing
            , label = Input.labelHidden "Text"
            , spellcheck = True
            }
        , paragraph []
            [ case model.complete of
                "" ->
                    text "Not completing."

                complete ->
                    text <|
                        ("Completing \"" ++ complete ++ "\": ")
                            ++ (String.replace "," ", " <|
                                    Debug.toString model.completions
                               )
            ]
        , row []
            [ text <|
                String.fromInt (List.length model.names.boys)
                    ++ " boy's names."
            , text <|
                String.fromInt (List.length model.names.girls)
                    ++ " girl's names."
            ]
        , row []
            [ link
                [ Font.underline
                , Element.mouseOver [ Font.color blue ]
                ]
                { url = packageUrl
                , label = text "billstclair/elm-id-search"
                }
            ]
        ]


blue : Color
blue =
    Element.rgb 0 0 1


packageUrl : String
packageUrl =
    "http://package.elm-lang.org/packages/billstclair/elm-id-search/latest"



---
--- Read and parse the names files
---


namesDir : String
namesDir =
    "names/"


boyNamesFile : String
boyNamesFile =
    namesDir ++ "boy-names.json"


girlNamesFile : String
girlNamesFile =
    namesDir ++ "girl-names.json"


getNameFile : String -> (Result Http.Error (List String) -> Msg) -> Cmd Msg
getNameFile file tagger =
    Http.get
        { url = file
        , expect = Http.expectJson tagger decodeNames
        }


decodeNames : Decoder (List String)
decodeNames =
    JD.field "names" <| JD.list JD.string
