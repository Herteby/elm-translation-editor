port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Dict.Extra as Dict
import Elm.CodeGen as CodeGen
import Elm.Parser
import Elm.Pretty as Pretty
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), FunctionImplementation)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import File exposing (File)
import File.Download as Download
import File.Select
import Html exposing (..)
import Html.Attributes exposing (attribute, autofocus, class, classList, disabled, placeholder, style, title, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
import Regex exposing (Regex)
import Result.Extra as Result
import Set exposing (Set)
import Task


port copyToClipboard : String -> Cmd msg


type Model
    = Start String (Maybe { name : String, languages : String })
    | Editor EditorModel


type alias EditorModel =
    { modal : Maybe Modal
    , search : String
    , languages : Dict Language Bool
    , moduleName : ModuleName
    , definitions : List Definition
    , output : Output
    }


type Output
    = Elm
    | I18next


type Modal
    = Preview
    | Confirm Msg String
    | New NewDefinition


type alias NewDefinition =
    { name : String
    , arguments : String
    , choices : String
    }


type alias Definition =
    { name : String
    , translation : Translation
    , complete : Bool
    , invalidTemplate : Bool
    }


type Translation
    = Basic (Dict Language String)
    | Choice (Dict Language (Dict ChoiceKey String))
    | Template (List Argument) (Dict Language TemplateBody)
    | TemplateChoice (List Argument) (Dict Language (Dict ChoiceKey TemplateBody))


type alias TemplateBody =
    { string : String
    , expressions : Maybe (List CodeGen.Expression)
    }


type alias Argument =
    String


type alias Language =
    String


type alias ChoiceKey =
    String


type Msg
    = SetCode String
    | SelectFile
    | OpenNewFile
    | CancelNewFile
    | NewFileName String
    | NewFileLanguages String
    | SaveNewFile { languages : Dict String Bool, moduleName : ModuleName }
    | GotFile File
    | AddExampleCode
      ------- Editor -------
    | StartEdit ( List String, List Definition )
    | SetTranslation Int (Maybe ChoiceKey) Language String
    | ClickedBack
    | ConfirmedBack
    | ClickedDownload
    | ConfirmedDownloadIncomplete
    | ClickedCopyFileToClipboard
    | ConfirmedCopyFileToClipboardIncomplete
    | CopyToClipboard String
    | ShowPreview
    | SetSearch String
    | ToggleLanguage Language
    | ToggleOutput
    | OpenNewDefinition
    | NewDefinitionName String
    | NewDefinitionArguments String
    | NewDefinitionChoices String
    | SaveNewDefinition Definition
    | CloseModal


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Start "" Nothing
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Start _ _, SelectFile ) ->
            ( model, File.Select.file [] GotFile )

        ( Start _ _, GotFile file ) ->
            ( model, Task.perform SetCode (File.toString file) )

        ( Start code m, OpenNewFile ) ->
            ( Start code (Just { name = "", languages = "" }), Cmd.none )

        ( Start code m, CancelNewFile ) ->
            ( Start code Nothing, Cmd.none )

        ( Start code newFile, NewFileName string ) ->
            ( Start code (Maybe.map (\n -> { n | name = string }) newFile), Cmd.none )

        ( Start code newFile, NewFileLanguages string ) ->
            ( Start code (Maybe.map (\n -> { n | languages = string }) newFile), Cmd.none )

        ( Start _ _, SaveNewFile file ) ->
            ( Editor
                { modal = Nothing
                , search = ""
                , languages = file.languages
                , moduleName = file.moduleName
                , definitions = []
                , output = Elm
                }
            , Cmd.none
            )

        ( Start code _, CloseModal ) ->
            ( Start code Nothing, Cmd.none )

        ( Start _ m, SetCode str ) ->
            ( Start str m, Cmd.none )

        ( Start _ m, AddExampleCode ) ->
            ( Start exampleCode m, Cmd.none )

        ( Start _ _, StartEdit ( moduleName, definitions ) ) ->
            ( Editor
                { modal = Nothing
                , search = ""
                , languages = getLanguages definitions
                , moduleName = moduleName
                , definitions = definitions
                , output = Elm
                }
            , Cmd.none
            )

        ------- Editor -------
        ( Editor em, SetTranslation index choiceKey language string ) ->
            ( Editor
                { em
                    | definitions =
                        em.definitions
                            |> List.indexedMap
                                (\i def ->
                                    if i == index then
                                        case ( def.translation, choiceKey ) of
                                            ( Basic languages, Nothing ) ->
                                                { def
                                                    | translation = Basic (Dict.insert language string languages)
                                                }

                                            ( Choice languages, Just key ) ->
                                                { def
                                                    | translation = Choice (Dict.update language (Maybe.map (Dict.insert key string)) languages)
                                                }

                                            ( Template args languages, Nothing ) ->
                                                { def
                                                    | translation =
                                                        Template args
                                                            (Dict.insert language
                                                                { string = string
                                                                , expressions = templateToExpressions args string
                                                                }
                                                                languages
                                                            )
                                                }

                                            ( TemplateChoice args languages, Just key ) ->
                                                { def
                                                    | translation =
                                                        TemplateChoice args
                                                            (Dict.update language
                                                                (Maybe.map
                                                                    (Dict.insert key
                                                                        { string = string
                                                                        , expressions = templateToExpressions args string
                                                                        }
                                                                    )
                                                                )
                                                                languages
                                                            )
                                                }

                                            _ ->
                                                def

                                    else
                                        def
                                )
                            |> List.indexedMap
                                (\i d ->
                                    if i == index then
                                        { d
                                            | complete = translationIsComplete d.translation
                                            , invalidTemplate = invalidTemplate d.translation
                                        }

                                    else
                                        d
                                )
                }
            , Cmd.none
            )

        ( Editor em, ClickedBack ) ->
            ( Editor { em | modal = Just (Confirm ConfirmedBack "Are you sure you want to go back? Your work will be lost.") }, Cmd.none )

        ( Editor _, ConfirmedBack ) ->
            ( Start "" Nothing, Cmd.none )

        ( Editor em, ClickedDownload ) ->
            if List.all (.translation >> translationIsComplete) em.definitions then
                ( model, download em )

            else
                ( Editor { em | modal = Just (Confirm ConfirmedDownloadIncomplete "Some of the translations are still blank. Are you sure you are done?") }, Cmd.none )

        ( Editor em, ConfirmedDownloadIncomplete ) ->
            ( model, download em )

        ( Editor em, ClickedCopyFileToClipboard ) ->
            if List.all (.translation >> translationIsComplete) em.definitions then
                ( model, print em.moduleName em.definitions |> copyToClipboard )

            else
                ( Editor { em | modal = Just (Confirm ConfirmedCopyFileToClipboardIncomplete "Some of the translations are still blank. Are you sure you are done?") }, Cmd.none )

        ( Editor em, ConfirmedCopyFileToClipboardIncomplete ) ->
            ( model, print em.moduleName em.definitions |> copyToClipboard )

        ( Editor _, CopyToClipboard string ) ->
            ( model, copyToClipboard string )

        ( Editor em, ShowPreview ) ->
            ( Editor { em | modal = Just Preview }, Cmd.none )

        ( Editor em, SetSearch string ) ->
            ( Editor { em | search = string }, Cmd.none )

        ( Editor em, ToggleLanguage language ) ->
            ( Editor { em | languages = Dict.update language (Maybe.map not) em.languages }, Cmd.none )

        ( Editor em, ToggleOutput ) ->
            ( Editor
                { em
                    | output =
                        case em.output of
                            Elm ->
                                I18next

                            I18next ->
                                Elm
                }
            , Cmd.none
            )

        ( Editor em, OpenNewDefinition ) ->
            ( Editor { em | modal = Just (New <| NewDefinition "" "" "") }, Cmd.none )

        ( Editor em, NewDefinitionName string ) ->
            updateNewDefinition em (\n -> { n | name = string })

        ( Editor em, NewDefinitionArguments string ) ->
            updateNewDefinition em (\n -> { n | arguments = string })

        ( Editor em, NewDefinitionChoices string ) ->
            updateNewDefinition em (\n -> { n | choices = string })

        ( Editor em, SaveNewDefinition definition ) ->
            ( Editor { em | modal = Nothing, definitions = definition :: em.definitions }, Cmd.none )

        ( Editor em, CloseModal ) ->
            ( Editor { em | modal = Nothing }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateNewDefinition : EditorModel -> (NewDefinition -> NewDefinition) -> ( Model, Cmd Msg )
updateNewDefinition em fn =
    case em.modal of
        Just (New newDefinition) ->
            ( Editor { em | modal = Just (New (fn newDefinition)) }, Cmd.none )

        _ ->
            ( Editor em, Cmd.none )


download : EditorModel -> Cmd msg
download em =
    case em.output of
        Elm ->
            Download.string (elmFileName em.moduleName) "text/plain" (print em.moduleName em.definitions)

        I18next ->
            let
                name =
                    em.moduleName |> List.reverse |> List.head |> Maybe.withDefault "i18n"
            in
            em.definitions
                |> toI18next
                |> jsonToStrings em.moduleName
                |> List.map
                    (\( fileName, content ) ->
                        Download.string fileName "text/plain" content
                    )
                |> Cmd.batch


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ header [] [ div [ class "container" ] [ text "Elm Translation Editor" ] ]
        , main_ [] <|
            case model of
                Start code newFile ->
                    [ Maybe.unwrap (text "") newFileModal newFile
                    , row 10
                        []
                        [ button "Upload a translation file" (Just SelectFile)
                        , secondary "Create a new file" (Just OpenNewFile)
                        , spacer
                        , secondary "click here to insert example code" (Just AddExampleCode)
                        ]
                    , textarea
                        [ value code
                        , placeholder "...or paste the contents here"
                        , onInput SetCode
                        , autofocus True
                        ]
                        []
                    , code
                        |> translationsFromCode
                        |> Result.toMaybe
                        |> Maybe.map StartEdit
                        |> button "Edit translations"
                    , case translationsFromCode code of
                        Err err ->
                            div [ class "error" ] [ text err ]

                        Ok _ ->
                            text ""
                    ]

                Editor em ->
                    let
                        ifValid msg =
                            if List.any .invalidTemplate em.definitions then
                                Nothing

                            else
                                Just msg

                        definitions =
                            if em.search == "" then
                                em.definitions

                            else
                                List.filter (matchDefiniton em.search) em.definitions
                    in
                    [ row 10
                        []
                        [ button "+ New Definition" (Just OpenNewDefinition)
                        , languageFilter em.languages
                        , spacer
                        , input
                            [ value em.search
                            , onInput SetSearch
                            , placeholder "Search translations"
                            ]
                            []
                        ]
                    , Lazy.lazy2 editor em.languages definitions
                    , Maybe.unwrap (text "") (editorModals em) em.modal
                    , footer []
                        [ div [ class "container" ]
                            [ button "Back to file import" (Just ClickedBack)
                            , div [ class "footerButtons" ]
                                [ secondary
                                    (case em.output of
                                        Elm ->
                                            "Output: Elm"

                                        I18next ->
                                            "Output: i18next"
                                    )
                                    (Just ToggleOutput)
                                , secondary "Preview" (Just ShowPreview)
                                , button "Download file" (ifValid ClickedDownload)
                                , button "Copy content to clipboard" (ifValid ClickedCopyFileToClipboard |> Maybe.filter (\_ -> em.output == Elm))
                                ]
                            ]
                        ]
                    ]
        ]


modal : msg -> Html msg -> Html msg
modal close content =
    div [ class "modalContainer" ]
        [ div [ class "overlay", onClick close ] []
        , div [ class "modal" ]
            [ content ]
        ]


newFileModal : { name : String, languages : String } -> Html Msg
newFileModal newFile =
    let
        file =
            createFile newFile.name newFile.languages

        invalid getter =
            case file of
                Ok _ ->
                    class ""

                Err err ->
                    if Maybe.isJust (getter err) then
                        class "invalid"

                    else
                        class ""
    in
    modal CancelNewFile <|
        div [ class "newFile" ]
            [ h3 [] [ text "New file" ]
            , label [ invalid .name ]
                [ span [] [ text "Name" ]
                , input [ value newFile.name, onInput NewFileName ] []
                ]
            , showError .name file
            , label [ invalid .languages ]
                [ span [] [ text "Language codes" ]
                , input [ value newFile.languages, onInput NewFileLanguages ] []
                ]
            , div [] [ text "(Separated by spaces)" ]
            , showError .languages file
            , div [ class "modalButtons" ]
                [ button "Save" (file |> Result.toMaybe |> Maybe.map SaveNewFile)
                , button "Cancel" (Just CloseModal)
                ]
            ]


createFile : String -> String -> Result { name : Maybe String, languages : Maybe String } { languages : Dict Language Bool, moduleName : ModuleName }
createFile name languages =
    let
        langs =
            split languages

        errors =
            { name =
                first
                    [ ( name == "", "Name required" )
                    , ( invalidChar name, "Name contains an invalid character" )
                    , ( name
                            |> String.toList
                            |> List.head
                            |> Maybe.unwrap False Char.isLower
                      , "The first letter must be uppercase"
                      )
                    ]
            , languages =
                first
                    [ ( langs == [], "At least one language required" )
                    , ( List.any invalidChar langs, "A language code contains an invalid character" )
                    , ( List.any containsUppercase langs, "Language codes must be lowercase" )
                    ]
            }
    in
    if List.any Maybe.isJust [ errors.name, errors.languages ] then
        Err errors

    else
        Ok
            { languages = langs |> List.map (\l -> ( l, True )) |> Dict.fromList
            , moduleName = [ name ]
            }


editorModals : EditorModel -> Modal -> Html Msg
editorModals em m =
    modal CloseModal <|
        case m of
            Preview ->
                case em.output of
                    Elm ->
                        div []
                            [ h1 [] [ text <| elmFileName em.moduleName ]
                            , pre [] [ text <| print em.moduleName em.definitions ]
                            ]

                    I18next ->
                        em.definitions
                            |> toI18next
                            |> jsonToStrings em.moduleName
                            |> List.concatMap
                                (\( lang, json ) ->
                                    [ h1 [] [ text lang ], pre [] [ text json ] ]
                                )
                            |> div []

            Confirm msg string ->
                div []
                    [ text string
                    , div [ class "modalButtons" ]
                        [ button "Yes" (Just msg)
                        , button "No" (Just CloseModal)
                        ]
                    ]

            New def ->
                let
                    names =
                        List.map (.name >> String.toLower) em.definitions

                    newDef =
                        createDefinition names em.languages def

                    invalid getter =
                        case newDef of
                            Ok _ ->
                                class ""

                            Err err ->
                                if Maybe.isJust (getter err) then
                                    class "invalid"

                                else
                                    class ""
                in
                div [ class "newDef" ]
                    [ h3 [] [ text "New definition" ]
                    , label [ invalid .name ]
                        [ span [] [ text "Name" ]
                        , input [ value def.name, onInput NewDefinitionName ] []
                        ]
                    , label [ invalid .arguments ]
                        [ span [] [ text "Variables" ]
                        , input [ value def.arguments, onInput NewDefinitionArguments ] []
                        ]
                    , label [ invalid .choices ]
                        [ span [] [ text "Choices" ]
                        , input [ value def.choices, onInput NewDefinitionChoices ] []
                        ]
                    , h3 [ class "previewHeader" ] [ text "Preview" ]
                    , div [ class "previewNewDef" ]
                        [ case newDef of
                            Ok nd ->
                                editorCard em.languages 0 nd

                            Err err ->
                                Maybe.values [ err.name, err.arguments, err.choices ]
                                    |> List.head
                                    |> Maybe.withDefault "Something went wrong"
                                    |> text
                        ]
                    , div [ class "modalButtons" ]
                        [ button "Save" (newDef |> Result.toMaybe |> Maybe.map SaveNewDefinition)
                        , button "Cancel" (Just CloseModal)
                        ]
                    ]


createDefinition : List String -> Dict Language Bool -> NewDefinition -> Result { name : Maybe String, arguments : Maybe String, choices : Maybe String } Definition
createDefinition names languages def =
    let
        choices =
            split def.choices

        arguments =
            split def.arguments

        errors =
            { name =
                first
                    [ ( def.name == "", "Name required" )
                    , ( invalidChar def.name, "Name contains an invalid character" )
                    , ( List.member (String.toLower def.name) names, "A definition with that name already exists" )
                    ]
            , arguments =
                first
                    [ ( List.any invalidChar arguments, "A variable name contains an invalid character" )
                    , ( List.length arguments /= List.length (List.unique arguments), "Variable names must be unique" )
                    , ( List.any containsUppercase arguments, "Variable names must be lowercase" )
                    ]
            , choices =
                first
                    [ ( List.any invalidChar arguments, "A choice contains an invalid character" )
                    , ( List.length choices == 1, "Definition must have zero, two, or more choices" )
                    , ( List.length choices /= List.length (List.unique choices), "Definition must not contain duplicate choices" )
                    , ( List.any containsUppercase arguments, "Choices must be lowercase" )
                    ]
            }
    in
    if List.any Maybe.isJust [ errors.name, errors.arguments, errors.choices ] then
        Err errors

    else
        Ok
            { name = def.name
            , complete = False
            , invalidTemplate = False
            , translation =
                case ( split def.arguments, choices ) of
                    ( [], [] ) ->
                        Basic (Dict.map (\_ _ -> "") languages)

                    ( [], _ ) ->
                        Choice (languages |> Dict.map (\_ _ -> choices |> List.map (\key -> ( key, "" )) |> Dict.fromList))

                    ( _, [] ) ->
                        Template arguments (Dict.map (\_ _ -> { expressions = Nothing, string = "" }) languages)

                    ( _, _ ) ->
                        TemplateChoice arguments (languages |> Dict.map (\_ _ -> choices |> List.map (\key -> ( key, { expressions = Nothing, string = "" } )) |> Dict.fromList))
            }


containsUppercase : String -> Bool
containsUppercase =
    String.toList >> List.any Char.isUpper


invalidChar : String -> Bool
invalidChar =
    Regex.contains (Regex.fromString "[^a-zA-Z_]" |> Maybe.withDefault Regex.never)


showError : (a -> Maybe String) -> Result a b -> Html msg
showError getter =
    Result.error >> Maybe.andThen getter >> Maybe.withDefault "" >> text


split : String -> List String
split str =
    String.split "," str
        |> List.concatMap (String.split " ")
        |> List.filter ((/=) "")


first : List ( Bool, a ) -> Maybe a
first =
    List.filter Tuple.first >> List.map Tuple.second >> List.head


matchDefiniton : String -> Definition -> Bool
matchDefiniton search { translation } =
    case translation of
        Basic languages ->
            Dict.any (\_ str -> containsInsensitive search str) languages

        Choice languages ->
            Dict.any
                (\_ choices ->
                    Dict.any (\_ str -> containsInsensitive search str) choices
                )
                languages

        Template _ languages ->
            Dict.any (\_ template -> containsInsensitive search template.string) languages

        TemplateChoice _ languages ->
            Dict.any
                (\_ choices ->
                    Dict.any (\_ template -> containsInsensitive search template.string) choices
                )
                languages


containsInsensitive : String -> String -> Bool
containsInsensitive str1 str2 =
    String.contains (String.toLower str1) (String.toLower str2)


button : String -> Maybe msg -> Html msg
button string maybeMsg =
    Html.button
        [ case maybeMsg of
            Just msg ->
                onClick msg

            Nothing ->
                disabled True
        ]
        [ text string ]


secondary : String -> Maybe msg -> Html msg
secondary string maybeMsg =
    Html.button
        [ case maybeMsg of
            Just msg ->
                onClick msg

            Nothing ->
                disabled True
        , class "secondary"
        ]
        [ text string ]


languageFilter : Dict Language Bool -> Html Msg
languageFilter languages =
    div [ class "languageFilter" ] <|
        span [] [ text "Languages:" ]
            :: (languages
                    |> Dict.toList
                    |> List.map
                        (\( language, enabled ) ->
                            div
                                [ classList [ ( "disabled", not enabled ) ]
                                , onClick (ToggleLanguage language)
                                ]
                                [ text language ]
                        )
               )


editor : Dict Language Bool -> List Definition -> Html Msg
editor languages translations =
    div [] <| List.indexedMap (\i def -> Lazy.lazy3 editorCard languages i def) translations


editorCard : Dict Language Bool -> Int -> Definition -> Html Msg
editorCard langs index { name, translation, complete } =
    let
        shownLanguages =
            langs
                |> Dict.toList
                |> List.filter Tuple.second
                |> List.map Tuple.first
                |> Set.fromList

        filterAndMap : Dict Language v -> (( Language, v ) -> b) -> List b
        filterAndMap languages f =
            Dict.filter (\k _ -> Set.member k shownLanguages) languages
                |> Dict.toList
                |> List.map f
    in
    div [ class "card" ] <|
        div []
            [ text name ]
            :: (case translation of
                    Basic languages ->
                        filterAndMap languages
                            (\( language, string ) ->
                                textInput language string False (SetTranslation index Nothing)
                            )

                    Choice languages ->
                        filterAndMap languages
                            (\( language, choices ) ->
                                choiceInput language (Dict.map (\_ c -> ( c, False )) choices) (SetTranslation index)
                            )

                    Template placeholders languages ->
                        viewPlaceholders placeholders
                            :: filterAndMap languages
                                (\( language, { string, expressions } ) ->
                                    textInput language string (expressions == Nothing) (SetTranslation index Nothing)
                                )

                    TemplateChoice placeholders languages ->
                        viewPlaceholders placeholders
                            :: filterAndMap languages
                                (\( language, choices ) ->
                                    choiceInput language
                                        (Dict.map (\_ { string, expressions } -> ( string, expressions == Nothing )) choices)
                                        (SetTranslation index)
                                )
               )


viewPlaceholders : List String -> Html Msg
viewPlaceholders placeholders =
    div [ class "placeholders" ]
        (List.map
            (\p ->
                let
                    withGullWings =
                        "{" ++ p ++ "}"
                in
                div
                    [ onClick (CopyToClipboard withGullWings)
                    , title <| "click to copy " ++ withGullWings ++ " to your clipboard"
                    ]
                    [ text withGullWings ]
            )
            placeholders
        )


textInput : String -> String -> Bool -> (Language -> String -> msg) -> Html msg
textInput language string invalid msg =
    label [ classList [ ( "invalid", invalid || string == "" ) ] ]
        [ span [] [ text language ]
        , autoExpand string (invalid || string == "") (msg language)
        ]


choiceInput : String -> Dict ChoiceKey ( String, Bool ) -> (Maybe ChoiceKey -> Language -> String -> msg) -> Html msg
choiceInput language choices msg =
    label [ classList [ ( "invalid", Dict.values choices |> List.any (\( str, invalid ) -> invalid || str == "") ) ] ]
        [ span [] [ text language ]
        , div [ class "choices" ]
            (Dict.toList choices
                |> List.concatMap
                    (\( choiceKey, ( string, invalid ) ) ->
                        [ span [ classList [ ( "invalid", invalid || string == "" ) ] ] [ text choiceKey ]
                        , autoExpand string (invalid || string == "") (msg (Just choiceKey) language)
                        ]
                    )
            )
        ]


autoExpand : String -> Bool -> (String -> msg) -> Html msg
autoExpand string invalid msg =
    div [ classList [ ( "autoexpand", True ), ( "invalid", invalid ) ] ]
        [ textarea [ onInput msg, value string ] []
        , div [] [ text (string ++ "_") ]
        ]


invalidTemplate : Translation -> Bool
invalidTemplate translation =
    case translation of
        Basic languages ->
            False

        Choice languages ->
            False

        Template args languages ->
            Dict.values languages |> List.any (\{ string, expressions } -> expressions == Nothing && string /= "")

        TemplateChoice args languages ->
            Dict.values languages
                |> List.concatMap Dict.values
                |> List.any (\{ string, expressions } -> expressions == Nothing && string /= "")


translationIsComplete : Translation -> Bool
translationIsComplete translation =
    case translation of
        Basic languages ->
            Dict.values languages |> List.all ((/=) "")

        Choice languages ->
            Dict.values languages |> List.concatMap Dict.values |> List.all ((/=) "")

        Template args languages ->
            Dict.values languages |> List.all (\{ string, expressions } -> expressions /= Nothing && string /= "")

        TemplateChoice args languages ->
            Dict.values languages |> List.concatMap Dict.values |> List.all (\{ string, expressions } -> expressions /= Nothing && string /= "")


getLanguages : List Definition -> Dict Language Bool
getLanguages =
    List.foldl
        (\def ->
            let
                languages =
                    case def.translation of
                        Basic dict ->
                            Dict.map (\_ _ -> True) dict

                        Choice dict ->
                            Dict.map (\_ _ -> True) dict

                        Template _ dict ->
                            Dict.map (\_ _ -> True) dict

                        TemplateChoice _ dict ->
                            Dict.map (\_ _ -> True) dict
            in
            Dict.union languages
        )
        Dict.empty


translationsFromCode : String -> Result String ( List String, List Definition )
translationsFromCode str =
    if str == "" then
        Err ""

    else
        str
            |> Elm.Parser.parse
            |> Result.mapError (always "Syntax Error")
            |> Result.map (Elm.Processing.process Elm.Processing.init)
            |> Result.andThen
                (\file ->
                    let
                        moduleDef =
                            case file.moduleDefinition of
                                Node _ (NormalModule moduleData) ->
                                    Ok <| Node.value moduleData.moduleName

                                _ ->
                                    Err "Unsupported module type"

                        definitions =
                            file.declarations
                                |> List.map fromAst
                                |> Result.combine
                                |> Result.map
                                    (List.sortBy
                                        (\d ->
                                            ( if d.complete then
                                                1

                                              else
                                                0
                                            , d.name
                                            )
                                        )
                                    )
                    in
                    Result.map2 Tuple.pair moduleDef definitions
                )


fromAst : Node Declaration -> Result String Definition
fromAst (Node range d) =
    (case d of
        FunctionDeclaration { declaration, documentation } ->
            Node.value declaration
                |> Maybe.oneOf
                    [ getBasic
                    , getTemplate
                    , getChoice
                    , getTemplateChoice
                    ]
                |> Maybe.map
                    (\translation ->
                        { name = Node.value declaration |> .name |> Node.value
                        , translation = translation
                        , complete = translationIsComplete translation
                        , invalidTemplate = invalidTemplate translation
                        }
                    )

        _ ->
            Nothing
    )
        |> Result.fromMaybe ("Unsupported translation definition on line: " ++ String.fromInt range.start.row)


getBasic : FunctionImplementation -> Maybe Translation
getBasic { arguments, expression } =
    case ( arguments, expression ) of
        ( [], Node _ (RecordExpr languages) ) ->
            languages
                |> List.map
                    (\(Node _ ( Node _ language, Node _ expression2 )) ->
                        case expression2 of
                            Literal string ->
                                Just ( language, string )

                            _ ->
                                Nothing
                    )
                |> Maybe.combine
                |> Maybe.map
                    (\pairs ->
                        Basic (Dict.fromList pairs)
                    )

        _ ->
            Nothing


getTemplateChoice : FunctionImplementation -> Maybe Translation
getTemplateChoice { arguments, expression } =
    case ( arguments, expression ) of
        ( _ :: _, Node _ (RecordExpr languages) ) ->
            languages
                |> List.map
                    (\(Node _ ( Node _ language, Node _ expression2 )) ->
                        case expression2 of
                            RecordExpr choices ->
                                choices
                                    |> List.map
                                        (\(Node _ ( Node _ choiceKey, Node _ expression3 )) ->
                                            case expression3 of
                                                ListExpr nodes ->
                                                    getTemplateBody nodes
                                                        |> Maybe.map (\list -> ( choiceKey, list ))

                                                _ ->
                                                    Nothing
                                        )
                                    |> Maybe.combine
                                    |> Maybe.map
                                        (\pairs ->
                                            ( language, Dict.fromList pairs )
                                        )

                            _ ->
                                Nothing
                    )
                |> Maybe.combine
                |> Maybe.map2
                    (\args pairs ->
                        TemplateChoice args (Dict.fromList pairs)
                    )
                    (List.map
                        (\a ->
                            case a of
                                Node _ (VarPattern str) ->
                                    Just str

                                _ ->
                                    Nothing
                        )
                        arguments
                        |> Maybe.combine
                    )

        _ ->
            Nothing


getTemplateBody : List (Node Expression) -> Maybe TemplateBody
getTemplateBody =
    List.map
        (\(Node _ item) ->
            case item of
                Literal str ->
                    Just ( str, CodeGen.string str )

                FunctionOrValue [] str ->
                    Just ( "{" ++ str ++ "}", CodeGen.val str )

                _ ->
                    Nothing
        )
        >> Maybe.combine
        >> Maybe.map
            (\list ->
                { string = list |> List.map Tuple.first |> String.join ""
                , expressions = Just <| List.map Tuple.second list
                }
            )


templateToExpressions : List String -> String -> Maybe (List CodeGen.Expression)
templateToExpressions args templateBody =
    let
        matches =
            Regex.find rgx templateBody
                |> List.map
                    (.match
                        >> String.dropLeft 1
                        >> String.dropRight 1
                    )

        vals =
            List.map CodeGen.val matches

        strings =
            Regex.split rgx templateBody

        gullWings =
            (String.join "" strings |> String.contains "{")
                || (String.join "" strings |> String.contains "}")
    in
    if Set.fromList args == Set.fromList matches && not gullWings then
        Just <| List.interweave (List.map CodeGen.string strings) vals

    else
        Nothing


rgx : Regex
rgx =
    Regex.fromString "{.*?}" |> Maybe.withDefault Regex.never


getChoice : FunctionImplementation -> Maybe Translation
getChoice { arguments, expression } =
    case ( arguments, expression ) of
        ( [], Node _ (RecordExpr languages) ) ->
            languages
                |> List.map
                    (\(Node _ ( Node _ language, Node _ expression2 )) ->
                        case expression2 of
                            RecordExpr choices ->
                                choices
                                    |> List.map
                                        (\(Node _ ( Node _ choiceKey, Node _ expression3 )) ->
                                            case expression3 of
                                                Literal string ->
                                                    Just ( choiceKey, string )

                                                _ ->
                                                    Nothing
                                        )
                                    |> Maybe.combine
                                    |> Maybe.map
                                        (\pairs ->
                                            ( language, Dict.fromList pairs )
                                        )

                            _ ->
                                Nothing
                    )
                |> Maybe.combine
                |> Maybe.map
                    (\pairs ->
                        Choice (Dict.fromList pairs)
                    )

        _ ->
            Nothing


getTemplate : FunctionImplementation -> Maybe Translation
getTemplate { arguments, expression, name } =
    case ( arguments, expression, name ) of
        ( _ :: _, Node _ (RecordExpr languages), Node _ name2 ) ->
            languages
                |> List.map
                    (\(Node _ ( Node _ language, Node _ expression2 )) ->
                        case expression2 of
                            ListExpr nodes ->
                                getTemplateBody nodes
                                    |> Maybe.map (\list -> ( language, list ))

                            _ ->
                                Nothing
                    )
                |> Maybe.combine
                |> Maybe.map2
                    (\args pairs ->
                        Template args (Dict.fromList pairs)
                    )
                    (List.map
                        (\a ->
                            case a of
                                Node _ (VarPattern str) ->
                                    Just str

                                _ ->
                                    Nothing
                        )
                        arguments
                        |> Maybe.combine
                    )

        _ ->
            Nothing


print : ModuleName -> List Definition -> String
print moduleName definitions =
    CodeGen.file
        (CodeGen.normalModule moduleName
            (List.map
                (.name >> CodeGen.funExpose)
                definitions
            )
        )
        []
        (definitions |> List.sortBy .name |> List.map toDeclaration)
        Nothing
        |> Pretty.pretty 100


toDeclaration : Definition -> CodeGen.Declaration
toDeclaration { name, translation, complete } =
    case translation of
        Basic languages ->
            CodeGen.valDecl Nothing
                Nothing
                name
                (CodeGen.record
                    (languages
                        |> Dict.toList
                        |> List.map
                            (\( language, string ) ->
                                ( language, CodeGen.string string )
                            )
                    )
                )

        Template args languages ->
            CodeGen.funDecl Nothing
                Nothing
                name
                (List.map CodeGen.varPattern args)
                (CodeGen.record
                    (languages
                        |> Dict.toList
                        |> List.map
                            (\( language, { expressions } ) ->
                                ( language
                                , CodeGen.list <| Maybe.withDefault [] expressions
                                )
                            )
                    )
                )

        Choice languages ->
            CodeGen.valDecl Nothing
                Nothing
                name
                (languages
                    |> Dict.toList
                    |> List.map
                        (\( language, choices ) ->
                            ( language
                            , choices
                                |> Dict.toList
                                |> List.map (\( choiceKey, string ) -> ( choiceKey, CodeGen.string string ))
                                |> CodeGen.record
                            )
                        )
                    |> CodeGen.record
                )

        TemplateChoice args languages ->
            CodeGen.funDecl Nothing
                Nothing
                name
                (List.map CodeGen.varPattern args)
                (CodeGen.record
                    (languages
                        |> Dict.toList
                        |> List.map
                            (\( language, choices ) ->
                                ( language
                                , choices
                                    |> Dict.toList
                                    |> List.map
                                        (\( choiceKey, { expressions } ) ->
                                            ( choiceKey
                                            , CodeGen.list <| Maybe.withDefault [] expressions
                                            )
                                        )
                                    |> CodeGen.record
                                )
                            )
                    )
                )


toI18next : List Definition -> Dict Language Value
toI18next definitions =
    let
        doubleGullWings =
            String.replace "{" "{{" >> String.replace "}" "}}"

        languageDicts =
            List.foldl
                (\def dict ->
                    let
                        translation : Dict Language (Dict String String)
                        translation =
                            case def.translation of
                                Basic languages ->
                                    Dict.map (\lang -> Dict.singleton def.name) languages

                                Choice languages ->
                                    Dict.map
                                        (\lang ->
                                            Dict.mapKeys
                                                (\choiceKey ->
                                                    if choiceKey == "singular" then
                                                        def.name

                                                    else
                                                        def.name ++ "_" ++ choiceKey
                                                )
                                        )
                                        languages

                                Template _ languages ->
                                    Dict.map (\lang body -> Dict.singleton def.name (doubleGullWings body.string)) languages

                                TemplateChoice _ languages ->
                                    Dict.map
                                        (\lang ->
                                            Dict.mapKeys
                                                (\choiceKey ->
                                                    if choiceKey == "singular" then
                                                        def.name

                                                    else
                                                        def.name ++ "_" ++ choiceKey
                                                )
                                                >> Dict.map (\k body -> doubleGullWings body.string)
                                        )
                                        languages
                    in
                    Dict.merge
                        Dict.insert
                        (\key a b -> Dict.insert key (Dict.union a b))
                        Dict.insert
                        dict
                        translation
                        Dict.empty
                )
                Dict.empty
                definitions
    in
    Dict.map
        (\lang dict ->
            dict |> Dict.toList |> List.map (Tuple.mapSecond Encode.string) |> Encode.object
        )
        languageDicts


jsonToStrings : ModuleName -> Dict Language Value -> List ( Language, String )
jsonToStrings moduleName =
    Dict.toList >> List.map (\( lang, json ) -> ( jsonFileName lang moduleName, Encode.encode 2 json ))


elmFileName : ModuleName -> String
elmFileName =
    List.reverse
        >> List.head
        >> Maybe.withDefault "Translations"
        >> (\n -> n ++ ".elm")


jsonFileName : Language -> ModuleName -> String
jsonFileName language =
    List.reverse
        >> List.head
        >> Maybe.withDefault "i18n"
        >> (\n -> n ++ "." ++ language ++ ".json")


exampleCode =
    """module Translations exposing (..)
       

welcome =
   { dk = "Velkommen"
   , en = "Welcome"
   , sv = "Välkommen"
   }


apples =
   { dk = { singular = "æble", plural = "æbler" }
   , en = { singular = "apple", plural = "apples" }
   , sv = { singular = "äpple", plural = "äpplen" }
   }
   

hi name =
   { dk = [ "Hej ", name, "!" ]
   , en = [ "Hi ", name, "!" ]
   , sv = [ "Hej ", name, "!" ]
   }
   
   
thereAreNCases n =
   { sv = { singular = [ "Det finns ", n, " ärende" ], plural = [ "Det finns ", n, " ärenden" ] }
   , dk = { singular = [], plural = [] }
   , en = { singular = [ "There is ", n, " case" ], plural = [ "There are ", n, " cases" ] }
   }

"""


mockJson =
    """
{
  "apples": "äpple",
  "apples_plural": "äpplen",
  "hi": "Hej {{name}}!",
  "thereAreNCases": "Det finns {{n}} ärende",
  "thereAreNCases_plural": "Det finns {{n}} ärenden",
  "welcome": "Välkommen"
}
"""


definitionsDecoder : Language -> Decoder (List Definition)
definitionsDecoder language =
    let
        findArgs str =
            Regex.find (Regex.fromString "{.*?}" |> Maybe.withDefault Regex.never) str
                |> List.map .match
                |> List.unique
                |> List.map (String.dropLeft 1 >> String.dropRight 1)
    in
    Decode.keyValuePairs Decode.string
        |> Decode.map
            (List.sortBy Tuple.first
                >> List.groupWhile (\( a, _ ) ( b, _ ) -> String.startsWith (a ++ "_") b)
                >> List.map
                    (\( head, tail ) ->
                        let
                            name =
                                Tuple.first head

                            toDrop =
                                String.length name + 1

                            choices =
                                head
                                    :: tail
                                    |> List.map
                                        (\( choiceKey, string ) ->
                                            ( if choiceKey == name then
                                                "singular"

                                              else
                                                String.dropLeft toDrop choiceKey
                                            , string |> String.replace "{{" "{" |> String.replace "}}" "}"
                                            )
                                        )
                        in
                        { name = name
                        , translation =
                            case ( (choices |> List.map Tuple.second) |> String.join "" |> findArgs, choices ) of
                                ( [], [ single ] ) ->
                                    Basic (Dict.singleton language (Tuple.second single))

                                ( [], many ) ->
                                    Choice (Dict.singleton language (Dict.fromList many))

                                ( args, [ single ] ) ->
                                    Template args (Dict.singleton language { expressions = Nothing, string = Tuple.second single })

                                ( args, many ) ->
                                    TemplateChoice args
                                        (Dict.singleton language
                                            (many
                                                |> List.map (Tuple.mapSecond (\str -> { expressions = Nothing, string = str }))
                                                |> Dict.fromList
                                            )
                                        )
                        , complete = False
                        , invalidTemplate = True
                        }
                    )
            )


mergeDefinitions : String -> List Definition -> String -> List Definition -> Result String (List Definition)
mergeDefinitions aLang a bLang b =
    let
        merger : String -> Definition -> Definition -> Result String (Dict String Definition) -> Result String (Dict String Definition)
        merger key a_ b_ dict =
            let
                combinedTranslation : Result String Translation
                combinedTranslation =
                    case ( a_.translation, b_.translation ) of
                        ( Basic dictA, Basic dictB ) ->
                            Ok <| Basic <| Dict.union dictA dictB

                        ( Choice dictA, Choice dictB ) ->
                            Ok <| Choice <| Dict.union dictA dictB

                        ( Template argsA dictA, Template argsB dictB ) ->
                            if argsA == argsB then
                                Ok <| Template argsA <| Dict.union dictA dictB

                            else
                                Err <| "The definition \"" ++ a_.name ++ "\" doesn't have the same variables in all languages"

                        ( TemplateChoice argsA dictA, TemplateChoice argsB dictB ) ->
                            if argsA == argsB then
                                Ok <| TemplateChoice argsA <| Dict.union dictA dictB

                            else
                                Err <| "The definition \"" ++ a_.name ++ "\" doesn't have the same variables in all languages"

                        _ ->
                            Err <| "The definition \"" ++ a_.name ++ "\" is not of the same type in all languages"

                combinedDef : Result String Definition
                combinedDef =
                    Result.map (\t -> { a_ | translation = t }) combinedTranslation
            in
            Result.map2 (Dict.insert key) combinedDef dict
    in
    Dict.merge
        (\k v d -> Result.map (Dict.insert k v) d)
        merger
        (\k v d -> Result.map (Dict.insert k v) d)
        (a |> List.map (\d -> ( d.name, d )) |> Dict.fromList)
        (b |> List.map (\d -> ( d.name, d )) |> Dict.fromList)
        (Ok Dict.empty)
        |> Result.map (Dict.toList >> List.map Tuple.second)


row : Int -> List (Attribute msg) -> List (Html msg) -> Html msg
row spacing attrs children =
    div ([ class "row", attribute "style" ("--spacing:" ++ String.fromInt spacing ++ "px") ] ++ attrs) children


col : Int -> List (Attribute msg) -> List (Html msg) -> Html msg
col spacing attrs children =
    div ([ class "row", attribute "style" ("--spacing:" ++ String.fromInt spacing ++ "px") ] ++ attrs) children


spacer : Html msg
spacer =
    div [ class "spacer" ] []
