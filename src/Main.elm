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
import Html.Attributes exposing (autofocus, class, classList, disabled, placeholder, style, title, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import List.Extra as List
import Maybe.Extra as Maybe
import Regex exposing (Regex)
import Result.Extra as Result
import Set exposing (Set)
import Task


port copyToClipboard : String -> Cmd msg


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


type Model
    = Start String
    | Editor EditorModel


type alias EditorModel =
    { modal : Maybe Modal
    , search : String
    , languages : Dict Language Bool
    , moduleName : ModuleName
    , definitions : List Definition
    }


type Modal
    = Preview
    | Confirm Msg String


type Msg
    = SetCode String
    | SelectFile
    | GotFile File
    | AddExampleCode
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
    ( Start ""
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Start _, SelectFile ) ->
            ( model, File.Select.file [] GotFile )

        ( Start _, GotFile file ) ->
            ( model, Task.perform SetCode (File.toString file) )

        ( Start _, SetCode str ) ->
            ( Start str, Cmd.none )

        ( Start _, AddExampleCode ) ->
            ( Start exampleCode, Cmd.none )

        ( Start _, StartEdit ( moduleName, definitions ) ) ->
            ( Editor
                { modal = Nothing
                , search = ""
                , languages = getLanguages definitions
                , moduleName = moduleName
                , definitions = definitions
                }
            , Cmd.none
            )

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
            ( Start "", Cmd.none )

        ( Editor em, ClickedDownload ) ->
            if List.all (.translation >> translationIsComplete) em.definitions then
                ( model, Download.string (getFileName em.moduleName) "text/plain" (print em.moduleName em.definitions) )

            else
                ( Editor { em | modal = Just (Confirm ConfirmedDownloadIncomplete "Some of the translations are still blank. Are you sure you are done?") }, Cmd.none )

        ( Editor em, ConfirmedDownloadIncomplete ) ->
            ( model, Download.string (getFileName em.moduleName) "text/plain" (print em.moduleName em.definitions) )

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

        ( Editor em, CloseModal ) ->
            ( Editor { em | modal = Nothing }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ header [] [ div [ class "container" ] [ text "Elm Translation Editor" ] ]
        , main_ [] <|
            case model of
                Start code ->
                    [ div [ class "toolbar" ]
                        [ button "Upload a translation file" (Just SelectFile)
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

                        shownLanguages =
                            em.languages |> Dict.toList |> List.filter Tuple.second |> List.map Tuple.first |> Set.fromList

                        definitions =
                            if em.search == "" then
                                em.definitions

                            else
                                List.filter (matchDefiniton em.search) em.definitions
                    in
                    [ div [ class "toolbar" ]
                        [ languageFilter em.languages
                        , input
                            [ value em.search
                            , onInput SetSearch
                            , placeholder "Search translations"
                            ]
                            []
                        ]
                    , editor shownLanguages definitions
                    , Maybe.unwrap (text "") (viewModal em) em.modal
                    , footer []
                        [ div [ class "container" ]
                            [ button "Back to file import" (Just ClickedBack)
                            , div [ class "footerButtons" ]
                                [ secondary "Preview" (Just ShowPreview)
                                , button "Download file" (ifValid ClickedDownload)
                                , button "Copy content to clipboard" (ifValid ClickedCopyFileToClipboard)
                                ]
                            ]
                        ]
                    ]
        ]


viewModal : EditorModel -> Modal -> Html Msg
viewModal model m =
    div [ class "ontop" ]
        [ div [ class "overlay", onClick CloseModal ] []
        , div [ class "modal" ]
            [ case m of
                Preview ->
                    pre [] [ text (print model.moduleName model.definitions) ]

                Confirm msg string ->
                    div []
                        [ text string
                        , div [ class "modalButtons" ]
                            [ button "Yes" (Just msg)
                            , button "No" (Just CloseModal)
                            ]
                        ]
            ]
        ]


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


preview : ModuleName -> List Definition -> Html Msg
preview moduleName definitions =
    div [ class "ontop" ]
        [ div [ class "overlay", onClick ShowPreview ] []
        , div [ class "preview" ]
            [ text <| print moduleName definitions
            ]
        ]


languageFilter : Dict Language Bool -> Html Msg
languageFilter languages =
    div [ class "languageFilter" ] <|
        span [] [ text "Show languages:" ]
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


editor : Set Language -> List Definition -> Html Msg
editor languages translations =
    div [] <| List.indexedMap (Lazy.lazy3 editorCard languages) translations


editorCard : Set Language -> Int -> Definition -> Html Msg
editorCard shownLanguages index { name, translation, complete } =
    let
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


type alias Definition =
    { name : String
    , translation : Translation
    , complete : Bool
    , invalidTemplate : Bool
    }


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
                                            if d.complete then
                                                1

                                            else
                                                0
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
        ( _ :: [], Node _ (RecordExpr languages) ) ->
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
        ( _ :: [], Node _ (RecordExpr languages), Node _ name2 ) ->
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


getFileName : ModuleName -> String
getFileName =
    List.reverse
        >> List.head
        >> Maybe.unwrap "Translations.elm"
            (\name -> name ++ ".elm")
