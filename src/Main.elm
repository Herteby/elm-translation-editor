port module Main exposing (main)

import Browser
import Dict exposing (Dict)
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
import Html.Attributes exposing (autofocus, class, classList, disabled, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy as Lazy
import List.Extra as List
import Maybe.Extra as Maybe
import Regex exposing (Regex)
import Result.Extra as Result
import Task


port confirmBack : () -> Cmd msg


port confirmedBack : (() -> msg) -> Sub msg


port copyToClipboard : String -> Cmd msg


exampleCode =
    """module Translations exposing (..)
       
{-| -}
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
   

{-| -}
hi name =
   { dk = [ "Hej ", name, "!" ]
   , en = [ "Hi ", name, "!" ]
   , sv = [ "Hej ", name, "!" ]
   }
   

{-| -}
thereAreNCases n =
   { sv = { singular = [ "Det finns ", n, " ärende" ], plural = [ "Det finns ", n, " ärenden" ] }
   , dk = { singular = [], plural = [] }
   , en = { singular = [ "There is ", n, " case" ], plural = [ "There are ", n, " cases" ] }
   }

"""


type Model
    = Start String
    | Editor Bool ModuleName (List Definition)


type Msg
    = SetCode String
    | SelectFile
    | GotFile File
    | AddExampleCode
    | StartEdit ( List String, List Definition )
    | SetTranslation Int (Maybe ChoiceKey) Language String
    | BackToStart
    | ConfirmedBackToStart
    | Download
    | CopyToClipBoard
    | TogglePreview


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always (confirmedBack (always ConfirmedBackToStart))
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
            ( Editor False moduleName definitions, Cmd.none )

        ( Editor _ moduleName definitions, SetTranslation index choiceKey language string ) ->
            ( definitions
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
                            { d | checked = translationIsValid d.translation }

                        else
                            d
                    )
                |> Editor False moduleName
            , Cmd.none
            )

        ( Editor _ _ _, BackToStart ) ->
            ( model, confirmBack () )

        ( Editor _ _ _, ConfirmedBackToStart ) ->
            ( Start "", Cmd.none )

        ( Editor _ moduleName definitions, Download ) ->
            ( model, Download.string (getFileName moduleName) "text/plain" (print moduleName definitions) )

        ( Editor _ moduleName definitions, CopyToClipBoard ) ->
            ( model, print moduleName definitions |> copyToClipboard )

        ( Editor showPreview m d, TogglePreview ) ->
            ( Editor (not showPreview) m d, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ header []
            [ div [ class "container" ]
                [ div [] [ text "Elm Translation Editor" ]
                , case model of
                    Editor _ moduleName _ ->
                        div [] [ text <| getFileName moduleName ]

                    Start _ ->
                        text ""
                ]
            ]
        , main_ [] <|
            case model of
                Start code ->
                    [ div [ class "toolbar" ]
                        [ button [ onClick SelectFile ] [ text "Upload a translation file" ]
                        , span [ class "action", onClick AddExampleCode ] [ text "click here to insert example code" ]
                        ]
                    , textarea
                        [ value code
                        , placeholder "...or paste the contents here"
                        , onInput SetCode
                        , autofocus True
                        ]
                        []
                    , case translationsFromCode code of
                        Ok translations ->
                            button [ onClick (StartEdit translations) ] [ text "Edit translations" ]

                        Err err ->
                            div [ class "error" ] [ text err ]
                    ]

                Editor showPreview moduleName definitions ->
                    let
                        valid =
                            List.all .checked definitions
                    in
                    [ editor definitions
                    , if showPreview then
                        preview moduleName definitions

                      else
                        text ""
                    , footer []
                        [ div [ class "container" ]
                            [ button [ onClick BackToStart ] [ text "Back to file import" ]
                            , div [ class "footerButtons" ]
                                [ span [ class "action", onClickIf valid TogglePreview ]
                                    [ text "Preview" ]
                                , button
                                    [ onClickIf valid Download ]
                                    [ text "Download file" ]
                                , button
                                    [ onClickIf valid CopyToClipBoard ]
                                    [ text "Copy content to clipboard" ]
                                ]
                            ]
                        ]
                    ]
        ]


preview : ModuleName -> List Definition -> Html Msg
preview moduleName definitions =
    div [ class "ontop" ]
        [ div [ class "overlay", onClick TogglePreview ] []
        , div [ class "preview" ]
            [ text <| print moduleName definitions
            ]
        ]


onClickIf : Bool -> msg -> Attribute msg
onClickIf bool msg =
    if bool then
        onClick msg

    else
        class "disabled"


editor : List Definition -> Html Msg
editor translations =
    div [] <| List.indexedMap (Lazy.lazy2 editorCard) translations


editorCard : Int -> Definition -> Html Msg
editorCard index { name, translation, checked } =
    div [ classList [ ( "card", True ), ( "todo", not checked ) ] ] <|
        div []
            [ text name ]
            :: (case translation of
                    Basic languages ->
                        List.map
                            (\( language, string ) ->
                                textInput language string False (SetTranslation index Nothing)
                            )
                            (Dict.toList languages)

                    Choice languages ->
                        List.map
                            (\( language, choices ) ->
                                choiceInput language (Dict.map (\_ c -> ( c, False )) choices) (SetTranslation index)
                            )
                            (Dict.toList languages)

                    Template placeholders languages ->
                        viewPlaceholders placeholders
                            :: List.map
                                (\( language, { string, expressions } ) ->
                                    textInput language string (expressions == Nothing) (SetTranslation index Nothing)
                                )
                                (Dict.toList languages)

                    TemplateChoice placeholders languages ->
                        viewPlaceholders placeholders
                            :: List.map
                                (\( language, choices ) ->
                                    choiceInput language
                                        (Dict.map (\_ { string, expressions } -> ( string, expressions == Nothing )) choices)
                                        (SetTranslation index)
                                )
                                (Dict.toList languages)
               )


viewPlaceholders : List String -> Html msg
viewPlaceholders placeholders =
    div [ class "placeholders" ]
        (List.map (\p -> div [] [ text <| "{" ++ p ++ "}" ]) placeholders)


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
    , checked : Bool
    }


translationIsValid : Translation -> Bool
translationIsValid translation =
    case translation of
        Basic languages ->
            Dict.values languages |> List.all ((/=) "")

        Choice languages ->
            Dict.values languages |> List.concatMap Dict.values |> List.all ((/=) "")

        Template args languages ->
            Dict.values languages |> List.all (\{ string, expressions } -> expressions /= Nothing && string /= "")

        TemplateChoice args languages ->
            Dict.values languages |> List.concatMap Dict.values |> List.all (\{ string, expressions } -> expressions /= Nothing && string /= "")


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
                                            if d.checked then
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
                        , checked = Maybe.isJust documentation && translationIsValid translation
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
    if List.all (\p -> List.member p args) matches && not gullWings then
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
        (definitions |> List.sortBy .name |> List.map toDeclaration )
        Nothing
        |> Pretty.pretty 100


toDeclaration : Definition -> CodeGen.Declaration
toDeclaration { name, translation, checked } =
    let
        comment =
            if checked then
                CodeGen.emptyDocComment
                    |> Just

            else
                Nothing
    in
    case translation of
        Basic languages ->
            CodeGen.valDecl comment
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
            CodeGen.funDecl comment
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
            CodeGen.valDecl comment
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
            CodeGen.funDecl comment
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
