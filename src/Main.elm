module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Elm.CodeGen as CodeGen
import Elm.Parser
import Elm.Pretty as Pretty
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), FunctionImplementation)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import File exposing (File)
import File.Select
import Html exposing (..)
import Html.Attributes exposing (class, classList, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as List
import Maybe.Extra as Maybe
import Regex exposing (Regex)
import Result.Extra as Result
import Task


exampleCode =
    """module Translations exposing (..)
       
{-| -}
welcome =
   { dk = "Velkommen"
   , en = "Welcome"
   , sv = "Välkommen"
   }
   
welcome =
   { dk = "Velkommen"
   , en = "Welcome"
   , sv = "Välkommen"
   }
   
{-| -}
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
   , dk = { singular = [ "", n, "" ], plural = [ "", n, "" ] }
   , en = { singular = [ "There is ", n, " case" ], plural = [ "There are ", n, " cases" ] }
   }

"""


type alias Model =
    { page : Page
    , code : String
    }


type Page
    = Start
    | Edit (List Definition)
    | Done (List Definition)


type Msg
    = SetCode String
    | SelectFile
    | GotFile File
    | AddExampleCode
    | StartEdit (List Definition)
    | SetTranslation Int (Maybe ChoiceKey) Language String


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = Start
      , code = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectFile ->
            ( model, File.Select.file [] GotFile )

        GotFile file ->
            let
                _ =
                    Debug.log "file" (File.mime file)
            in
            ( model, Task.perform SetCode (File.toString file) )

        SetCode str ->
            ( { model | code = str }, Cmd.none )

        AddExampleCode ->
            ( { model | code = exampleCode }, Cmd.none )

        StartEdit translations ->
            ( { model | page = Edit translations }, Cmd.none )

        SetTranslation index choiceKey language string ->
            case model.page of
                Edit translations ->
                    ( { model
                        | page =
                            Edit <|
                                List.indexedMap
                                    (\i def ->
                                        if i == index then
                                            case ( def.translation, choiceKey ) of
                                                ( Basic languages, Nothing ) ->
                                                    { def
                                                        | translation = Basic (Dict.insert language string languages)
                                                        , checked = True
                                                    }

                                                ( Choice languages, Just key ) ->
                                                    { def
                                                        | translation = Choice (Dict.update language (Maybe.map (Dict.insert key string)) languages)
                                                        , checked = True
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
                                                        , checked = True
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
                                                        , checked = True
                                                    }

                                                _ ->
                                                    def

                                        else
                                            def
                                    )
                                    translations
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ header [] [ div [ class "container" ] [ text "Elm Translation Editor" ] ]
        , main_ [ class "container" ] <|
            case model.page of
                Start ->
                    [ div [ class "toolbar" ]
                        [ button [ onClick SelectFile ] [ text "Upload a translation file" ]
                        , span [ class "addExample", onClick AddExampleCode ] [ text "click here to insert example code" ]
                        ]
                    , textarea [ value model.code, placeholder "...or paste the contents here", onInput SetCode ] []
                    , case translationsFromCode model.code of
                        Ok translations ->
                            button [ onClick (StartEdit translations) ] [ text "Edit translations" ]

                        Err err ->
                            div [ class "error" ] [ text err ]
                    ]

                Edit translations ->
                    [ editor translations ]

                Done translations ->
                    [ pre [] [ text (print translations) ] ]
        ]


editor : List Definition -> Html Msg
editor translations =
    div [] <|
        List.indexedMap
            (\i { name, translation, checked } ->
                div [ classList [ ( "card", True ), ( "todo", not checked ) ] ] <|
                    div []
                        [ text name ]
                        :: (case translation of
                                Basic languages ->
                                    List.map
                                        (\( language, string ) ->
                                            textInput language string False (SetTranslation i Nothing)
                                        )
                                        (Dict.toList languages)

                                Choice languages ->
                                    [ div [ class "columns" ] [] ]

                                Template placeholders languages ->
                                    List.map
                                        (\( language, { string, expressions } ) ->
                                            textInput language string (expressions == Nothing) (SetTranslation i Nothing)
                                        )
                                        (Dict.toList languages)

                                TemplateChoice placeholders languages ->
                                    [ div [ class "columns" ] [] ]
                           )
            )
            translations


textInput : String -> String -> Bool -> (String -> String -> msg) -> Html msg
textInput language string invalid msg =
    label [ classList [ ( "invalid", invalid ) ] ]
        [ span [] [ text language ]
        , div [ class "autoexpand" ]
            [ textarea [ onInput (msg language), value string ] []
            , div [] [ text (string ++ "_") ]
            ]
        ]


type alias Definition =
    { name : String
    , translation : Translation
    , checked : Bool
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


translationsFromCode : String -> Result String (List Definition)
translationsFromCode str =
    if str == "" then
        Err ""

    else
        str
            |> Elm.Parser.parse
            |> Result.mapError (always "Syntax Error")
            |> Result.map (Elm.Processing.process Elm.Processing.init)
            |> Result.andThen
                (.declarations
                    >> List.map fromAst
                    >> Result.combine
                    >> Result.map
                        (List.sortBy
                            (\d ->
                                if d.checked then
                                    1

                                else
                                    0
                            )
                        )
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
                        , checked = Maybe.isJust documentation
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
            Regex.split rgx templateBody |> List.map CodeGen.string
    in
    if List.all (\p -> List.member p args) matches then
        Just <| List.interweave strings vals

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


print : List Definition -> String
print definition =
    CodeGen.file
        (CodeGen.normalModule [ "Translations" ]
            (List.map
                (.name >> CodeGen.funExpose)
                definition
            )
        )
        []
        (List.map toDeclaration definition)
        Nothing
        |> Pretty.pretty 999


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
