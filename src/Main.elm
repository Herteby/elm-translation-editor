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
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra as Maybe
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
    | SetTranslation Int String String


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

        SetTranslation index key val ->
            case model.page of
                Edit translations ->
                    ( { model
                        | page =
                            Edit <|
                                List.indexedMap
                                    (\i def ->
                                        if i == index then
                                            case def.translation of
                                                Basic pairs ->
                                                    { def
                                                        | translation = Basic (Dict.insert key val pairs)
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
                    [ div [ class "upload" ]
                        [ button [ onClick SelectFile ] [ text "Upload a translation file" ]
                        , span [] [ text "or paste the contents below" ]
                        , span [ class "addExample", onClick AddExampleCode ] [ text "...or click here to add example code" ]
                        ]
                    , textarea [ value model.code, onInput SetCode ] []
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
                case translation of
                    Basic pairs ->
                        div [ class "translation" ] <|
                            div []
                                [ if checked then
                                    span [ style "color" "green" ] [ text "✔" ]

                                  else
                                    span [ style "color" "red" ] [ text "⚠" ]
                                , text name
                                ]
                                :: List.map
                                    (\( key, val ) ->
                                        label [] [ span [] [ text key ], input [ value val, onInput (SetTranslation i key) ] [] ]
                                    )
                                    (Dict.toList pairs)

                    _ ->
                        div [] [ text "TODO" ]
            )
            translations


type alias Definition =
    { name : String
    , translation : Translation
    , checked : Bool
    }


type Translation
    = Basic (Dict Language String)
    | Choice (Dict Language (Dict ChoiceKey String))
    | Template (List Argument) (Dict Language (List TemplateItem))
    | TemplateChoice (List Argument) (Dict Language (Dict ChoiceKey (List TemplateItem)))


type alias Argument =
    String


type alias Language =
    String


type alias ChoiceKey =
    String


type TemplateItem
    = Text String
    | Placeholder String


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
                                                    nodes
                                                        |> List.map
                                                            (\(Node _ item) ->
                                                                case item of
                                                                    Literal str ->
                                                                        Just <| Text str

                                                                    FunctionOrValue [] str ->
                                                                        Just <| Placeholder str

                                                                    _ ->
                                                                        Nothing
                                                            )
                                                        |> Maybe.combine
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
                                nodes
                                    |> List.map
                                        (\(Node _ item) ->
                                            case item of
                                                Literal str ->
                                                    Just <| Text str

                                                FunctionOrValue [] str ->
                                                    Just <| Placeholder str

                                                _ ->
                                                    Nothing
                                        )
                                    |> Maybe.combine
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
                            (\( language, list ) ->
                                ( language
                                , list
                                    |> List.map
                                        (\item ->
                                            case item of
                                                Text str ->
                                                    CodeGen.string str

                                                Placeholder str ->
                                                    CodeGen.val str
                                        )
                                    |> CodeGen.list
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
                                        (\( choiceKey, list ) ->
                                            ( choiceKey
                                            , list
                                                |> List.map
                                                    (\item ->
                                                        case item of
                                                            Text str ->
                                                                CodeGen.string str

                                                            Placeholder str ->
                                                                CodeGen.val str
                                                    )
                                                |> CodeGen.list
                                            )
                                        )
                                    |> CodeGen.record
                                )
                            )
                    )
                )
