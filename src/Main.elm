module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Elm.CodeGen as CodeGen
import Elm.Parser
import Elm.Pretty as Pretty
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), FunctionImplementation)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Html exposing (..)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput)
import Maybe.Extra as Maybe


code =
    """module Translations exposing (..)
       
{-| OK
-}
welcome =
   { sv = "Välkommen"
   , dk = "Velkommen"
   , en = "Welcome"
   }


cases =
   { sv = { singular = "ärende", plural = "ärenden" }
   , dk = { singular = "ærinde", plural = "ærinder" }
   , en = { singular = "case", plural = "cases" }
   }

{-| OK
-}
hello name =
   { sv = [ "Hej ", name, "!" ]
   , dk = [ "Hej ", name, "!" ]
   , en = [ "Hi ", name, "!" ]
   }


thereAreNCases n =
   { sv = { singular = [ "Det finns ", n, " ärende" ], plural = [ "Det finns ", n, " ärenden" ] }
   , dk = { singular = [ "", n, "" ], plural = [ "", n, "" ] }
   , en = { singular = [ "There is ", n, " case" ], plural = [ "There are ", n, " cases" ] }
   }

"""


type alias Model =
    { code : String
    , translations : Result String (List Translation)
    }


type Msg
    = SetCode String
    | SetTranslation Int String String



{- main2 =
   code
       |> Elm.Parser.parse
       >> Result.mapError (always "Syntax Error")
       >> Result.map (Elm.Processing.process Elm.Processing.init)
       >> Debug.toString
       >> text
-}


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { code = code
      , translations = translationsFromCode code
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCode str ->
            ( { model | code = str, translations = translationsFromCode str }, Cmd.none )

        SetTranslation index key val ->
            ( { model
                | translations =
                    Result.map
                        (List.indexedMap
                            (\i translation ->
                                if i == index then
                                    case translation of
                                        Basic name pairs ->
                                            Basic name (Dict.insert key val pairs)

                                        _ ->
                                            translation

                                else
                                    translation
                            )
                        )
                        model.translations
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    main_ [] <|
        textarea [ value model.code, onInput SetCode ] []
            :: (case model.translations of
                    Ok translations ->
                        [ editor translations
                        , pre [] [ text (print translations) ]
                        ]

                    Err err ->
                        [ pre [] [ text err ] ]
               )


editor : List Translation -> Html Msg
editor translations =
    div [] <|
        List.indexedMap
            (\i t ->
                case t of
                    Basic name pairs ->
                        div [ class "translation" ] <|
                            div [] [ text name ]
                                :: List.map
                                    (\( key, val ) ->
                                        label [] [ text key, input [ value val, onInput (SetTranslation i key) ] [] ]
                                    )
                                    (Dict.toList pairs)

                    _ ->
                        div [] [ text "TODO" ]
            )
            translations


type Translation
    = Basic Name (Dict Language String)
    | Choice Name (Dict Language (Dict ChoiceKey String))
    | Template Name (List Argument) (Dict Language (List TemplateItem))
    | TemplateChoice Name (List Argument) (Dict Language (Dict ChoiceKey (List TemplateItem)))


type alias Name =
    String


type alias Argument =
    String


type alias Language =
    String


type alias ChoiceKey =
    String


type TemplateItem
    = Text String
    | Placeholder String


translationsFromCode : String -> Result String (List Translation)
translationsFromCode =
    Elm.Parser.parse
        >> Result.mapError (always "Syntax Error")
        >> Result.map (Elm.Processing.process Elm.Processing.init)
        >> Result.andThen
            (.declarations
                >> List.map fromAst
                >> Maybe.combine
                >> Result.fromMaybe "Unsupported code structure"
            )


fromAst : Node Declaration -> Maybe Translation
fromAst =
    getFunction
        >> Maybe.andThen
            (Maybe.oneOf
                [ getBasic
                , getTemplate
                , getChoice
                , getTemplateChoice
                ]
            )


getBasic : FunctionImplementation -> Maybe Translation
getBasic { arguments, expression, name } =
    case ( arguments, expression, name ) of
        ( [], Node _ (RecordExpr languages), Node _ name2 ) ->
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
                        Basic name2 (Dict.fromList pairs)
                    )

        _ ->
            Nothing


ast =
    RecordExpr
        [ Node { end = { column = 99, row = 4 }, start = { column = 7, row = 4 } }
            ( Node { end = { column = 9, row = 4 }, start = { column = 7, row = 4 } } "sv"
            , Node { end = { column = 99, row = 4 }, start = { column = 12, row = 4 } }
                (RecordExpr
                    [ Node { end = { column = 55, row = 4 }, start = { column = 14, row = 4 } } ( Node { end = { column = 22, row = 4 }, start = { column = 14, row = 4 } } "singular", Node { end = { column = 55, row = 4 }, start = { column = 25, row = 4 } } (ListExpr [ Node { end = { column = 39, row = 4 }, start = { column = 27, row = 4 } } (Literal "Det finns "), Node { end = { column = 42, row = 4 }, start = { column = 41, row = 4 } } (FunctionOrValue [] "n"), Node { end = { column = 53, row = 4 }, start = { column = 44, row = 4 } } (Literal " ärende") ]) )
                    , Node { end = { column = 98, row = 4 }, start = { column = 57, row = 4 } } ( Node { end = { column = 63, row = 4 }, start = { column = 57, row = 4 } } "plural", Node { end = { column = 97, row = 4 }, start = { column = 66, row = 4 } } (ListExpr [ Node { end = { column = 80, row = 4 }, start = { column = 68, row = 4 } } (Literal "Det finns "), Node { end = { column = 83, row = 4 }, start = { column = 82, row = 4 } } (FunctionOrValue [] "n"), Node { end = { column = 95, row = 4 }, start = { column = 85, row = 4 } } (Literal " ärenden") ]) )
                    ]
                )
            )
        , Node { end = { column = 5, row = 6 }, start = { column = 7, row = 5 } } ( Node { end = { column = 9, row = 5 }, start = { column = 7, row = 5 } } "dk", Node { end = { column = 64, row = 5 }, start = { column = 12, row = 5 } } (RecordExpr [ Node { end = { column = 38, row = 5 }, start = { column = 14, row = 5 } } ( Node { end = { column = 22, row = 5 }, start = { column = 14, row = 5 } } "singular", Node { end = { column = 38, row = 5 }, start = { column = 25, row = 5 } } (ListExpr [ Node { end = { column = 29, row = 5 }, start = { column = 27, row = 5 } } (Literal ""), Node { end = { column = 32, row = 5 }, start = { column = 31, row = 5 } } (FunctionOrValue [] "n"), Node { end = { column = 36, row = 5 }, start = { column = 34, row = 5 } } (Literal "") ]) ), Node { end = { column = 63, row = 5 }, start = { column = 40, row = 5 } } ( Node { end = { column = 46, row = 5 }, start = { column = 40, row = 5 } } "plural", Node { end = { column = 62, row = 5 }, start = { column = 49, row = 5 } } (ListExpr [ Node { end = { column = 53, row = 5 }, start = { column = 51, row = 5 } } (Literal ""), Node { end = { column = 56, row = 5 }, start = { column = 55, row = 5 } } (FunctionOrValue [] "n"), Node { end = { column = 60, row = 5 }, start = { column = 58, row = 5 } } (Literal "") ]) ) ]) )
        , Node { end = { column = 5, row = 7 }, start = { column = 7, row = 6 } } ( Node { end = { column = 9, row = 6 }, start = { column = 7, row = 6 } } "en", Node { end = { column = 94, row = 6 }, start = { column = 12, row = 6 } } (RecordExpr [ Node { end = { column = 52, row = 6 }, start = { column = 14, row = 6 } } ( Node { end = { column = 22, row = 6 }, start = { column = 14, row = 6 } } "singular", Node { end = { column = 52, row = 6 }, start = { column = 25, row = 6 } } (ListExpr [ Node { end = { column = 38, row = 6 }, start = { column = 27, row = 6 } } (Literal "There is "), Node { end = { column = 41, row = 6 }, start = { column = 40, row = 6 } } (FunctionOrValue [] "n"), Node { end = { column = 50, row = 6 }, start = { column = 43, row = 6 } } (Literal " case") ]) ), Node { end = { column = 93, row = 6 }, start = { column = 54, row = 6 } } ( Node { end = { column = 60, row = 6 }, start = { column = 54, row = 6 } } "plural", Node { end = { column = 92, row = 6 }, start = { column = 63, row = 6 } } (ListExpr [ Node { end = { column = 77, row = 6 }, start = { column = 65, row = 6 } } (Literal "There are "), Node { end = { column = 80, row = 6 }, start = { column = 79, row = 6 } } (FunctionOrValue [] "n"), Node { end = { column = 90, row = 6 }, start = { column = 82, row = 6 } } (Literal " cases") ]) ) ]) )
        ]


getTemplateChoice : FunctionImplementation -> Maybe Translation
getTemplateChoice { arguments, expression, name } =
    case ( arguments, expression, name ) of
        ( _ :: [], Node _ (RecordExpr languages), Node _ name2 ) ->
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
                        TemplateChoice name2 args (Dict.fromList pairs)
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
getChoice { arguments, expression, name } =
    case ( arguments, expression, name ) of
        ( [], Node _ (RecordExpr languages), Node _ name2 ) ->
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
                        Choice name2 (Dict.fromList pairs)
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
                        Template name2 args (Dict.fromList pairs)
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


getFunction : Node Declaration -> Maybe FunctionImplementation
getFunction node =
    case node of
        Node _ (FunctionDeclaration { declaration }) ->
            case declaration of
                Node _ stuff ->
                    Just stuff

        _ ->
            Nothing


print translations =
    CodeGen.file
        (CodeGen.normalModule [ "Translations" ]
            (List.map
                (\t ->
                    CodeGen.funExpose <|
                        case t of
                            Basic name _ ->
                                name

                            Choice name _ ->
                                name

                            Template name _ _ ->
                                name

                            TemplateChoice name _ _ ->
                                name
                )
                translations
            )
        )
        []
        (List.map toDeclaration translations)
        Nothing
        |> Pretty.pretty 999


toDeclaration : Translation -> CodeGen.Declaration
toDeclaration t =
    case t of
        Basic name languages ->
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

        Template name args languages ->
            CodeGen.funDecl Nothing
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

        Choice name languages ->
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

        TemplateChoice name args languages ->
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
