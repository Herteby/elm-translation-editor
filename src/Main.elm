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


hello name =
    { sv = [ "Hej ", name, "!" ]
    , dk = [ "Hej ", name, "!" ]
    , en = [ "Hi ", name, "!" ]
    }
"""


type alias Model =
    { code : String
    , translations : Result String (List Translation)
    }


type Msg
    = SetCode String
    | SetTranslation Int String String



{- main =
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
    = Basic String (Dict String String)
    | Choice String (Dict String (Dict String String))
    | Template String (List String) (Dict String (List TemplateItem))
    | ChoiceTemplate String (Dict String ( List String, List String ))


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
                ]
            )


getBasic : FunctionImplementation -> Maybe Translation
getBasic { arguments, expression, name } =
    case ( arguments, expression, name ) of
        ( [], Node _ (RecordExpr pairs), Node _ name2 ) ->
            pairs
                |> List.map
                    (\(Node _ ( Node _ key, Node _ expression2 )) ->
                        case expression2 of
                            Literal string ->
                                Just ( key, string )

                            _ ->
                                Nothing
                    )
                |> Maybe.combine
                |> Maybe.map
                    (\pairs2 ->
                        Basic name2 (Dict.fromList pairs2)
                    )

        _ ->
            Nothing


ast =
    RecordExpr
        [ Node { end = { column = 50, row = 4 }, start = { column = 6, row = 4 } }
            ( Node { end = { column = 8, row = 4 }, start = { column = 6, row = 4 } } "sv"
            , Node { end = { column = 50, row = 4 }, start = { column = 11, row = 4 } }
                (RecordExpr
                    [ Node { end = { column = 30, row = 4 }, start = { column = 12, row = 4 } } ( Node { end = { column = 20, row = 4 }, start = { column = 12, row = 4 } } "singular", Node { end = { column = 30, row = 4 }, start = { column = 22, row = 4 } } (Literal "ärende") )
                    , Node { end = { column = 49, row = 4 }, start = { column = 31, row = 4 } } ( Node { end = { column = 37, row = 4 }, start = { column = 31, row = 4 } } "plural", Node { end = { column = 48, row = 4 }, start = { column = 39, row = 4 } } (Literal "ärenden") )
                    ]
                )
            )
        , Node { end = { column = 4, row = 6 }, start = { column = 6, row = 5 } } ( Node { end = { column = 8, row = 5 }, start = { column = 6, row = 5 } } "dk", Node { end = { column = 50, row = 5 }, start = { column = 11, row = 5 } } (RecordExpr [ Node { end = { column = 30, row = 5 }, start = { column = 12, row = 5 } } ( Node { end = { column = 20, row = 5 }, start = { column = 12, row = 5 } } "singular", Node { end = { column = 30, row = 5 }, start = { column = 22, row = 5 } } (Literal "ærinde") ), Node { end = { column = 49, row = 5 }, start = { column = 31, row = 5 } } ( Node { end = { column = 37, row = 5 }, start = { column = 31, row = 5 } } "plural", Node { end = { column = 48, row = 5 }, start = { column = 39, row = 5 } } (Literal "ærinder") ) ]) )
        , Node { end = { column = 4, row = 7 }, start = { column = 6, row = 6 } } ( Node { end = { column = 8, row = 6 }, start = { column = 6, row = 6 } } "en", Node { end = { column = 46, row = 6 }, start = { column = 11, row = 6 } } (RecordExpr [ Node { end = { column = 28, row = 6 }, start = { column = 12, row = 6 } } ( Node { end = { column = 20, row = 6 }, start = { column = 12, row = 6 } } "singular", Node { end = { column = 28, row = 6 }, start = { column = 22, row = 6 } } (Literal "case") ), Node { end = { column = 45, row = 6 }, start = { column = 30, row = 6 } } ( Node { end = { column = 36, row = 6 }, start = { column = 30, row = 6 } } "plural", Node { end = { column = 44, row = 6 }, start = { column = 37, row = 6 } } (Literal "cases") ) ]) )
        ]


getChoice : FunctionImplementation -> Maybe Translation
getChoice { arguments, expression, name } =
    case ( arguments, expression, name ) of
        ( [], Node _ (RecordExpr pairs), Node _ name2 ) ->
            pairs
                |> List.map
                    (\(Node _ ( Node _ key, Node _ expression2 )) ->
                        case expression2 of
                            RecordExpr pairs2 ->
                                pairs2
                                    |> List.map
                                        (\(Node _ ( Node _ key2, Node _ expression3 )) ->
                                            case expression3 of
                                                Literal string ->
                                                    Just ( key2, string )

                                                _ ->
                                                    Nothing
                                        )
                                    |> Maybe.combine
                                    |> Maybe.map
                                        (\pairs3 ->
                                            ( key, Dict.fromList pairs3 )
                                        )

                            _ ->
                                Nothing
                    )
                |> Maybe.combine
                |> Maybe.map
                    (\pairs2 ->
                        Choice name2 (Dict.fromList pairs2)
                    )

        _ ->
            Nothing


getTemplate : FunctionImplementation -> Maybe Translation
getTemplate { arguments, expression, name } =
    case ( arguments, expression, name ) of
        ( _ :: [], Node _ (RecordExpr pairs), Node _ name2 ) ->
            pairs
                |> List.map
                    (\(Node _ ( Node _ key, Node _ expression2 )) ->
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
                                    |> Maybe.map (\list -> ( key, list ))

                            _ ->
                                Nothing
                    )
                |> Maybe.combine
                |> Maybe.map2
                    (\args pairs2 ->
                        Template name2 args (Dict.fromList pairs2)
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

                            ChoiceTemplate name _ ->
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
        Basic name pairs ->
            CodeGen.valDecl Nothing
                Nothing
                name
                (CodeGen.record
                    (pairs
                        |> Dict.toList
                        |> List.map
                            (\( key, val ) ->
                                ( key, CodeGen.string val )
                            )
                    )
                )

        Template name args pairs ->
            CodeGen.funDecl Nothing
                Nothing
                name
                (List.map CodeGen.varPattern args)
                (CodeGen.record
                    (pairs
                        |> Dict.toList
                        |> List.map
                            (\( key, list ) ->
                                ( key
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

        Choice name pairs ->
            CodeGen.valDecl Nothing
                Nothing
                name
                (pairs
                    |> Dict.toList
                    |> List.map
                        (\( key, pairs2 ) ->
                            ( key
                            , pairs2
                                |> Dict.toList
                                |> List.map (\( key2, string ) -> ( key2, CodeGen.string string ))
                                |> CodeGen.record
                            )
                        )
                    |> CodeGen.record
                )

        _ ->
            Debug.todo "TODO"
