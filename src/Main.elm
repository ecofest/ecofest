module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Effect
import File exposing (File)
import File.Download
import File.Select
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), frenchLocale)
import Helpers as H
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import Icons
import Json.Decode as Decode exposing (string)
import Json.Decode.Pipeline as Decode
import Json.Encode
import Markdown
import Platform.Cmd as Cmd
import Publicodes as P exposing (Mecanism(..), NodeValue(..))
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as AnimProp
import Task
import UI



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Evaluation =
    { nodeValue : P.NodeValue
    , missingVariables : List P.RuleName
    , isNullable : Bool
    }


evaluationDecoder : Decode.Decoder Evaluation
evaluationDecoder =
    Decode.succeed Evaluation
        |> Decode.required "nodeValue" P.nodeValueDecoder
        |> Decode.required "missingVariables" (Decode.list string)
        |> Decode.required "isNullable" Decode.bool


type alias Model =
    { rawRules : P.RawRules
    , evaluations : Dict P.RuleName Evaluation
    , situation : P.Situation
    , questions : UI.Questions
    , categories : UI.Categories
    , orderCategories : List UI.Category
    , currentError : Maybe AppError
    , currentTab : Maybe P.RuleName
    , openedCategories : Dict P.RuleName Bool
    }


type AppError
    = DecodeError Decode.Error
    | UnvalidSituationFile


emptyModel : Model
emptyModel =
    { rawRules = Dict.empty
    , evaluations = Dict.empty
    , questions = Dict.empty
    , situation = Dict.empty
    , categories = Dict.empty
    , orderCategories = []
    , currentError = Nothing
    , currentTab = Nothing
    , openedCategories = Dict.empty
    }


type alias Flags =
    { rules : Json.Encode.Value
    , ui : Json.Encode.Value
    , situation : Json.Encode.Value
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    case
        ( Decode.decodeValue P.rawRulesDecoder flags.rules
        , Decode.decodeValue UI.uiDecoder flags.ui
        , Decode.decodeValue P.situationDecoder flags.situation
        )
    of
        ( Ok rawRules, Ok ui, Ok situation ) ->
            ( { emptyModel
                | rawRules = rawRules
                , questions = ui.questions
                , categories = ui.categories
                , situation = situation
                , orderCategories = UI.getOrderedCategories ui.categories
                , currentTab = List.head (UI.getOrderedCategories ui.categories)
              }
            , Dict.toList rawRules
                |> List.map (\( name, _ ) -> name)
                |> Effect.evaluateAll
            )

        ( Err e, _, _ ) ->
            ( { emptyModel | currentError = Just (DecodeError e) }, Cmd.none )

        ( _, Err e, _ ) ->
            ( { emptyModel | currentError = Just (DecodeError e) }, Cmd.none )

        ( _, _, Err e ) ->
            ( { emptyModel | currentError = Just (DecodeError e) }, Cmd.none )



-- UPDATE


type Msg
    = NewAnswer ( P.RuleName, P.NodeValue )
    | UpdateEvaluation ( P.RuleName, Json.Encode.Value )
    | UpdateAllEvaluation (List ( P.RuleName, Json.Encode.Value ))
    | Evaluate ()
    | ChangeTab P.RuleName
    | SetSubCategoryGraphStatus P.RuleName Bool
    | SelectFile
    | UploadedFile File
    | NewEncodedSituation String
    | ExportSituation
    | ResetSituation
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewAnswer ( name, value ) ->
            let
                newSituation =
                    case value of
                        _ ->
                            Dict.insert name value model.situation
            in
            ( { model | situation = newSituation }
            , newSituation
                |> P.encodeSituation
                |> Effect.setSituation
            )

        UpdateEvaluation ( name, encodedEvaluation ) ->
            ( updateEvaluation ( name, encodedEvaluation ) model, Cmd.none )

        UpdateAllEvaluation encodedEvaluations ->
            ( List.foldl updateEvaluation model encodedEvaluations, Cmd.none )

        Evaluate () ->
            ( model
            , -- TODO: could it be clever to only evaluate the rules that have been updated?
              Dict.toList model.rawRules
                |> List.map (\( name, _ ) -> name)
                |> Effect.evaluateAll
            )

        ChangeTab category ->
            ( { model | currentTab = Just category }, Cmd.none )

        SetSubCategoryGraphStatus category status ->
            let
                newOpenedCategories =
                    Dict.insert category status model.openedCategories
            in
            ( { model | openedCategories = newOpenedCategories }, Cmd.none )

        ExportSituation ->
            ( model
            , P.encodeSituation model.situation
                |> Json.Encode.encode 0
                |> File.Download.string "simulation-ekofest.json" "json"
            )

        UploadedFile file ->
            ( model, Task.perform NewEncodedSituation (File.toString file) )

        SelectFile ->
            ( model, File.Select.file [ "json" ] UploadedFile )

        ResetSituation ->
            ( { model | situation = Dict.empty }
            , Dict.empty
                |> P.encodeSituation
                |> Effect.setSituation
            )

        NewEncodedSituation encodedSituation ->
            case Decode.decodeString P.situationDecoder encodedSituation of
                Ok situation ->
                    ( { model | situation = situation }
                    , P.encodeSituation situation
                        |> Effect.setSituation
                    )

                Err _ ->
                    ( { model | currentError = Just UnvalidSituationFile }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateEvaluation : ( P.RuleName, Json.Encode.Value ) -> Model -> Model
updateEvaluation ( name, encodedEvaluation ) model =
    case Decode.decodeValue evaluationDecoder encodedEvaluation of
        Ok eval ->
            { model | evaluations = Dict.insert name eval model.evaluations }

        Err e ->
            { model | currentError = Just (DecodeError e) }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewHeader
        , if Dict.isEmpty model.rawRules || Dict.isEmpty model.evaluations then
            div [ class "flex flex-col w-full items-center" ]
                [ viewError model.currentError
                , div [ class "loading loading-lg text-primary mt-4" ] []
                ]

          else
            div
                [ class "flex flex-col-reverse lg:grid lg:grid-cols-3" ]
                [ div [ class "p-4 lg:pl-8 lg:pr-4 lg:col-span-2" ]
                    [ lazy2 viewCategoriesTabs model.orderCategories model.currentTab
                    , lazy viewCategory model
                    ]
                , lazy viewError model.currentError
                , div [ class "flex flex-col p-4 lg:pl-4 lg:col-span-1 lg:pr-8" ]
                    [ lazy viewResult model
                    , lazy viewGraph model
                    ]
                ]
        , viewFooter
        ]


viewHeader : Html Msg
viewHeader =
    let
        btnClass =
            "join-item btn-sm bg-base-100 font-semibold border border-base-200 hover:bg-base-200"
    in
    header []
        [ div [ class "flex items-center justify-between w-full px-8 mb-4 border-b border-base-200 text-primary bg-neutral" ]
            [ div [ class "flex items-center" ]
                [ div [ class "text-3xl font-bold text-dark m-2" ] [ text "EkoFest" ]
                , span [ class "badge badge-accent badge-outline" ] [ text "beta" ]
                ]
            , div [ class "join join-vertical p-2 sm:join-horizontal" ]
                [ button [ class (btnClass ++ " btn-primary"), onClick ResetSituation ] [ text "Recommencer ↺ " ]
                , button [ class btnClass, onClick ExportSituation ] [ text "Exporter ↑" ]
                , button
                    [ class btnClass
                    , type_ "file"
                    , multiple False
                    , accept ".json"
                    , onClick SelectFile
                    ]
                    [ text "Importer ↓" ]
                ]
            ]
        ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ div [ class "flex flex-col gap-y-2 items-center justify-center w-full px-4 py-4 mt-4 border-t border-base-200 text-primary bg-neutral" ]
            [ div [ class "flex gap-x-4" ]
                [ a
                    [ class "hover:text-primary cursor-pointer"
                    , href "https://ekofest.github.io/publicodes-evenements"
                    , target "_blank"
                    ]
                    [ text "Consulter le modèle de calcul" ]
                , div [ class "text-base-200" ]
                    [ text " | " ]
                , a
                    [ class "hover:text-primary cursor-pointer"
                    , href "https://github.com/ekofest/ekofest"
                    , target "_blank"
                    ]
                    [ text "Consulter le code source" ]
                ]
            , div [ class "text-accent text-sm" ] [ text "Fait avec amour par Milou et Clemog au Moulin Bonne Vie 🏡" ]
            ]
        ]


viewError : Maybe AppError -> Html Msg
viewError maybeError =
    case maybeError of
        Just (DecodeError e) ->
            div [ class "alert alert-error flex" ]
                [ Icons.error
                , span [] [ text (Decode.errorToString e) ]
                ]

        Just UnvalidSituationFile ->
            div [ class "alert alert-error flex" ]
                [ Icons.error
                , span [] [ text "Le fichier renseigné ne contient pas de situation valide." ]
                ]

        Nothing ->
            text ""


viewCategoriesTabs : List UI.Category -> Maybe P.RuleName -> Html Msg
viewCategoriesTabs categories currentTab =
    div [ class "flex flex-wrap md:justify-center bg-neutral rounded-md border border-base-200 p-2 mb-4" ]
        [ ul [ class "menu menu-horizontal gap-2" ]
            (categories
                |> List.map
                    (\category ->
                        let
                            activeClass =
                                currentTab
                                    |> Maybe.andThen
                                        (\tab ->
                                            if tab == category then
                                                Just " bg-primary text-white border-transparent"

                                            else
                                                Nothing
                                        )
                                    |> Maybe.withDefault ""
                        in
                        li []
                            [ a
                                [ class
                                    ("bg-base-100 rounded-md border border-base-200 cursor-pointer px-4 py-2 text-xs font-semibold hover:bg-primary hover:text-white hover:border-transparent"
                                        ++ activeClass
                                    )
                                , onClick (ChangeTab category)
                                ]
                                [ text (String.toUpper category) ]
                            ]
                    )
            )
        ]


viewCategory : Model -> Html Msg
viewCategory model =
    let
        currentCategory =
            Maybe.withDefault "" model.currentTab
    in
    div [ class "bg-neutral border-x border-b border-base-200 rounded-md " ]
        (model.categories
            |> Dict.toList
            |> List.map
                (\( category, _ ) ->
                    let
                        toShow =
                            currentCategory == category
                    in
                    Animated.div showCategoryContent
                        [ class "mb-8"
                        , style "display"
                            (if toShow then
                                "block"

                             else
                                "none"
                            )
                        ]
                        [ div [ class "pl-6 bg-base-200 font-semibold p-2 border border-base-300 rounded-t-mds" ]
                            [ text (String.toUpper category)
                            ]
                        , viewMarkdownCategoryDescription model category
                        , viewQuestions model (Dict.get category model.questions)
                        ]
                )
        )


showCategoryContent : Animation
showCategoryContent =
    Animation.fromTo
        { duration = 250
        , options = [ Animation.easeIn ]
        }
        [ AnimProp.opacity 0.5 ]
        [ AnimProp.opacity 1 ]


viewMarkdownCategoryDescription : Model -> String -> Html Msg
viewMarkdownCategoryDescription model currentCategory =
    let
        categoryDescription =
            Dict.get currentCategory model.rawRules
                |> Maybe.andThen (\ruleCategory -> ruleCategory.description)
    in
    case categoryDescription of
        Nothing ->
            text ""

        Just desc ->
            div [ class "px-6 py-3 mb-4 border-b bg-orange-50" ]
                [ div [ class "prose max-w-full" ] <|
                    Markdown.toHtml Nothing desc
                ]


viewQuestions : Model -> Maybe (List (List P.RuleName)) -> Html Msg
viewQuestions model maybeQuestions =
    case maybeQuestions of
        Just questions ->
            div [ class "grid grid-cols-1 lg:grid-cols-2 gap-6 px-6" ]
                (List.map (viewSubQuestions model) questions)

        Nothing ->
            -- TODO: display a message to the user
            text ""


viewSubQuestions : Model -> List P.RuleName -> Html Msg
viewSubQuestions model subquestions =
    div [ class "bg-neutral rounded-md p-4 border border-base-200" ]
        (subquestions
            |> List.map
                (\name ->
                    case ( Dict.get name model.rawRules, Dict.get name model.evaluations ) of
                        ( Just rule, Just eval ) ->
                            viewQuestion model ( name, rule ) eval.isNullable

                        _ ->
                            -- TODO: display a message to the user
                            text ""
                )
        )


viewQuestion : Model -> ( P.RuleName, P.RawRule ) -> Bool -> Html Msg
viewQuestion model ( name, rule ) isDisabled =
    rule.title
        |> Maybe.map
            (\title ->
                div []
                    [ label [ class "form-control mb-1" ]
                        [ div [ class "label" ]
                            [ span [ class "label-text text-md font-semibold" ] [ text title ]
                            , span [ class "label-text-alt text-md" ] [ viewUnit rule ]
                            ]
                        , if name == "transport . public . parts totales" then
                            viewCustomTransportTotal model name

                          else
                            viewInput model ( name, rule ) isDisabled
                        ]
                    ]
            )
        |> Maybe.withDefault (text "")


viewCustomTransportTotal : Model -> P.RuleName -> Html Msg
viewCustomTransportTotal model name =
    let
        maybeNodeValue =
            Dict.get name model.evaluations
                |> Maybe.map (\{ nodeValue } -> nodeValue)
    in
    case maybeNodeValue of
        Just (P.Num num) ->
            if num == 100 then
                div [ class "text-end text-success" ] [ text "100 % ✅" ]

            else
                div [ class "text-end text-error" ] [ text (H.formatFloatToFrenchLocale 1 num ++ " %") ]

        _ ->
            text ""


viewInput : Model -> ( P.RuleName, P.RawRule ) -> Bool -> Html Msg
viewInput model ( name, rule ) isDisabled =
    let
        newAnswer val =
            case String.toFloat val of
                Just value ->
                    NewAnswer ( name, P.Num value )

                Nothing ->
                    if String.isEmpty val then
                        NoOp

                    else
                        NewAnswer ( name, P.Str val )
    in
    let
        maybeNodeValue =
            Dict.get name model.evaluations
                |> Maybe.map (\{ nodeValue } -> nodeValue)
    in
    -- TODO: refactor this shit
    case ( ( rule.formula, rule.unit ), Dict.get name model.situation, maybeNodeValue ) of
        -- We have the value in the situation
        ( ( Just (UnePossibilite { possibilites }), _ ), Just situationValue, _ ) ->
            viewSelectInput model.rawRules name possibilites situationValue isDisabled

        ( ( Just (UnePossibilite { possibilites }), _ ), Nothing, Just nodeValue ) ->
            viewSelectInput model.rawRules name possibilites nodeValue isDisabled

        ( ( _, Just "%" ), Just (P.Num num), _ ) ->
            viewSliderInput num newAnswer isDisabled

        ( ( _, Just "%" ), Nothing, Just (P.Num num) ) ->
            viewSliderInput num newAnswer isDisabled

        ( _, Just (P.Num num), _ ) ->
            viewNumberInput num newAnswer isDisabled

        ( _, Just (P.Str str), _ ) ->
            viewTextInput str newAnswer isDisabled

        ( _, Just (P.Boolean bool), _ ) ->
            viewBooleanRadioInput name bool isDisabled

        -- We have a default value
        ( _, Nothing, Just (P.Num num) ) ->
            viewNumberInputOnlyPlaceHolder num newAnswer isDisabled

        ( _, Nothing, Just (P.Str str) ) ->
            viewTextInputOnlyPlaceHolder str newAnswer isDisabled

        ( _, Nothing, Just (P.Boolean bool) ) ->
            viewBooleanRadioInput name bool isDisabled

        ( _, Just Empty, Just (P.Num num) ) ->
            viewNumberInputOnlyPlaceHolder num newAnswer isDisabled

        ( _, Just Empty, Just (P.Str str) ) ->
            viewTextInputOnlyPlaceHolder str newAnswer isDisabled

        ( _, Just Empty, Just (P.Boolean bool) ) ->
            viewBooleanRadioInput name bool isDisabled

        ( _, Just Empty, _ ) ->
            viewDisabledInput

        _ ->
            viewDisabledInput


viewNumberInput : Float -> (String -> Msg) -> Bool -> Html Msg
viewNumberInput num newAnswer isDisabled =
    div [ class "flex flex-row-reverse" ]
        [ input
            [ type_ "number"
            , disabled isDisabled
            , class "input input-bordered w-1/2"
            , value (String.fromFloat num)
            , onInput newAnswer
            ]
            []
        ]


viewNumberInputOnlyPlaceHolder : Float -> (String -> Msg) -> Bool -> Html Msg
viewNumberInputOnlyPlaceHolder num newAnswer isDisabled =
    div [ class "flex flex-row-reverse" ]
        [ input
            [ type_ "number"
            , disabled isDisabled
            , class "input input-bordered w-1/2"
            , placeholder (String.fromFloat num)
            , onInput newAnswer
            ]
            []
        ]


viewTextInput : String -> (String -> Msg) -> Bool -> Html Msg
viewTextInput str newAnswer isDisabled =
    input
        [ type_ "text"
        , disabled isDisabled
        , class "input input-bordered"
        , value str
        , onInput newAnswer
        ]
        []


viewTextInputOnlyPlaceHolder : String -> (String -> Msg) -> Bool -> Html Msg
viewTextInputOnlyPlaceHolder str newAnswer isDisabled =
    input
        [ type_ "text"
        , disabled isDisabled
        , class "input input-bordered"
        , placeholder str
        , onInput newAnswer
        ]
        []


viewSelectInput : P.RawRules -> P.RuleName -> List String -> P.NodeValue -> Bool -> Html Msg
viewSelectInput rules ruleName possibilites nodeValue isDisabled =
    select
        [ onInput (\v -> NewAnswer ( ruleName, P.Str v ))
        , class "select select-bordered"
        , disabled isDisabled
        ]
        (possibilites
            |> List.map
                (\possibilite ->
                    option
                        [ value possibilite
                        , selected (H.getStringFromSituation nodeValue == possibilite)
                        ]
                        [ text (H.getOptionTitle rules ruleName possibilite) ]
                )
        )


viewBooleanRadioInput : P.RuleName -> Bool -> Bool -> Html Msg
viewBooleanRadioInput name bool isDisabled =
    div [ class "form-control" ]
        [ label [ class "label cursor-pointer" ]
            [ span [ class "label-text" ] [ text "Oui" ]
            , input
                [ class "radio radio-sm"
                , type_ "radio"
                , checked bool
                , disabled isDisabled
                , onCheck (\b -> NewAnswer ( name, P.Boolean b ))
                ]
                []
            ]
        , label [ class "label cursor-pointer" ]
            [ span [ class "label-text" ] [ text "Non" ]
            , input
                [ class "radio radio-sm"
                , type_ "radio"
                , checked (not bool)
                , disabled isDisabled
                , onCheck (\b -> NewAnswer ( name, P.Boolean (not b) ))
                ]
                []
            ]
        ]


viewSliderInput : Float -> (String -> Msg) -> Bool -> Html Msg
viewSliderInput num newAnswer isDisabled =
    div [ class "flex flex-row" ]
        [ input
            [ type_ "range"
            , disabled isDisabled
            , class "range range-xs my-2"
            , value (String.fromFloat num)
            , onInput newAnswer
            , Html.Attributes.min "0"
            , Html.Attributes.max "100"

            -- Should use `plancher` and `plafond` attributes
            ]
            []
        , span [ class "ml-4" ] [ text (String.fromFloat num) ]
        ]


viewDisabledInput : Html Msg
viewDisabledInput =
    input [ class "input", disabled True ] []



-- Results


viewResult : Model -> Html Msg
viewResult model =
    let
        resultRules =
            Dict.toList model.rawRules
                |> List.filterMap
                    (\( name, rule ) ->
                        case P.splitRuleName name of
                            [ namespace, _ ] ->
                                if namespace == H.resultNamespace then
                                    Just ( name, rule )

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
    in
    div [ class "stats stats-vertical border w-full rounded-md bg-neutral border-base-200" ]
        (resultRules
            |> List.map
                (\( name, rule ) ->
                    div [ class "stat" ]
                        [ div [ class "stat-title" ]
                            [ text (H.getTitle model.rawRules name) ]
                        , div [ class "flex items-baseline" ]
                            [ div [ class "stat-value text-primary" ]
                                [ viewEvaluation (Dict.get name model.evaluations) ]
                            , div [ class "stat-desc text-primary ml-2 text-base" ] [ viewUnit rule ]
                            ]
                        ]
                )
        )


viewEvaluation : Maybe Evaluation -> Html Msg
viewEvaluation eval =
    case eval of
        Just { nodeValue } ->
            text (P.nodeValueToString nodeValue)

        Nothing ->
            text ""


viewUnit : P.RawRule -> Html Msg
viewUnit rawRule =
    case rawRule.unit of
        Just "l" ->
            text " litre"

        Just unit ->
            text (" " ++ unit)

        Nothing ->
            text ""


viewGraph : Model -> Html Msg
viewGraph model =
    let
        total =
            Dict.get H.totalRuleName model.evaluations
                |> Maybe.andThen (\{ nodeValue } -> P.nodeValueToFloat nodeValue)
                |> Maybe.withDefault 0
    in
    let
        subInfos subList totalCat =
            subList
                |> List.filterMap
                    (\sub ->
                        Dict.get sub model.evaluations
                            |> Maybe.andThen
                                (\{ nodeValue } ->
                                    case nodeValue of
                                        P.Num value ->
                                            Just
                                                { subCat = H.getTitle model.rawRules sub
                                                , percent = (value / totalCat) * 100
                                                , totalSubCat = value
                                                }

                                        _ ->
                                            Nothing
                                )
                    )
    in
    let
        data =
            model.categories
                |> Dict.toList
                |> List.filterMap
                    -- TODO: manage subcategories
                    (\( category, infos ) ->
                        Dict.get category model.evaluations
                            |> Maybe.andThen
                                (\{ nodeValue } ->
                                    case nodeValue of
                                        P.Num value ->
                                            Just
                                                { category = category
                                                , percent = (value / total) * 100
                                                , subCatInfos = subInfos infos.sub value
                                                }

                                        _ ->
                                            Nothing
                                )
                    )
    in
    div [ class "stats stats-vertical border border-base-200 w-full rounded-md bg-neutral mt-4" ]
        (data
            |> List.sortBy .percent
            |> List.reverse
            |> List.map
                (\{ category, percent, subCatInfos } ->
                    let
                        formattedPercent =
                            format { frenchLocale | decimals = Exact 0 } percent ++ "%"
                    in
                    let
                        subCatHidden =
                            Dict.get category model.openedCategories
                                |> Maybe.withDefault True
                    in
                    div []
                        [ div [ class "stat py-2 cursor-pointer relative z-10 bg-white", onClick (SetSubCategoryGraphStatus category (not subCatHidden)) ]
                            [ div [ class "stat-title" ]
                                [ viewCategoryArrow subCatHidden, span [] [ text (String.toUpper category) ] ]
                            , div []
                                [ div [ class "h-8 flex items-center" ]
                                    [ div
                                        [ class "stat-value text-primary w-20 text-2xl" ]
                                        [ text
                                            (H.formatFloatToFrenchLocale 1 percent
                                                ++ " %"
                                            )
                                        ]
                                    , div [ class "bg-secondary rounded-lg h-2", style "width" formattedPercent ]
                                        []
                                    ]
                                ]
                            ]
                        , viewSubCategoryGraph subCatHidden subCatInfos
                        ]
                )
        )


viewCategoryArrow : Bool -> Html Msg
viewCategoryArrow subCatHidden =
    if subCatHidden then
        span [ class "mr-2 text-xs" ] [ text "▶" ]

    else
        span [ class "mr-2 text-xs" ] [ text "▼" ]


showSubCat : Animation
showSubCat =
    Animation.fromTo
        { duration = 250
        , options = []
        }
        [ AnimProp.opacity 0.6, AnimProp.y -50 ]
        [ AnimProp.opacity 1, AnimProp.y 0 ]


viewSubCategoryGraph : Bool -> List { subCat : P.RuleName, percent : Float, totalSubCat : Float } -> Html Msg
viewSubCategoryGraph subCatHidden subCatInfos =
    div [ class "relative z-0" ]
        [ Animated.div showSubCat
            [ class "border-x-0 bg-neutral py-2", style "boxShadow" "0px 6px 6px -2px rgba(21, 3, 35, 0.05) inset", hidden subCatHidden ]
            (subCatInfos
                |> List.sortBy .percent
                |> List.reverse
                |> List.map
                    (\{ subCat, percent, totalSubCat } ->
                        let
                            formattedPercent =
                                format { frenchLocale | decimals = Exact 0 } percent ++ "%"
                        in
                        div [ class "stat py-1" ]
                            [ div [ class "stat-title text-primary text-xs" ]
                                [ span []
                                    [ text
                                        (String.toUpper subCat
                                            ++ " - "
                                            ++ (H.formatFloatToFrenchLocale 0 (totalSubCat / 1000)
                                                    ++ " tCO2e"
                                               )
                                        )
                                    ]
                                ]
                            , div [ class "h-2 flex items-center" ]
                                [ div [ class "bg-primary rounded-lg h-0.5", style "width" formattedPercent ]
                                    []
                                ]
                            ]
                    )
            )
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Effect.evaluatedRule UpdateEvaluation
        , Effect.evaluatedRules UpdateAllEvaluation
        , Effect.situationUpdated Evaluate
        ]
