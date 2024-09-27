module Main exposing (..)

import AppUrl
import Browser exposing (Document)
import Browser.Navigation as Nav
import Dict
import Effect
import File exposing (File)
import File.Download
import File.Select
import FormatNumber.Locales exposing (Decimals(..))
import Helpers as H
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode
import Page.About as About
import Page.Documentation as Documentation
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Template as Template
import Platform.Cmd as Cmd
import Publicodes as P exposing (Mecanism(..), NodeValue(..))
import Session as S
import Task
import Url
import Url.Parser exposing (Parser)



-- MAIN


main : Program Json.Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = Home Home.Model
    | Documentation Documentation.Model
    | About S.Data
    | NotFound S.Data


init : Json.Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init encodedFlags url key =
    router url <|
        case
            Decode.decodeValue S.flagsDecoder encodedFlags
        of
            Ok flags ->
                let
                    session =
                        S.init flags
                in
                Model key (NotFound session)

            Err e ->
                let
                    emptySession =
                        S.empty
                in
                Model key (NotFound { emptySession | currentErr = Just (S.DecodeError e) })


gotoHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
gotoHome model ( homeModel, cmd ) =
    ( { model | page = Home homeModel }
    , Cmd.map HomeMsg cmd
    )


gotoDocumentation : Model -> ( Documentation.Model, Cmd Documentation.Msg ) -> ( Model, Cmd Msg )
gotoDocumentation model ( documentationModel, cmd ) =
    ( { model | page = Documentation documentationModel }
    , Cmd.map DocumentationMsg cmd
    )



-- EXIT


exit : Model -> S.Data
exit model =
    case model.page of
        Home m ->
            m.session

        Documentation m ->
            m.session

        About session ->
            session

        NotFound session ->
            session



-- ROUTING


router : Url.Url -> Model -> ( Model, Cmd Msg )
router url model =
    let
        session =
            exit model

        appUrl =
            AppUrl.fromUrl url
    in
    case appUrl.path of
        [] ->
            Home.init session
                |> gotoHome model

        [ "documentation" ] ->
            -- NOTE: we may want to redirect to the corresponding rule to have a correct URL
            Documentation.init session H.totalRuleName
                |> gotoDocumentation model

        "documentation" :: rulePath ->
            let
                ruleName =
                    String.join "/" rulePath
                        |> P.decodeRuleName
            in
            if Dict.member ruleName session.rawRules then
                Documentation.init session ruleName
                    |> gotoDocumentation model

            else
                ( { model | page = NotFound session }, Cmd.none )

        [ "about" ] ->
            ( { model | page = About session }, Cmd.none )

        _ ->
            ( { model | page = NotFound session }, Cmd.none )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Url.Parser.map handler parser



-- UPDATE


type Msg
    = NoOp
      -- Page's Msg wrappers
    | HomeMsg Home.Msg
    | DocumentationMsg Documentation.Msg
      -- Navigation
    | UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | ReactLinkClicked String
    | EngineInitialized
    | NewEncodedSituation String
      -- Situation buttons (reset, import, export)
    | ResetSituation
    | SelectSituationFile
    | ExportSituation
    | ImportSituationFile File
      -- Personas
    | SetPersonaSituation P.Situation
    | OpenPersonasModal
    | ClosePersonasModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomeMsg homeMsg ->
            case model.page of
                Home m ->
                    Home.update homeMsg m
                        |> gotoHome model

                _ ->
                    ( model, Cmd.none )

        DocumentationMsg docMsg ->
            case model.page of
                Documentation m ->
                    Documentation.update docMsg m
                        |> gotoDocumentation model

                _ ->
                    ( model, Cmd.none )

        EngineInitialized ->
            case model.page of
                Home m ->
                    -- NOTE: currently, we only evalute rules in the home page,
                    -- because the evaluation is done in the Home module.
                    -- However, me may want to evaluate rules in the other pages as well
                    -- (e.g. to display the result of a rule in the documentation page).
                    -- To do so, we would need to move the evaluation logic to the Main module.
                    let
                        ( newHomeModel, homeCmd ) =
                            S.updateEngineInitialized True m
                                |> Home.update Home.Evaluate
                    in
                    gotoHome model ( newHomeModel, homeCmd )

                Documentation m ->
                    ( { model | page = Documentation (S.updateEngineInitialized True m) }
                    , Cmd.none
                    )

                About s ->
                    ( { model | page = About { s | engineInitialized = True } }
                    , Cmd.none
                    )

                NotFound s ->
                    ( { model | page = NotFound { s | engineInitialized = True } }
                    , Cmd.none
                    )

        ResetSituation ->
            updateSituation Dict.empty model

        ExportSituation ->
            let
                session =
                    exit model
            in
            ( model
            , P.encodeSituation session.situation
                |> Json.Encode.encode 0
                --TODO: add current date to the filename
                |> File.Download.string "simulation-ekofest.json" "json"
            )

        SelectSituationFile ->
            ( model, File.Select.file [ "json" ] ImportSituationFile )

        ImportSituationFile file ->
            ( model, Task.perform NewEncodedSituation (File.toString file) )

        NewEncodedSituation encodedSituation ->
            case Decode.decodeString P.situationDecoder encodedSituation of
                Ok situation ->
                    updateSituation situation model

                Err _ ->
                    ( model, Cmd.none )

        ReactLinkClicked url ->
            ( model, Nav.pushUrl model.key url )

        UrlRequested (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        UrlRequested (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            router url model

        NoOp ->
            ( model, Cmd.none )

        SetPersonaSituation personaSituation ->
            let
                ( newModel, cmd ) =
                    updateSituation personaSituation model
            in
            ( newModel, Cmd.batch [ cmd, Effect.closeModal "persona-modal" ] )

        OpenPersonasModal ->
            let
                newModel =
                    case model.page of
                        Home m ->
                            { model | page = Home (S.openPersonasModal m) }

                        Documentation m ->
                            { model | page = Documentation (S.openPersonasModal m) }

                        About s ->
                            { model | page = About { s | alreadyOpenedPersonasModal = True } }

                        NotFound s ->
                            { model | page = NotFound { s | alreadyOpenedPersonasModal = True } }
            in
            ( newModel
            , Effect.showModal "persona-modal"
            )

        ClosePersonasModal ->
            ( model, Effect.closeModal "persona-modal" )


updateSituation : P.Situation -> Model -> ( Model, Cmd Msg )
updateSituation situation model =
    let
        newModel =
            case model.page of
                Home m ->
                    { model | page = Home (S.updateSituation (\_ -> situation) m) }

                Documentation m ->
                    { model | page = Documentation (S.updateSituation (\_ -> situation) m) }

                About s ->
                    { model | page = About { s | situation = s.situation } }

                NotFound s ->
                    { model | page = NotFound { s | situation = s.situation } }
    in
    ( newModel
    , Effect.setSituation (P.encodeSituation situation)
    )



-- VIEW


view : Model -> Document Msg
view model =
    let
        session =
            exit model

        baseConfig =
            { title = ""
            , content = text ""
            , session = session
            , showReactRoot = False
            , resetSituation = ResetSituation
            , exportSituation = ExportSituation
            , importSituation = SelectSituationFile
            , openPersonasModal = OpenPersonasModal
            , closePersonasModal = ClosePersonasModal
            , setPersonaSituation = SetPersonaSituation
            }
    in
    case model.page of
        Home m ->
            Template.view
                { baseConfig
                    | title = "Simulateur"
                    , content = Html.map HomeMsg (Home.view m)
                }

        Documentation m ->
            Template.view
                { baseConfig
                    | title = "Documentation" ++ " - " ++ H.getTitle session.rawRules m.rule
                    , content = Html.map DocumentationMsg (Documentation.view m)
                    , showReactRoot = True
                }

        About _ ->
            Template.view
                { baseConfig
                    | title = "À propos"
                    , content = About.view
                }

        NotFound _ ->
            Template.view
                { baseConfig
                    | title = "404"
                    , content = NotFound.view
                }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Effect.engineInitialized (\_ -> EngineInitialized)
        , Effect.reactLinkClicked ReactLinkClicked
        , Sub.map HomeMsg Home.subscriptions
        ]
