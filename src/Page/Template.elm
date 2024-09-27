module Page.Template exposing (Config, view)

import Browser exposing (Document)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Personas exposing (Personas)
import Publicodes as P
import Session as S
import Views.Icons as Icons
import Views.Link as Link


type alias Config msg =
    { title : String
    , content : Html msg
    , session : S.Data

    -- Show an empty div to mount React components and render custom elements.
    -- Currenlty, this is used to render the Publicodes documentation.
    , showReactRoot : Bool
    , resetSituation : msg
    , exportSituation : msg
    , importSituation : msg
    , openPersonasModal : msg
    , closePersonasModal : msg
    , setPersonaSituation : P.Situation -> msg
    }


view : Config msg -> Document msg
view config =
    { title = config.title ++ " | Ekofest"
    , body =
        [ viewHeader config
        , viewPersonasModal
            config.session.personas
            config.setPersonaSituation
            config.closePersonasModal
        , if config.showReactRoot then
            div [ id "react-root" ] []

          else
            text ""
        , main_ []
            [ if Dict.isEmpty config.session.rawRules then
                div [ class "flex flex-col w-full h-full items-center" ]
                    [ S.viewError config.session.currentErr
                    , div [ class "loading loading-lg text-primary mt-4" ] []
                    ]

              else
                config.content
            ]
        , viewFooter
        ]
    }


viewHeader : Config msg -> Html msg
viewHeader { resetSituation, exportSituation, importSituation, openPersonasModal, session } =
    let
        btnClass =
            "join-item inline-flex items-center btn-sm bg-base-100 border border-base-200 hover:bg-base-200"
    in
    header []
        [ div [ class "flex items-center md:flex-row justify-between flex-col w-full px-4 lg:px-8 border-b border-base-200 bg-neutral" ]
            [ div [ class "flex flex-col items-center gap-4 mb-4 sm:mb-0 sm:items-center sm:justify-center sm:flex-row" ]
                [ a [ href "/" ]
                    [ img
                        [ src "/assets/logo.svg"
                        , class "w-32 m-4"
                        , width 128
                        , height 32
                        , alt "ekofest logo"
                        ]
                        []
                    ]
                , span [ class "relative inline-flex" ]
                    [ button [ class (btnClass ++ " rounded-md"), onClick openPersonasModal ]
                        [ text "Commencer avec un profil d'évènement"
                        ]
                    , viewPing (not session.alreadyOpenedPersonasModal)
                    ]
                ]
            , div [ class "join my-4 md:my-0 md:mb-0 rounded-md" ]
                [ button [ class btnClass, onClick resetSituation ]
                    [ span [ class "mx-2 xsm:mr-2" ] [ Icons.refresh ]
                    , span [ class "invisible hidden xsm:visible xsm:block" ] [ text "Recommencer" ]
                    ]
                , button [ class btnClass, onClick exportSituation ]
                    [ span [ class "mx-2 xsm:mr-2" ] [ Icons.download ]
                    , span [ class "invisible hidden xsm:visible xsm:block" ] [ text "Télécharger" ]
                    ]
                , button
                    [ class btnClass
                    , type_ "file"
                    , multiple False
                    , accept ".json"
                    , onClick importSituation
                    ]
                    [ span [ class "mx-2 xsm:mr-2" ] [ Icons.upload ]
                    , span [ class "invisible hidden xsm:visible xsm:block" ] [ text "Importer" ]
                    ]
                ]
            ]
        ]


viewPing : Bool -> Html msg
viewPing show =
    if show then
        span [ class "flex absolute h-3 w-3 top-0 right-0 -mt-1 -mr-1" ]
            [ span [ class "animate-ping absolute inline-flex h-full w-full rounded-full bg-accent opacity-75" ] []
            , span [ class "relative inline-flex rounded-full h-3 w-3 bg-accent" ] []
            ]

    else
        span [] []


{-| TODO: abstract this into a reusable component
-}
viewPersonasModal : Personas -> (P.Situation -> msg) -> msg -> Html msg
viewPersonasModal personas setPersonaSituation closePersonasModal =
    node "dialog"
        [ id "persona-modal", class "modal modal-bottom sm:modal-middle" ]
        [ div [ class "modal-box rounded-md bg-neutral" ]
            [ h3 [ class "text-xl pb-4 font-semibold" ]
                [ text "Choississez le profil qui correspond le plus à votre évènement" ]
            , viewPersonas personas setPersonaSituation
            , div [ class "modal-action" ]
                [ button
                    [ class "btn-sm border border-base-200 hover:bg-base-200 rounded-md"
                    , onClick closePersonasModal
                    ]
                    [ text "Fermer" ]
                ]
            ]
        ]


viewPersonas : Personas -> (P.Situation -> msg) -> Html msg
viewPersonas personas setPersonaSituation =
    div [ class "grid grid-cols-2 gap-4" ]
        (personas
            |> Dict.toList
            |> List.map
                (\( _, persona ) ->
                    button
                        [ class "btn text-md font-semibold bg-primary/5 border border-primary/20 hover:bg-primary/20 hover:border-primary/20 rounded-md p-4 flex items-center justify-center w-full h-24"
                        , onClick (setPersonaSituation persona.situation)
                        ]
                        [ text persona.titre ]
                )
        )


viewFooter : Html msg
viewFooter =
    div []
        [ footer [ class "footer p-8 bg-neutral text-base-content border-t border-base-200" ]
            [ aside [ class "text-md max-w-4xl" ]
                [ div []
                    [ text """
                    Ekofest a pour objectif de faciliter l'organisation d'événements festifs et culturels éco-responsables.
                    L'outil permet de rapidement estimer l'impact carbone (en équivalent CO2) d'un événement
                    afin de repérer les postes les plus émetteurs et anticiper les actions à mettre en place.
                    """
                    ]
                , div [ class "" ]
                    [ text """
                    Ce simulateur a été développé dans une démarche de transparence et de partage.
                    Ainsi, le code du simulateur est libre et ouvert, de la même manière que le modèle de calcul.
                    """
                    ]
                , div []
                    [ text "Fait avec "
                    , Icons.heartHandshake
                    , text " par "
                    , Link.external [ href "https://github.com/EmileRolley" ] [ text "Milou" ]
                    , text " et "
                    , Link.external [ href "https://github.com/clemog" ] [ text "Clemog" ]
                    , text " au Moulin Bonne Vie en février 2024."
                    ]
                ]
            , nav []
                [ h6 [ class "footer-title" ] [ text "Liens utiles" ]
                , Link.internal [ href "/about" ]
                    [ text "À propos" ]
                , Link.internal [ href "/documentation" ]
                    [ text "Documentation du modèle" ]
                , Link.external [ href "https://github.com/ekofest/publicodes-evenements" ]
                    [ text "Code source du modèle" ]
                , Link.external [ href "https://github.com/ekofest/ekofest" ]
                    [ text "Code source du site" ]
                ]
            , nav []
                [ h6 [ class "footer-title" ] [ text "Informations" ]
                , a [ href "https://bff.ecoindex.fr/redirect/?url=https://ekofest.fr", target "_blank" ]
                    [ img
                        [ src "https://bff.ecoindex.fr/badge/?theme=light&url=https://ekofest.fr"
                        , alt "Ecoindex Badge"
                        , class "w-24"
                        , width 96
                        , height 24
                        ]
                        []
                    ]
                , a [ href "https://www.netlify.com" ]
                    [ img
                        [ src "https://www.netlify.com/v3/img/components/netlify-light.svg"
                        , alt "Deploys by Netlify"
                        , class "w-24"
                        , width 96
                        , height 24
                        ]
                        []
                    ]
                ]
            ]
        , footer [ class "footer p-4 bg-red-50 text-base-content border-t border-base-200" ]
            [ div []
                [ div [ class "text-sm" ]
                    [ text """Ce simulateur étant en cours de développement, les résultats obtenus
                sont donc à prendre avec précaution et ne peuvent se substituer à un bilan carbone.
                Pour toute question ou suggestion, n'hésitez pas """
                    , a [ class "link", href "mailto:emile.rolley@tuta.io" ] [ text "à nous contacter" ]
                    , text "."
                    ]
                ]
            ]
        ]
