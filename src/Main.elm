module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, input, label, legend, p, text)
import Html.Attributes exposing (checked, class, src, style, type_)
import Html.Events exposing (onCheck, onClick)
import Random
import Random.List exposing (shuffle)
import Set exposing (Set, empty)



---- MODEL ----


type alias Model =
    { tabeller : List Int
    , gangestykker : List ( Int, Int )
    }


type alias Tabell =
    ( Int, Bool )


initModel =
    { tabeller = [ 6, 7, 8, 9 ]
    , gangestykker = kombinerGangestykker [ 6, 7, 8, 9 ]
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Random.generate BlandetListe (shuffle initModel.gangestykker)
    )


lagGangestykker : Int -> List ( Int, Int )
lagGangestykker gange =
    [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
        |> List.map (\x -> ( x, gange ))


kombinerGangestykker : List Int -> List ( Int, Int )
kombinerGangestykker gangetabeller =
    gangetabeller
        |> List.map lagGangestykker
        |> List.concat


dTest : Set ( Int, Int ) -> Set ( Int, Int )
dTest d =
    Set.empty



---- UPDATE ----


type Msg
    = ValgtTabell Int Bool
    | BlandGangestykker
    | BlandetListe (List ( Int, Int ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ValgtTabell tabell verdi ->
            let
                tabeller_ =
                    if List.member tabell model.tabeller then
                        List.filter (\x -> x /= tabell) model.tabeller

                    else
                        model.tabeller ++ [ tabell ]
            in
            ( { model | tabeller = tabeller_ }, Random.generate BlandetListe (shuffle <| kombinerGangestykker tabeller_) )

        BlandGangestykker ->
            ( model, Random.generate BlandetListe (shuffle <| kombinerGangestykker model.tabeller) )

        BlandetListe gangeliste ->
            ( { model | gangestykker = gangeliste }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Øveark - gangetabellen" ]
        , button [ onClick BlandGangestykker ] [ text "Bland regnestykker" ]
        , div [ class "sjekkbokser" ] <|
            List.map
                (\n ->
                    label [] [ input [ type_ "checkbox", onCheck <| ValgtTabell n, checked (List.member n model.tabeller) ] [], text <| String.fromInt n ]
                )
                [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
        , p [] <| List.map viewGangestykke model.gangestykker
        ]


viewGangestykke : ( Int, Int ) -> Html Msg
viewGangestykke ( x, y ) =
    div
        [ class "stykke" ]
        [ text <| String.fromInt x ++ " x " ++ String.fromInt y ++ " = ___" ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
