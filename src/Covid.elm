module Covid exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (int, list, string)
import Json.Decode.Pipeline exposing (required)


--MODELO
type alias Continent =
    { name : String
    , population : Int
    , totalCases: Int
    , active: Int
    , recovered: Int
    , deaths: Int
    }


type alias Model =
    { continents : List Continent
    , errorMessage : Maybe String
    }


--VISTA
view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ]
            [ text "Get data from server" ]
        , viewContinentOrError model  
        ]


viewContinentOrError : Model -> Html Msg
viewContinentOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewContinents model.continents


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewContinents : List Continent -> Html Msg
viewContinents continents =
    div []
        [ h3 [] [ text "Continents" ]
        , table []
            ([ viewTableHeader ] ++ List.map viewContinent continents)
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "Continente" ]
        , th []
            [ text "Población" ]
        , th []
            [ text "Casos totales" ]
        , th []
            [ text "Casos activos" ]
        , th []
            [ text "Casos recuperados" ]
        , th []
            [ text "Muertes" ]
        ]


viewContinent : Continent -> Html Msg
viewContinent continent =
    tr []
        [ td []
            [ text continent.name ]
        , td []
            [ text (String.fromInt continent.population) ]
        , td []
            [ text (String.fromInt continent.totalCases) ]
        , td []
            [ text (String.fromInt continent.active) ]
        , td []
            [ text (String.fromInt continent.recovered) ]
        , td []
            [ text (String.fromInt continent.deaths) ]
        ]


--ACTUALIZAR
type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Continent))


jsonDecoder : Decode.Decoder (List Continent)
jsonDecoder = 
    Decode.list (Decode.succeed Continent
        |> required "continent" string
        |> required "population" int
        |> required "cases" int
        |> required "active" int
        |> required "recovered" int
        |> required "deaths" int)
    

getDatos : Cmd Msg
getDatos =
    Http.get
        { url = "https://corona.lmao.ninja/v2/continents?yesterday=true&sort"
        , expect = Http.expectJson DataReceived jsonDecoder
        }
    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, getDatos )

        DataReceived (Ok continents) ->
            ( { model
                | continents = continents
                , errorMessage = Nothing
              }
            , Cmd.none
            )

        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


init : () -> ( Model, Cmd Msg )
init _ =
    ( { continents = []
      , errorMessage = Nothing
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }