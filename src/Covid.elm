module Covid exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string, field, at)
import Json.Decode.Pipeline exposing (required)


--MODELO
type alias Summary =
    { country : String
    , countryCode : String
    , slug : String
    , newConfirmed: Int
    , totalConfirmed: Int
    , newDeaths: Int
    , totalDeath: Int
    , newRecovered: Int
    , totalRecovered: Int
    }


type alias Model =
    { summaries : List Summary
    , errorMessage : Maybe String
    }


--VISTA
view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ]
            [ text "Get data from server" ]
        , viewSummaryOrError model
        ]


viewSummaryOrError : Model -> Html Msg
viewSummaryOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewSummaries model.summaries


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


viewSummaries : List Summary -> Html Msg
viewSummaries summaries =
    div []
        [ h3 [] [ text "Summaries" ]
        , table []
            ([ viewTableHeader ] ++ List.map viewSummary summaries)
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "Country" ]
        , th []
            [ text "Country code" ]
        , th []
            [ text "Slug" ]
        , th []
            [ text "New confirmed" ]
        , th []
            [ text "Total confirmed" ]
        , th []
            [ text "New deaths" ]
        , th []
            [ text "Total deaths" ]
        , th []
            [ text "New recovered" ]
        , th []
            [ text "Total recovered" ]
        ]


viewSummary : Summary -> Html Msg
viewSummary summary =
    tr []
        [ td []
            [ text summary.country ]
        , td []
            [ text summary.countryCode ]
        , td []
            [ text summary.slug ]
        , td []
            [ text (String.fromInt summary.newConfirmed) ]
        , td []
            [ text (String.fromInt summary.totalConfirmed) ]
        , td []
            [ text (String.fromInt summary.newDeaths) ]
        , td []
            [ text (String.fromInt summary.totalDeath) ]
        , td []
            [ text (String.fromInt summary.newRecovered) ]
        , td []
            [ text (String.fromInt summary.totalRecovered) ]
        ]


--ACTUALIZAR
type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List Summary))


summaryDecoder : Decoder Summary
summaryDecoder =
    
    Decode.succeed Summary
        |> required "Country" string
        |> required "CountryCode" string
        |> required "Slug" string
        |> required "NewConfirmed" int
        |> required "TotalConfirmed" int
        |> required "NewDeaths" int
        |> required "TotalDeaths" int
        |> required "NewRecovered" int
        |> required "TotalRecovered" int
        

    

getDatos : Cmd Msg
getDatos =
    Http.get
        { url = "http://localhost:5019/Countries"
        , expect = Http.expectJson DataReceived (list summaryDecoder)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, getDatos )

        DataReceived (Ok summaries) ->
            ( { model
                | summaries = summaries
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
    ( { summaries = []
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