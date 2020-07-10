module Covid exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value)
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

type alias Search =
    {
        continent: String
    }

type alias Model =
    { continents : List Continent
    , search: Search
    , errorMessage : Maybe String
    }


--VISTA
view : Model -> Html Msg
view model =
    div [][
        viewContinentOrError model
        , input [ placeholder "", value model.search.continent, onInput Change ] [] 
        , button [ onClick DoSearch ]
            [ text "Buscar" ]
        , button [ onClick GoBack ]
            [ text "Back" ]
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
            "No se pudieron recuperar los datos en este momento."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewContinents : List Continent -> Html Msg
viewContinents continents =
    div []
        [ h2 [] [ text "Continentes" ]
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
    = DataReceived (Result Http.Error (List Continent))
    | Change String
    | DoSearch
    | SearchCompleted (Result Http.Error Continent)
    | GoBack


{- 
    Decodificador de JSON: Convierte la lista de objetos JSON
    en la lista de Continent
-}
listContinentDecoder : Decode.Decoder (List Continent)
listContinentDecoder = 
    Decode.list (Decode.succeed Continent
        |> required "continent" string
        |> required "population" int
        |> required "cases" int
        |> required "active" int
        |> required "recovered" int
        |> required "deaths" int)


{- 
    Decodificador de JSON: Convierte un único objeto JSON
    en un Continent
-}
continentDecoder : Decode.Decoder Continent
continentDecoder = 
    Decode.succeed Continent
        |> required "continent" string
        |> required "population" int
        |> required "cases" int
        |> required "active" int
        |> required "recovered" int
        |> required "deaths" int
    

{- 
    Peticion GET para obtener los datos de 
    todos los contienentes
-}
getDatos : Cmd Msg
getDatos =
    Http.get
        { url = "https://corona.lmao.ninja/v2/continents?yesterday=true&sort"
        , expect = Http.expectJson DataReceived listContinentDecoder
        }


{- 
    Peticion GET para obtener toda la información de
    un continente especificado por parametro
-}
getDatosPorContinente: String -> Cmd Msg
getDatosPorContinente continent = 

    if String.toLower continent == "australia" || String.toLower continent == "oceania" || String.toLower continent == "australia/oceania" then
        Http.get
            { url = "https://corona.lmao.ninja/v2/continents/"++"Australia%2Foceania"++"?yesterday&strict"
            , expect = Http.expectJson SearchCompleted continentDecoder
            }
    else
        Http.get
                { url = "https://corona.lmao.ninja/v2/continents/"++continent++"?yesterday&strict"
                , expect = Http.expectJson SearchCompleted continentDecoder
                }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
    
        {- 
            Si he recibido los datos correctamente, añado
            la lista a mi lista de continentes del modelo
        -}
        DataReceived (Ok continents) ->
            ( { model
                | continents = continents
                , errorMessage = Nothing
              }
            , Cmd.none
            )


        {- 
            Si ha ocurrido algun error durante el proceso
            añado el mensaje de error al campo errorMessage
            del modelo
        -}
        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )
        

        {- 
            Si el input cambia de valor, model.search
            es actualizado con ese nuevo valor
        -}
        Change continent ->
            ({model|search = updateSearch continent}, Cmd.none)


        {- 
            Al pulsar sobre el botón "Buscar", se realizara la llamada
            al metodo al metodo donde obteniamos los datos por el continente
            especificado en el input
        -}
        DoSearch ->
            {-
                Si el campo contienent del modelo Search no esta
                vacio, obtenemos los datos del continente especificado
            -}
            if model.search.continent /= "" then
                ( model, getDatosPorContinente model.search.continent)
            
            {-
                Si el campo contienent del modelo Search esta
                vacio, llamamos al metodo para obtener todos
                los datos
            -}
            else
                ( model, getDatos)

        {- 
            Si he recibido los datos correctamente, añado
            el continentes obtenido a la lista vacia
        -}
        SearchCompleted (Ok continent) ->
            ( { model
                | continents = continent :: []
                , errorMessage = Nothing
              }
            , Cmd.none
            )


        {- 
            Si ha ocurrido algun error durante el proceso
            añado el mensaje de error al campo errorMessage
            del modelo
        -}
        SearchCompleted (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )


        {- 
            Utilizado para volver hacia atrás cuando
            se haya realizado una busqueda
        -}
        GoBack ->
            ( { continents = []
            , search = Search ""
            , errorMessage = Nothing
            }
            , getDatos
            )


-- METODOS AUXILIARES


{- 
    Metodo para actualizar el campo 
    de Search con el valor "string"
-}
updateSearch: String -> Search
updateSearch string =   
    Search string


{- 
    Metodo para obtener un mensaje de error
    dependiento del tipo de error HTTP que
    haya ocurrido
-}
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


-- INIT

{-
    Iniciamos la aplicacion inicializacion el modelo
    y realizando una llamada al metodo "getDatos" para obtener
    los datos del servidor
-}
init : () -> ( Model, Cmd Msg )
init _ =
    ( { continents = []
      , search = Search ""
      , errorMessage = Nothing
      }
    , getDatos
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }