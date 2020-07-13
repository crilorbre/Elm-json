module Covid exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value, style, disabled, selected)
import Http
import Json.Decode as Decode exposing (list, string)
import Json.Decode.Pipeline exposing (optional)

import Paginate exposing (..)


--MODELO
type alias Country =
    { nameCountry : String
    , lastUpdate : String
    , totalCases: String
    , activeCases: String
    , recoveredCases: String
    , deaths: String
    }

type alias Search =
    {
        nameCountry: String
    }

type alias Pagination = 
    {
        pageSize: Int
    }

type alias Model =
    { countries : PaginatedList Country
    , search: Search
    , pagination: Pagination
    , errorMessage : Maybe String
    }


--VISTA
view : Model -> Html Msg
view model =
    div [][
        viewCountriesTableOrError model
        ]


viewCountriesTableOrError : Model -> Html Msg
viewCountriesTableOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewCountriesTable model


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Data could not be recovered at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        , button [ onClick GoBack ]
                    [ text "Try again" ]
        ]


viewCountriesTable : Model -> Html Msg
viewCountriesTable model =
    div []
        [ h2 [style "margin" "50px"] [ text "Summary: COVID-19 Countries" ]
        ,div [style "margin" "150px auto", style "width" "1000px"][
            div[style "margin" "10px"][
                strong [][text "Search country "]
                , input [ placeholder "Country...", value model.search.nameCountry, onInput Change ] [] 
                , button [style "margin" "5px", onClick DoSearch ]
                    [ text "Search" ]
                , button [ onClick GoBack ]
                    [ text "Back" ]
            ]

        ,
            div [style "margin" "10px"]
                [ text "Show "
                , select [ onInput ChangePageSize ]
                    [ option [ value "3" ] [ text "3" ]
                    , option [ value "5", selected True ] [ text "5" ]
                    , option [ value "10" ] [ text "10" ]
                    ]
                , text " countries per page"
                ]

            , table [style "width" "100%", style "text-align" "center", style "border-collapse" "collapse", style "border" "1px solid black"]
            ([ viewTableHeader ] ++ List.map viewCountry (Paginate.page (model.countries)))
        
            
            , div[style "text-align" "center", style "margin" "10px"][
            button [ onClick First, disabled <| Paginate.isFirst model.countries ] [ text "<<" ]
            , button [ onClick Prev, disabled <| Paginate.isFirst model.countries ] [ text "<" ]
            , span [] <| Paginate.elidedPager pagerOptions model.countries
            , button [ onClick Next, disabled <| Paginate.isLast model.countries ] [ text ">" ]
            , button [ onClick Last, disabled <| Paginate.isLast model.countries ] [ text ">>" ]
            ]
        ]
        ]

-- METODOS AUXILIARES PAGINACION

pagerOptions =
            { innerWindow = 1
            , outerWindow = 1
            , pageNumberView = pagerButtonView
            , gapView = text "..."
            }

pagerButtonView: Int-> Bool -> Html Msg
pagerButtonView index isActive =
            button
                [ style "font-weight"
                    (if isActive then
                        "bold"

                     else
                        "normal"
                    )
                , onClick <| GoTo index
                ]
                [ text <| String.fromInt index ]

viewTableHeader : Html Msg
viewTableHeader =
    tr [style "height" "70px", style "font-size" "20px", style "background" "darkorange", style "color" "white"]
        [ th []
            [ text "Country" ]
        , th []
            [ text "Last update" ]
        , th []
            [ text "Total cases" ]
        , th []
            [ text "Actives cases" ]
        , th []
            [ text "Recovered cases" ]
        , th []
            [ text "Deaths" ]
        ]

viewCountry : Country -> Html Msg
viewCountry country =
    tr [style "height" "50px"]
        [ td []
            [ text country.nameCountry ]
        , td []
            [ text country.lastUpdate ]
        , td []
            [ text country.totalCases ]
        , td []
            [ text country.activeCases ]
        , td []
            [ text country.recoveredCases ]
        , td []
            [ text country.deaths ]
        ]


--ACTUALIZAR
type Msg
    = DataReceived (Result Http.Error (List Country))
    | Change String
    | DoSearch
    | SearchCompleted (Result Http.Error Country)
    | GoBack
    | Next
    | Prev
    | First
    | Last
    | GoTo Int
    | ChangePageSize String


{- 
    Decodificador de JSON: Convierte la lista de objetos JSON
    en la lista de Countries
-}
listCountryDecoder : Decode.Decoder (List Country)
listCountryDecoder = 
    Decode.list (Decode.succeed Country 
        |> optional "Country_text" string "N/A"
        |> optional "Last Update" string "N/A"
        |> optional "Total Cases_text" string "N/A"
        |> optional "Active Cases_text" string "N/A"
        |> optional "Total Recovered_text" string "N/A"
        |> optional "Total Deaths_text" string "N/A")


{- 
    Decodificador de JSON: Convierte un único objeto JSON
    en un objeto "Country"
-}
countryDecoder : Decode.Decoder Country
countryDecoder = 
    Decode.succeed Country
        |> optional "Country_text" string "N/A"
        |> optional "Last Update" string "N/A"
        |> optional "Total Cases_text" string "N/A"
        |> optional "Active Cases_text" string "N/A"
        |> optional "Total Recovered_text" string "N/A"
        |> optional "Total Deaths_text" string "N/A"
    

--HTTP


{- 
    Peticion GET para obtener los datos de 
    todos los paises
-}
getDatas : Cmd Msg
getDatas =
    Http.get
        { url = "https://covid-19.dataflowkit.com/v1"
        , expect = Http.expectJson DataReceived listCountryDecoder
        }


{- 
    Peticion GET para obtener toda la información de
    un continente especificado por parametro
-}
getDatasByCountry: String -> Cmd Msg
getDatasByCountry country = 
        Http.get
                { url = "https://covid-19.dataflowkit.com/v1/"++country
                , expect = Http.expectJson SearchCompleted countryDecoder
                }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
    
        {- 
            Si la peticion se ha realizado correctamente, añado
            la lista a mi lista de paises del modelo
        -}
        DataReceived (Ok countries) ->
            ( { model
                | countries = fromList model.pagination.pageSize countries
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
        Change country ->
            ({model|search = updateSearch country}, Cmd.none)


        {- 
            Al pulsar sobre el botón "Buscar", se realizara la llamada
            al metodo al metodo donde obteniamos los datos por el pais
            especificado en el input
        -}
        DoSearch ->
            {-
                Si el atributo "nameCountry" del modelo Search no esta
                vacio, obtenemos los datos del pais especificado
            -}
            if model.search.nameCountry /= "" then
                ( model, getDatasByCountry model.search.nameCountry)
            
            {-
                Si el campo "nameCountry" del modelo Search esta
                vacio, llamamos al metodo para obtener todos
                los datos
            -}
            else
                ( model, getDatas)

        {- 
            Si he recibido los datos correctamente, añado
            el pais obtenido a la lista vacia
        -}
        SearchCompleted (Ok country) ->
            ( { model
                | countries = fromList 1 (country::[])
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
            ( {model|search = updateSearch ""}
            , getDatas
            )

        {- 
            Paginacion: Ir al numero de pagina seleccionado
        -}
        GoTo index ->
            ({ model | countries = Paginate.goTo index model.countries }, Cmd.none)

        {- 
            Paginacion: Ir a la siguiente pagina
        -}
        Next ->
            ({ model | countries = Paginate.next model.countries }, Cmd.none)


        {- 
            Paginacion: Ir a la pagina anterior
        -}
        Prev ->
            ({ model | countries = Paginate.prev model.countries }, Cmd.none)

        {- 
            Paginacion: Ir a la primera pagina
        -}
        First ->
            ({ model | countries = Paginate.first model.countries }, Cmd.none)

        {- 
            Paginacion: Ir a la ultima pagina
        -}
        Last ->
            ({ model | countries = Paginate.last model.countries }, Cmd.none)

        {- 
            Paginacion: Cambiar el numero de
            paises que se muestran en la lista y actualiza
            el campo "pageSize" del modelo "Pagination"
        -}
        ChangePageSize size ->
            let
                sizeAsInt =
                    Maybe.withDefault 10 <| String.toInt size
            in
            ({ model | countries = Paginate.changeItemsPerPage sizeAsInt model.countries , pagination = updatePagination sizeAsInt }, Cmd.none)


-- METODOS AUXILIARES


{- 
    Metodo para actualizar el campo 
    de Search con el valor "string"
-}
updateSearch: String -> Search
updateSearch string =   
    Search string

updatePagination: Int -> Pagination
updatePagination size =   
    Pagination size


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
    ( { countries = fromList 0 []
      , search = Search ""
      , pagination = Pagination 5
      , errorMessage = Nothing
      }
    , getDatas
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }