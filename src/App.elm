module App exposing (..)

import Html exposing (Html, text, div, img)
import Autocomplete exposing (view)
import Json.Decode exposing (Decoder, list, at, string)
import Json.Decode.Pipeline exposing (decode, required, requiredAt)
import Http


type alias Model =
    { autocompleteState : Autocomplete.State
    , suggestions : List Suggestion
    , starWarsPerson : Maybe String
    }


init : String -> ( Model, Cmd Msg )
init path =
    { autocompleteState = Autocomplete.initialState
    , suggestions = []
    , starWarsPerson = Nothing
    }
        ! []


type Msg
    = AutocompleteSetState String Autocomplete.State
    | AutocompleteSelectedValue String
    | NewSuggestions (Result Http.Error (List Suggestion))


type alias Suggestion =
    { label : String
    , value : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        AutocompleteSetState inputValue autocompleteState ->
            { model | autocompleteState = autocompleteState } ! [ doSearch inputValue ]

        AutocompleteSelectedValue value ->
            { model
                | starWarsPerson = Just value
                , suggestions = []
                , autocompleteState = Autocomplete.initialState
            }
                ! []

        NewSuggestions (Ok suggestions) ->
            { model | suggestions = suggestions } ! []

        NewSuggestions (Err _) ->
            model ! []


view : Model -> Html Msg
view ({ autocompleteState, suggestions, starWarsPerson } as model) =
    let
        autocompleteConfig =
            Autocomplete.config
                { toMsg = AutocompleteSetState
                , selectedValue = AutocompleteSelectedValue
                , readValue = .label
                }
    in
        div []
            [ starWarsPerson
                |> Maybe.map (\v -> text v)
                |> Maybe.withDefault (text "")
            , Autocomplete.view autocompleteConfig autocompleteState suggestions
            ]


doSearch : String -> Cmd Msg
doSearch input =
    let
        url =
            "https://swapi.co/api/people/?search=" ++ input

        req =
            Http.get url decoderSuggestions
    in
        Http.send NewSuggestions req


decoderSuggestions : Decoder (List Suggestion)
decoderSuggestions =
    at [ "results" ] (list decoderSuggestion)


decoderSuggestion : Decoder Suggestion
decoderSuggestion =
    decode Suggestion
        |> required "name" string
        |> required "url" string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
