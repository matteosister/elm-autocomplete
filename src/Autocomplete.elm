module Autocomplete exposing (..)

import Html exposing (Html, div, input, tr, td, text, a)
import Html.Attributes exposing (href, value)
import Html.Events exposing (onInput, onClick)


type State
    = State String


initialState : State
initialState =
    State ""


type Config msg data
    = Config
        { toMsg : String -> State -> msg
        , selectedValue : String -> msg
        , readValue : data -> String
        }


config :
    { toMsg : String -> State -> msg
    , selectedValue : String -> msg
    , readValue : data -> String
    }
    -> Config msg data
config { toMsg, selectedValue, readValue } =
    Config
        { toMsg = toMsg
        , selectedValue = selectedValue
        , readValue = readValue
        }


view : Config msg data -> State -> List data -> Html msg
view ((Config { toMsg }) as config) (State val) suggestions =
    div []
        [ input
            [ onInput
                (\inputValue -> toMsg inputValue (State inputValue))
            , value val
            ]
            []
        , viewSuggestions config suggestions
        ]


viewSuggestions : Config msg data -> List data -> Html msg
viewSuggestions config suggestions =
    div [] (List.map (showSuggestion config) suggestions)


showSuggestion : Config msg data -> data -> Html msg
showSuggestion (Config { readValue, selectedValue }) data =
    tr []
        [ td [] [ a [ onClick <| selectedValue (readValue data), href "#" ] [ text <| readValue data ] ]
        ]
