port module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import DevTools
import Browser.Navigation as Navigation
import Url exposing (Url)
import Html exposing (..)
import Html.Events exposing (onClick)
import List
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)


type alias Flags =
    ()


type alias Model =
    Int


encodeModel : Model -> Value
encodeModel =
    Encode.int


decodeModel : Decoder Model
decodeModel =
    Decode.int


type Msg
    = Increment
    | Decrement
    | Reset


encodeMsg : Msg -> Value
encodeMsg msg =
    case msg of
        Increment ->
            Encode.object
                [ ( "type", Encode.string "INCREMENT" ) ]

        Decrement ->
            Encode.object
                [ ( "type", Encode.string "DECREMENT" ) ]

        Reset ->
            Encode.object
                [ ( "type", Encode.string "RESET" ) ]


initialModel : Model
initialModel =
    0


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , text (String.fromInt model)
        , button [ onClick Increment ] [ text "+" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1
            , Cmd.none
            )

        Decrement ->
            ( model - 1
            , Cmd.none
            )

        Reset ->
            ( initialModel
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    DevTools.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , export =
            { encodeModel = encodeModel
            , decodeModel = decodeModel
            , encodeMsg = encodeMsg
            }
        }
