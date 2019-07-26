port module DevTools exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Url exposing (Url)
import Html exposing (..)
import Html.Events exposing (onClick)
import List
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)


type Msg model msg
    = SetModel model
    | RunMsg msg


type alias Model model =
    { model : model }


type alias Encoder m =
    m -> Value


type alias Init flags model msg =
    flags -> ( model, Cmd msg )


type alias ApplicationInit flags model msg =
    flags -> Url -> Navigation.Key -> ( model, Cmd msg )


type alias Update model msg =
    msg -> model -> ( model, Cmd msg )


type alias SandboxUpdate model msg =
    msg -> model -> model


type alias View model msg =
    model -> Html msg


type alias ViewDocument model msg =
    model -> Document msg


type alias Subscriptions model msg =
    model -> Sub msg


type alias Exporter model msg =
    { encodeModel : Encoder model
    , decodeModel : Decoder model
    , encodeMsg : Encoder msg
    }


type alias Action model msg =
    { model : model
    , message : Maybe msg
    }


encodeAction : Exporter model msg -> Action model msg -> Value
encodeAction export action =
    Encode.object
        [ ( "model", export.encodeModel action.model )
        , ( "message"
          , action.message
                |> Maybe.map export.encodeMsg
                |> Maybe.withDefault Encode.null
          )
        ]


worker :
    { init : Init flags model msg
    , update : Update model msg
    , subscriptions : Subscriptions model msg
    , export : Exporter model msg
    }
    -> Program flags (Model model) (Msg model msg)
worker cfg =
    Platform.worker
        { init = init cfg.export cfg.init
        , update = update cfg.export cfg.update
        , subscriptions = subscriptions cfg.export cfg.subscriptions
        }


sandbox :
    { init : model
    , view : View model msg
    , update : SandboxUpdate model msg
    , export : Exporter model msg
    }
    -> Program () (Model model) (Msg model msg)
sandbox cfg =
    Browser.element
        { init = init cfg.export (\_ -> ( cfg.init, Cmd.none ))
        , view = view cfg.view
        , update = update cfg.export (\msg model -> ( cfg.update msg model, Cmd.none ))
        , subscriptions = subscriptions cfg.export (\_ -> Sub.none)
        }


element :
    { init : Init flags model msg
    , view : View model msg
    , update : Update model msg
    , subscriptions : Subscriptions model msg
    , export : Exporter model msg
    }
    -> Program flags (Model model) (Msg model msg)
element cfg =
    Browser.element
        { init = init cfg.export cfg.init
        , view = view cfg.view
        , update = update cfg.export cfg.update
        , subscriptions = subscriptions cfg.export cfg.subscriptions
        }


document :
    { init : Init flags model msg
    , view : model -> Browser.Document msg
    , update : Update model msg
    , subscriptions : Subscriptions model msg
    , export : Exporter model msg
    }
    -> Program flags (Model model) (Msg model msg)
document cfg =
    Browser.document
        { init = init cfg.export cfg.init
        , view = viewDocument cfg.view
        , update = update cfg.export cfg.update
        , subscriptions = subscriptions cfg.export cfg.subscriptions
        }


application :
    { init : ApplicationInit flags model msg
    , view : model -> Document msg
    , update : Update model msg
    , subscriptions : Subscriptions model msg
    , export : Exporter model msg
    , onUrlRequest : UrlRequest -> msg
    , onUrlChange : Url -> msg
    }
    -> Program flags (Model model) (Msg model msg)
application cfg =
    Browser.application
        { init = initApplication cfg.export cfg.init
        , view = viewDocument cfg.view
        , update = update cfg.export cfg.update
        , subscriptions = subscriptions cfg.export cfg.subscriptions
        , onUrlChange = \u -> RunMsg (cfg.onUrlChange u)
        , onUrlRequest = \u -> RunMsg (cfg.onUrlRequest u)
        }


init : Exporter model msg -> Init flags model msg -> Init flags (Model model) (Msg model msg)
init export initializer flags =
    let
        ( model, cmd ) =
            initializer flags
    in
        ( Model model
        , Cmd.batch
            [ Cmd.map RunMsg cmd
            , toDevTools <|
                encodeAction export { model = model, message = Nothing }
            ]
        )


initApplication : Exporter model msg -> ApplicationInit flags model msg -> ApplicationInit flags (Model model) (Msg model msg)
initApplication export initializer flags url key =
    let
        ( model, cmd ) =
            initializer flags url key
    in
        ( Model model
        , Cmd.batch
            [ Cmd.map RunMsg cmd
            , toDevTools <|
                encodeAction export { model = model, message = Nothing }
            ]
        )


update : Exporter model msg -> Update model msg -> Update (Model model) (Msg model msg)
update export updater msg { model } =
    case msg of
        -- Replace the application's model
        SetModel imodel ->
            ( Model imodel
            , Cmd.none
            )

        RunMsg imsg ->
            let
                ( updated, cmd ) =
                    updater imsg model
            in
                ( Model updated
                , Cmd.batch
                    [ Cmd.map RunMsg cmd
                    , toDevTools <|
                        encodeAction export { model = model, message = Just imsg }
                    ]
                )


view : View model msg -> View (Model model) (Msg model msg)
view viewer { model } =
    viewer model
        |> Html.map RunMsg


viewDocument : ViewDocument model msg -> ViewDocument (Model model) (Msg model msg)
viewDocument viewer { model } =
    let
        doc =
            viewer model
    in
        { title = doc.title
        , body = List.map (Html.map RunMsg) doc.body
        }


subscriptions : Exporter model msg -> Subscriptions model msg -> Subscriptions (Model model) (Msg model msg)
subscriptions export subscriber { model } =
    Sub.batch
        [ Sub.map RunMsg (subscriber model)
        , fromDevTools
            (\val ->
                case Decode.decodeValue export.decodeModel val of
                    Err _ ->
                        SetModel model

                    Ok new ->
                        SetModel new
            )
        ]


port toDevTools : Value -> Cmd msg


port fromDevTools : (Value -> msg) -> Sub msg
