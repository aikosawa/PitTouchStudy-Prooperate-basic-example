module Main exposing (..)

import Browser
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (id)
import Http
import Http.Tasks
import Json.Decode as Json
import Maybe exposing (Maybe)
import ProOperate
import ProOperate.Card as Card
import ProOperate.Config as Config exposing (Config_pro2, defaultConfig_pro2)
import ProOperate.Touch as Touch exposing (TouchResponse)
import Procedure
import Procedure.Program
import Result.Extra
import Task exposing (Task)
import Time


type Error
    = HttpError Http.Error
    | DecodeError String
    | ProOperateError ProOperate.Error


type alias Setting =
    { name : String
    }



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = ProcMsg (Procedure.Program.Msg Msg)
    | GotSetting Setting
    | OnError Error
    | OnTouch TouchResponse



-- Application Model


type alias Model =
    { procModel : Procedure.Program.Model Msg
    , config : Config_pro2
    , touch : Maybe TouchResponse
    }


type alias Flags =
    {}



-- FUNCTIONS


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { procModel = Procedure.Program.init
      , config = defaultConfig_pro2
      , touch = Nothing
      }
    , getProviderSettingCmd
    )


getProviderSetting : Task Error Setting
getProviderSetting =
    Http.Tasks.get
        { url = "http://localhost/providersetting.json"
        , resolver = Http.Tasks.resolveString
        }
        |> Task.mapError HttpError
        |> Task.map (Json.decodeString settingDecoder)
        |> Task.map (Result.mapError Json.errorToString)
        |> Task.andThen
            (Result.Extra.unpack (DecodeError >> Task.fail) Task.succeed)


settingDecoder : Json.Decoder Setting
settingDecoder =
    let
        decoder =
            Json.map Setting
                (Json.field "name" Json.string)
    in
    Json.field "settings" decoder


getProviderSettingCmd : Cmd Msg
getProviderSettingCmd =
    let
        toMsg =
            Result.Extra.unpack OnError GotSetting
    in
    getProviderSetting
        |> Task.attempt toMsg


configFromSetting : Setting -> Config_pro2
configFromSetting setting =
    defaultConfig_pro2
        |> (\r -> { r | felicaList = [ Card.sapica, Card.suica, Card.felica ] })
        |> (\r -> { r | waitLamp = "WW1L" })



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "update:" msg
    in
    case msg of
        ProcMsg pMsg ->
            Procedure.Program.update pMsg model.procModel
                |> Tuple.mapFirst (\updated -> { model | procModel = updated })

        OnError err ->
            ( model, Cmd.none )

        OnTouch touch ->
            ( { model | touch = Just touch }, Cmd.none )

        GotSetting setting ->
            let
                config =
                    configFromSetting setting

                toMsg =
                    Result.Extra.unpack (ProOperateError >> OnError) OnTouch
            in
            ( { model | config = config }
            , Touch.observeOnce_pro2 config
                |> Procedure.try ProcMsg toMsg
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        label =
            Maybe.andThen .idm model.touch
                |> Maybe.withDefault ""
                |> text
    in
    div [ id "body" ]
        [ div [] [ p [] [ text "Main" ] ]
        , div [] [ p [] [ label ] ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Procedure.Program.subscriptions model.procModel
        ]