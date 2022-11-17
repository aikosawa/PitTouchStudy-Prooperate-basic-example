module Main exposing (AccessLog)

import Browser
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (id)
import Http
import Http.Tasks
import Json.Decode as Json
import Hex
import Maybe exposing (Maybe)
import ProOperate
import ProOperate.Card as Card
import ProOperate.Config as Config exposing (Config_pro2, defaultConfig_pro2)
import ProOperate.Touch as Touch exposing (TouchResponse)
import Procedure
import Procedure.Program
import Result.Extra
import Maybe.Extra
import Task exposing (Task)
import Dict exposing (Dict)
import Dict.Extra
import Task.Extra
import Time
import Time.Format
import Time.Format.Config.Config_ja_jp


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
    | OnTouch TouchResponse Time.Zone Time.Posix



-- Application Model

type alias Model =
    { procModel : Procedure.Program.Model Msg
    , config : Config_pro2
    , touch : Maybe TouchResponse
    , logs : List AccessLog
    }


type alias AccessLog =
    { idm : Maybe String
    , time : Maybe Time.Posix
    , zone : Maybe Time.Zone
    }


userAccessLog =
    { idm = Nothing
    , time = Nothing
    , zone = Nothing
    }


defaultAccessLog =
    { idm = Nothing
    , time = Nothing
    , zone = Nothing
    }


type alias Flags =
    {}



-- FUNCTIONS

init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { procModel = Procedure.Program.init
      , config = defaultConfig_pro2
      , touch = Nothing
      , logs = []
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


formatTime : Time.Zone -> Time.Posix -> String
formatTime = Time.Format.format Time.Format.Config.Config_ja_jp.config "%Y-%m-%d %H:%M:%S"


-- 入室、退室数計算

takeLatestIdm data =
    List.reverse data
        |> List.head
        |> Maybe.withDefault defaultAccessLog
        |> .idm


groupEachIdm data =
    Dict.Extra.groupBy .idm data
        |> Dict.map (\_ v -> List.length v )
{-| 
@docs Dict [(idm, [{userAccessLog}])]
@docs Dict [(idm, dataLength)]
-}
        
transformToCounts data =
    Dict.map (\_ v -> (( v + 1 )//2, v//2)) data
{-| 入退室カウント取得
@docs Dict [(idm, (入室回数, 退室回数))]
 -}

totalEntExiCount data =
    Dict.values data
        |> List.unzip
        |> Tuple.mapBoth List.sum List.sum
{-|
@docs [(入室回数, 退室回数)]
@docs ([入室回数], [退室回数])
@docs (入室総数, 退室総数)
 -}



-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            case msg of
                ProcMsg _ ->
                    msg

                _ ->
                    Debug.log "update:" msg
    in
    case msg of
        ProcMsg pMsg ->
            Procedure.Program.update pMsg model.procModel
                |> Tuple.mapFirst (\updated -> { model | procModel = updated })

        OnError err ->
            ( model, Cmd.none )

        OnTouch touch zone posix ->
            let
                _= Debug.log "model:" model
                _= Debug.log "touch:" touch
                _= Debug.log "newLog:" newLog
                _= Debug.log "date:" date

                date : String
                date = Maybe.map2 formatTime (Just zone) (Just posix)
                    |> Maybe.withDefault ""

                newLog : AccessLog
                newLog = { userAccessLog
                            | idm = touch.idm
                            , time = Just posix
                            , zone = Just zone
                        }

                updateLogs : List AccessLog
                updateLogs = model.logs ++ [AccessLog touch.idm (Just posix) (Just zone)]
            in

            ( { model
                | touch = Just touch
                , logs = updateLogs
                 }
            , observeTouchCmd model.config
            )

        GotSetting setting ->
            let
                config =
                    configFromSetting setting
            in
            ( { model | config = config }
            , observeTouchCmd config
            )



-- VIEW

view : Model -> Html Msg
view model =
    let
        _= Debug.log "logs:" model.logs

        label =
            Maybe.andThen .idm model.touch
                |> Maybe.withDefault ""
                |> text

        -- date = 
        --     Maybe.map2 formatTime model.zone model.time
        --     |> Maybe.withDefault ""

        data =
            Maybe.andThen .data model.touch

        deposit =
            Just (++) 
                |> Maybe.Extra.andMap (Maybe.map (String.slice 22 24) data)
                |> Maybe.Extra.andMap (Maybe.map (String.slice 20 22) data)
                |> Maybe.withDefault ""
                |> Hex.fromString
                |> Result.withDefault 0
        
        --EnterCount = 
            --groupEachIdm model.logs
                -- |>
    in
    div [ id "body" ]
        [ div [] [ p [] [ text "Main" ] ]
        , div [] [ p [] [ label ] ]
        -- , div [] [ p [] [ text <| "touch at : " ++ date ]]
        , div [] [ p [] [ (text << String.fromInt) deposit ]]
        , div [] [ p [] [ text <| "Enter : " ]]
        ]



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Procedure.Program.subscriptions model.procModel
        ]


{-| -}
observeTouchCmd : Config_pro2 -> Cmd Msg
observeTouchCmd config =
    let
        andMap =
            Procedure.map2 (|>)
    in
    Procedure.provide OnTouch
        |> andMap (Touch.observeOnce_pro2 config)
        |> andMap (Procedure.fromTask Time.here)
        |> andMap (Procedure.fromTask Time.now)
        |> Procedure.try ProcMsg
            (Result.Extra.extract (OnError << ProOperateError))
