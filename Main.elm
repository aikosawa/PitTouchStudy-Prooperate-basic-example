module Main exposing (..)

import Browser
import Html exposing (Html, div, p, text, ul, li)
import Html.Attributes exposing (id)
import Http
import Http.Tasks
import Json.Decode as Json
import Hex
import Hex.Convert
import Bytes
import Bytes.Decode
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
    , logs : List TouchLog
    }


type alias TouchLog =
    { idm : String
    , posix : Maybe Time.Posix
    , zone : Maybe Time.Zone
    }


userTouchLog =
    { idm = ""
    , posix = Nothing
    , zone = Nothing
    }


defaultTouchLog =
    { idm = ""
    , posix = Nothing
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



-- 入室、退室数計算

-- groupEachIdm : List TouchLog -> Dict String Int
groupEachIdm data =
    Dict.Extra.groupBy .idm data
        |> Dict.map (\_ v -> List.length v )
{-| 
@docs Dict [(idm, [{userTouchLog}])]
@docs Dict [(idm, dataLength)]
-}

-- transformToCounts : Dict String Int -> List (String, (Int, Int))      
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



-- TouchLog 操作関数

appendLog : List TouchLog -> List TouchLog -> List TouchLog
appendLog logs touchLog = 
    logs ++ touchLog

-- getLastTouchLog : List TouchLog -> TouchLog
getLastTouchLog logs = 
    List.reverse logs
        |> List.head
        |> Maybe.withDefault defaultTouchLog




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
            in

            ( { model
                | touch = Just touch
                , logs = appendLog model.logs [TouchLog (Maybe.withDefault "" touch.idm) (Just posix) (Just zone)]
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

        deposit =
            model.touch
                |> Maybe.andThen .data
                |> Maybe.map (String.slice 20 24)
                |> Maybe.andThen Hex.Convert.toBytes
                |> Maybe.andThen (Bytes.Decode.decode (Bytes.Decode.unsignedInt16 Bytes.LE))
                |> Maybe.withDefault 0

        formattedTime : List TouchLog -> String
        formattedTime logs = 
            Just Time.Format.format
                |> Maybe.Extra.andMap (Just Time.Format.Config.Config_ja_jp.config)
                |> Maybe.Extra.andMap (Just "%Y-%m-%d %H:%M:%S")
                |> Maybe.Extra.andMap (getLastTouchLog logs).zone
                |> Maybe.Extra.andMap (getLastTouchLog logs).posix
                |> Maybe.withDefault ""

        entryTimes : List TouchLog -> String
        entryTimes logs = 
            groupEachIdm logs
                |> transformToCounts
                |> Dict.get (getLastTouchLog logs).idm
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0
                |> String.fromInt

        exitTimes : List TouchLog -> String
        exitTimes logs =
            groupEachIdm logs
                |> transformToCounts
                |> Dict.get (getLastTouchLog logs).idm
                |> Maybe.map Tuple.second
                |> Maybe.withDefault 0
                |> String.fromInt

        totalTouchCounts : List TouchLog -> String
        totalTouchCounts  logs =
            groupEachIdm logs
                |> Dict.get (getLastTouchLog logs).idm
                |> Maybe.withDefault 0
                |> String.fromInt

        entredNumbers : List TouchLog -> String
        entredNumbers logs =
            groupEachIdm logs
                |> transformToCounts
                |> totalEntExiCount
                |> Tuple.first
                |> String.fromInt

        exitedNumbers : List TouchLog -> String
        exitedNumbers logs =
            groupEachIdm logs
                |> transformToCounts
                |> totalEntExiCount
                |> Tuple.second
                |> String.fromInt
        
        viewTouchLog : TouchLog -> Html msg
        viewTouchLog log =
            let
                time = 
                    Just Time.Format.format
                        |> Maybe.Extra.andMap (Just Time.Format.Config.Config_ja_jp.config)
                        |> Maybe.Extra.andMap (Just "%Y-%m-%d %H:%M:%S")
                        |> Maybe.Extra.andMap log.zone
                        |> Maybe.Extra.andMap log.posix
                        |> Maybe.withDefault ""
            in
            li [] [ text <| "IDM : " ++ log.idm ++ " TIME : " ++ time ]
    in
    div [ id "body" ]
        [ div [] [ p [] [ text "Main" ] ]
        , div [] [ p [] [ label ] ]
        , div [] [ p [] [ text <| "touch at : " ++ formattedTime model.logs ]]
        , div [] [ p [] [ (text << String.fromInt) deposit ]]
        , div [] [ p [] [ text <| "Enter : " ++ entryTimes model.logs ++ " times "
                        , text <| "Exit : " ++ exitTimes model.logs ++ " times "
                        , text <| "Total Counts : " ++ totalTouchCounts model.logs ++ " times "
                        ]]
        , div [] [ p [] [ text <| "Entred Numbers : " ++ entredNumbers model.logs ++ " times "
                        , text <| "Exited Numbers : " ++ exitedNumbers model.logs ++ " times "]]
        , div [] [ ul [] ( List.map viewTouchLog model.logs)] 
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
