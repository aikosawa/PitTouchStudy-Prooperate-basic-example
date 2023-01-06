module Main exposing (..)

import Browser
import Html exposing (Html, div, p, text, ul, li)
import Html.Attributes exposing (id)
import Http
import Http.Tasks
import Json.Decode
import Hex
import Hex.Convert
import Bytes
import Bytes.Decode
import ProOperate
import ProOperate.Card as Card
import ProOperate.Config as Config exposing (Config_pro2, defaultConfig_pro2)
import ProOperate.Touch as Touch exposing (TouchResponse)
import Procedure
import Procedure.Program
import Dict
import Dict.Extra
import Maybe exposing (Maybe)
import Maybe.Extra
import Task exposing (Task)
import Task.Extra
import Result.Extra
import List.Extra
import Time exposing (ZoneName(..))
import Time.Format
import Time.Format.Config.Config_ja_jp
import TimeZone


type Error
    = HttpError Http.Error
    | DecodeError String
    | ProOperateError ProOperate.Error


type alias Setting =
    { name : String
    , waitLamp : String
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
    | OnTouch TouchResponse -- Time.Posix, Time.Zone の変換用分岐
    | OnTouchWithTime TouchResponse (Maybe String) Int



-- Application Model


type alias Model =
    { procModel : Procedure.Program.Model Msg
    , config : Config_pro2
    , history : List TouchData
    }


type alias TouchData =
    { data : Maybe String   -- ICカードの残高
    , idm : String
    , zoneName : String
    , millis : Int
    }


userTouchData =
    { data = Nothing
    , idm = ""
    , zoneName = ""
    , millis = 0
    }


defaultTouchData =
    { data = Nothing
    , idm = ""
    , zoneName = ""
    , millis = 0
    }


type alias Flags =
    {}



-- FUNCTIONS


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { procModel = Procedure.Program.init
      , config = defaultConfig_pro2
      , history = []
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
        |> Task.map (Json.Decode.decodeString settingDecoder)
        |> Task.map (Result.mapError Json.Decode.errorToString)
        |> Task.andThen
            (Result.Extra.unpack (DecodeError >> Task.fail) Task.succeed)


settingDecoder : Json.Decode.Decoder Setting
settingDecoder =
    let
        decoder =
            Json.Decode.map2 Setting
                (Json.Decode.field "name" Json.Decode.string)
                (Json.Decode.field "waitLamp" Json.Decode.string)
    in
    Json.Decode.field "settings" decoder


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
        |> (\r -> { r | waitLamp = setting.waitLamp })


-- TouchData 操作関数
-- getLastTouchData : List TouchData -> TouchData
getLastTouchData data = 
    List.head data
        |> Maybe.withDefault defaultTouchData



-- UPDATE


filterHistoryByIdm : String -> List TouchData -> List TouchData
filterHistoryByIdm idm =
    List.filter (\a -> a.idm == idm)


filterHistoryByMSec : Int -> List TouchData -> List TouchData
filterHistoryByMSec millis =
    List.filter (\a -> a.millis > millis)


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

        OnTouch touch ->
            let
                andMap =
                    Task.Extra.andMap

                extractName zoneName =
                    case zoneName of
                        Name name ->
                            Just name

                        _ ->
                            Nothing
            in
            ( model
            , Task.succeed (OnTouchWithTime touch)
                |> andMap (Task.map extractName Time.getZoneName)
                |> andMap (Task.map Time.posixToMillis Time.now)
                |> Task.perform identity
            )

        OnTouchWithTime touch zoneName millis ->
            let
                validateTime ms =   -- 10秒はじく
                    Maybe.map2 filterHistoryByIdm touch.idm (Just model.history)
                        |> Maybe.map (filterHistoryByMSec (ms - 10 * 1000))
                        |> Maybe.map (List.sortBy (.millis >> (-) ms))
                        |> Maybe.andThen List.head
                        |> Maybe.Extra.unwrap (Just millis) (always Nothing)

                appendHistory newData = -- Log追加
                    newData :: model.history

                history =
                    Just (TouchData touch.data)
                        |> Maybe.Extra.andMap touch.idm
                        |> Maybe.Extra.andMap zoneName
                        |> Maybe.Extra.andMap (validateTime millis)
                        |> Maybe.Extra.unwrap model.history appendHistory
            in
            ( { model | history = history }
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


-- 入室、退室数計算
groupEachIdm data =
    Dict.Extra.groupBy .idm data
        |> Dict.map (\_ v -> List.length v )
{-| 
@docs Dict [(idm, [{userTouchData}])]
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


formatTime : Maybe TouchData -> String
formatTime data =
    let
        nameToZone zoneName =
            Dict.get zoneName TimeZone.zones

        touchToZone touchData =
            Maybe.map .zoneName touchData
                |> Maybe.andThen nameToZone
                |> Maybe.map (\zone -> zone ())

        touchToPosix touchData =
            Maybe.map .millis touchData
                |> Maybe.map Time.millisToPosix
    in
    Just Time.Format.format
        |> Maybe.Extra.andMap (Just Time.Format.Config.Config_ja_jp.config)
        |> Maybe.Extra.andMap (Just "%Y-%m-%d %H:%M:%S")
        |> Maybe.Extra.andMap (touchToZone data)
        |> Maybe.Extra.andMap (touchToPosix data)
        |> Maybe.withDefault "--/--/-- 00:00:00"


view : Model -> Html Msg
view model =
    let
        lastTouch =
            List.head model.history

        label =
            lastTouch
                |> Maybe.map .idm
                |> Maybe.withDefault ""

        deposit =
            lastTouch
                |> Maybe.andThen .data
                |> Maybe.map (String.slice 20 24)
                |> Maybe.andThen Hex.Convert.toBytes
                |> Maybe.andThen (Bytes.Decode.decode (Bytes.Decode.unsignedInt16 Bytes.LE))
                |> Maybe.withDefault 0
        

        -- 入退室カウント表示用
        entryTimes : List TouchData -> String
        entryTimes data = 
            groupEachIdm data
                |> transformToCounts
                |> Dict.get (getLastTouchData data).idm
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0
                |> String.fromInt

        exitTimes : List TouchData -> String
        exitTimes data =
            groupEachIdm data
                |> transformToCounts
                |> Dict.get (getLastTouchData data).idm
                |> Maybe.map Tuple.second
                |> Maybe.withDefault 0
                |> String.fromInt

        totalTouchCounts : List TouchData -> String
        totalTouchCounts data =
            groupEachIdm data
                |> Dict.get (getLastTouchData data).idm
                |> Maybe.withDefault 0
                |> String.fromInt

        entredNumbers : List TouchData -> String
        entredNumbers logs =
            groupEachIdm logs
                |> transformToCounts
                |> totalEntExiCount
                |> Tuple.first
                |> String.fromInt

        exitedNumbers : List TouchData -> String
        exitedNumbers data =
            groupEachIdm data
                |> transformToCounts
                |> totalEntExiCount
                |> Tuple.second
                |> String.fromInt

        viewTouchData : Maybe TouchData -> Html msg
        viewTouchData maybeData =
            let
                data = Maybe.withDefault defaultTouchData maybeData
            in
            li [] [ text <| "IDM : " ++ data.idm ++ " TIME : " ++ (formatTime maybeData) ]

        lastFiveEnteredPeople : List TouchData -> List String
        lastFiveEnteredPeople data =
            List.reverse data
                |> groupEachIdm
                |> Dict.toList
                |> List.take 4
                |> List.unzip
                |> Tuple.first

        viewLastFiveEnteredPeople : String -> Html msg
        viewLastFiveEnteredPeople data =
            li [] [ text <| "IDM : " ++ data ]
    in
    div [ id "body" ]
        [ div [] [ p [] [ text "Main" ] ]
        , div [] [ p [] [ text label ] ]
        , div [] [ p [] [ text <| formatTime lastTouch ] ]
        , div [] [ p [] [ text <| String.fromInt deposit ] ]
        , div [] [ p [] [ text <| "Enter : " ++ entryTimes model.history ++ " times "
                        , text <| "Exit : " ++ exitTimes model.history ++ " times "
                        , text <| "Total Counts : " ++ totalTouchCounts model.history ++ " times "
                        ]]
        , div [] [ p [] [ text <| "Entred People : " ++ entredNumbers model.history ++ " times "
                        , text <| "Exited People : " ++ exitedNumbers model.history ++ " times "]]
        , div [] [ ul [] ( List.map viewLastFiveEnteredPeople (lastFiveEnteredPeople model.history)) ]
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
        |> Procedure.try ProcMsg
            (Result.Extra.extract (OnError << ProOperateError))
