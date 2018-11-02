module Brain
  ( processRequest
  , BrainCells(..)
  ) where

import Brains.AI
import Brains.Speech
import Brains.Server
import Brains.Trello
import Brains.Home
import Conduit
import Data.Optional
import Data.Time
import Data.List

data BrainCells = BrainCells
  { ai     :: Brains.AI.APIOwner
  , trello :: Brains.Trello.APIOwner
  , server :: Brains.Server.ServerConfig
  }

zone :: TimeZone
zone = TimeZone {timeZoneMinutes = 180, timeZoneSummerOnly = True, timeZoneName = "Moscow"}

getTaskTime :: Optional TimeOfDay -> Optional Day -> IO (Optional UTCTime)
getTaskTime Default Default = return Default
getTaskTime time day = do
  ctime <- getCurrentTime
  let ltime = utcToLocalTime zone ctime
  let ttime = defaultTo (TimeOfDay 12 0 0) time
  let tday = defaultTo (localDay ltime) day
  let nwtime = LocalTime {localDay = tday, localTimeOfDay = ttime}
  return $ Specific $ localTimeToUTC zone nwtime

processCommand :: BrainCells -> AICommand -> String -> IO ConduitResponse
processCommand cells (AICommand (IntentTaskParams time date) response AIActionTask) meta = do
  utcTime <- getTaskTime time date
  result <- createTask (trello cells) meta utcTime
  case result of
    Left err   -> return $ ConduitResponseMessages ("Ошибка: " ++ err)
    Right resp -> return $ ConduitResponseMessages response
processCommand cells (AICommand IntentDoorQuery response AIActionDoorQuery) meta = do
  response <- homeIsDoorOpen
  case response of
    Left err -> return $ ConduitResponseMessages ("Ошибка: " ++ err)
    Right ContactOpen -> return $ ConduitResponseMessages "Сейчас дверь открыта"
    Right ContactClosed -> return $ ConduitResponseMessages "Сейчас дверь закрыта"
    Right ContactUnknown -> return $ ConduitResponseMessages "Точно не знаю"
processCommand cells (AICommand IntentDoorOpenTimeQuery response AIQueryDoorOpenTime) meta = do
  response <- homeDoorOpenTime
  case response of
    Left err -> return $ ConduitResponseMessages ("Ошибка: " ++ err)
    Right time -> return $ ConduitResponseMessages ("Дверь открывали: " ++ (show time))
processCommand cells (AICommand IntentWhoIsHome response _) meta = do
  response <- homeQueryPresenceMulti [Myself, Tanya]
  case response of
    Left err -> return $ ConduitResponseMessages ("Ошибка: " ++ err)
    Right states -> return $ ConduitResponseMessages $ intercalate "\n" $ map (\(dweller, state) ->
      let presence = if state then "дома" else "не дома" in
      (homeToRealName dweller) ++ " " ++ presence) states
processCommand cells (AICommand params response action) meta = return $ ConduitResponseMessages response
processCommand _ (AICommandError code error) _ = return $
  ConduitResponseMessages $ "Не смог понять: " ++ error ++ " (код: " ++ (show code) ++ ")"

processRequest :: BrainCells -> ConduitEvent -> IO ConduitResponse
processRequest cells (ConduitEventCommand text meta) = do
  command <- askAI (ai cells) text "test"
  case command of
    Left err  -> return $ ConduitResponseMessages err
    Right cmd -> processCommand cells cmd meta
processRequest cells (ConduitEventAudio url saying) = do
  textResult <- speechToText url
  case textResult of
    Nothing -> return $ ConduitResponseMessages "Не смог ничего понять"
    Just text -> do
      command <- askAI (ai cells) text "test"
      case command of
        Left err -> return $ prependLineResponse (saying ++ " " ++ text) (ConduitResponseMessages err)
        Right cmd -> fmap (prependLineResponse (saying ++ " " ++ text)) (processCommand cells cmd "")
processRequest owner _ = return $ ConduitResponseMessages "Чел, не знаю че как"
