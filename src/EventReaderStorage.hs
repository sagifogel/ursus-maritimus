{-# LANGUAGE InstanceSigs #-}

module EventReaderStorage ( put
                          , create
                          , ReaderStorage
                          , getEventCountByType
                          , getEventCountByData
                          ) where

import Events
import Data.IORef
import qualified Data.Map as Map

type TypedEvents = Map.Map String Int
type DataEvents = Map.Map String Int
data EventState = EventState { eventsByType :: TypedEvents
                             , eventsByData :: DataEvents }

newtype ReaderStorage = ReaderStorage { getStorage :: IORef EventState }

create :: IO ReaderStorage
create =  do
  ref <- newIORef (EventState Map.empty Map.empty)
  return $ ReaderStorage ref

class EventReaderStorage s where
  put :: s -> Event -> IO ()
  getEventCountByType :: s -> IO (Map.Map String Int)
  getEventCountByData :: s -> IO (Map.Map String Int)

instance EventReaderStorage ReaderStorage where
  getEventCountByData ref = pick ref (eventsByData)
  getEventCountByType ref = pick ref (eventsByType)   
  put (ReaderStorage ref) event = modifyIORef' ref (updateEventState event)

updateEventState :: Event -> EventState -> EventState
updateEventState ev events = EventState typedEvents dataEvents
  where typedEvents = updateMap (eventsByType events) (eventType ev) :: TypedEvents
        dataEvents = updateMap (eventsByData events) (_data ev) :: DataEvents

updateMap :: Map.Map String Int -> String -> Map.Map String Int
updateMap map key = Map.insertWith (+) key 1 map  

pick :: ReaderStorage -> (EventState -> Map.Map String Int) -> IO (Map.Map String Int)
pick (ReaderStorage ref) f = f <$> readIORef ref