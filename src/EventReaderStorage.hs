{-# LANGUAGE InstanceSigs #-}

module EventReaderStorage ( put
                          , create
                          , ReaderStorage
                          , getEventCountByType
                          , getEventCountByData
                          ) where

import Events
import Data.Map
import Data.IORef

type TypedEvents = Map String Int
type DataEvents = Map String Int
data EventState = EventState { eventsByType :: TypedEvents
                             , eventsByData :: DataEvents }

newtype ReaderStorage = ReaderStorage { getStorage :: IORef EventState }

create :: IO ReaderStorage
create =  do
  ref <- newIORef (EventState empty empty)
  return $ ReaderStorage ref

class EventsStorage s => EventReaderStorage s where
  getEventCountByType :: s -> IO (Map String Int)
  getEventCountByData :: s -> IO (Map String Int)

instance EventsStorage ReaderStorage where
  put (ReaderStorage ref) event = modifyIORef' ref (updateEventState event)

instance EventReaderStorage ReaderStorage where
  getEventCountByData ref = pick ref eventsByData
  getEventCountByType ref = pick ref eventsByType   

updateEventState :: Event -> EventState -> EventState
updateEventState ev events = EventState typedEvents dataEvents
  where typedEvents = updateMap (eventsByType events) (eventType ev) :: TypedEvents
        dataEvents = updateMap (eventsByData events) (_data ev) :: DataEvents

updateMap :: Map String Int -> String -> Map String Int
updateMap map key = insertWith (+) key 1 map  

pick :: ReaderStorage -> (EventState -> Map String Int) -> IO (Map String Int)
pick (ReaderStorage ref) f = f <$> readIORef ref