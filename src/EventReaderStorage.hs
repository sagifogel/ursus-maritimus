{-# LANGUAGE InstanceSigs #-}

module EventReaderStorage ( create,
                            ReaderStorage(..),
                            EventReaderStorage,
                            getEventCountByType
                          , getEventCountByData ) where

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
  put (ReaderStorage ref) ev = modifyIORef ref (updateEventState ev)

updateEventState :: Event -> EventState -> EventState
updateEventState ev events = EventState typedEvents dataEvents
  where typedEvents = updateMap (eventsByType events) (eventType ev) :: TypedEvents
        dataEvents = updateMap (eventsByData events) (_data ev) :: DataEvents

updateMap :: Map.Map String Int -> String -> Map.Map String Int
updateMap map key = foldr (\_ map' -> Map.adjust (+1) key map') map lookup         
                    where lookup = Map.lookup key map 

pick :: ReaderStorage -> (EventState -> Map.Map String Int) -> IO (Map.Map String Int)
pick (ReaderStorage state) f = do 
  events <- readIORef state
  return $ f events