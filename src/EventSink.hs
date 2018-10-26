module EventSink where

import Pipes
import ProcessPipes
import EventWriterStorage
import EventReaderStorage
import Data.ByteString (ByteString)

writeStorageSink :: MonadIO m => WriterStorage -> Pipe ByteString String m ()
writeStorageSink = undefined

readStorageSink :: MonadIO m => ReaderStorage -> Pipe ByteString String m () 
readStorageSink = undefined