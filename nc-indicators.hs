-- Copyright 2014 Mihaly Barasz & Gergely Risko
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import qualified Data.Array.MArray as MA
import Data.Attoparsec.ByteString.Char8 (decimal, (.*>), skipSpace, skipWhile)
import qualified Data.Attoparsec.ByteString as AP
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Word (Word32)
import qualified Graphics.UI.Gtk as Gtk
import HFlags
import Pipes (Producer, yield, (>->), await, Consumer, Pipe, lift, runEffect)
import System.Environment
import System.IO
import System.Posix.Signals
import Text.Printf


defineFlag "show_cpu" True "Show the CPU load indicator/icon."
defineFlag "show_mem" True "Show the memory usage indicator/icon."
defineFlag "update_interval" (500 :: Int)
  "Update interval for all indicators in milliseconds."

--------------------------------------------------------------------------------
-- Acquiring raw CPU and RAM numbers

data CPUCounters =
  CPUCounters { cpuUser :: {-# UNPACK #-} !Int
              , cpuAllSys :: {-# UNPACK #-} !Int
              , cpuIdle :: {-# UNPACK #-} !Int
              } deriving (Show)

-- The counters are in milliseconds and cumulative since system start.
readCPUCounters :: IO CPUCounters
readCPUCounters = parseProcStat <$> readProcFile "/proc/stat"
  where
    parseProcStat = forceParse parseFirstLine
    parseFirstLine = wrapResult <$ "cpu " .*> skipSpace
                     <*> decimal <* skipSpace <*> decimal <* skipSpace
                     <*> decimal <* skipSpace <*> decimal <* skipSpace
                     <*> decimal <* skipSpace <*> decimal <* skipSpace
                     <*> decimal
    wrapResult user nice system idle iowait irq softirq =
      CPUCounters (user+nice) (system+iowait+irq+softirq) idle

data MEMCounters =
  MEMCounters { ramTotal :: {-# UNPACK #-} !Int
              , ramCached :: {-# UNPACK #-} !Int
              , ramFree :: {-# UNPACK #-} !Int
              } deriving (Show)

-- The counters are in kB.
readMEMCounters :: IO MEMCounters
readMEMCounters = parseProcMeminfo <$> readProcFile "/proc/meminfo"
  where
    parseProcMeminfo = forceParse parseHead
    parseHead :: AP.Parser MEMCounters
    parseHead =
      wrapResult <$ "MemTotal:" .*> skipSpace <*> decimal <* kb <*
        "MemFree:" .*> skipSpace <*> decimal <* kb <*>
        maybeMemAvailable <*
        "Buffers:" .*> skipSpace <* (skipWhile isDigit) <* kb <*
        "Cached:" .*> skipSpace <*> decimal <* kb
    kb = skipSpace *> "kB" .*> skipSpace
    maybeMemAvailable =
      Just <$ "MemAvailable:" .*> skipSpace <*> decimal <* kb <|> pure Nothing
    wrapResult total free mAvailable cached =
      MEMCounters total cached $ maybe free id mAvailable

forceParse :: AP.Parser a -> BS.ByteString -> a
forceParse parser = either error id . AP.parseOnly parser

-- If you stat /proc/stat or /proc/meminfo they look 0 sized, so we
-- cannot use BS.readFile.
-- We assume that all files of interest fit in 4k.
readProcFile :: FilePath -> IO ByteString
readProcFile file = withBinaryFile file ReadMode $ flip BS.hGetSome 4096

--------------------------------------------------------------------------------
-- Pipes

-- TODO(klao): use strict pairs
type Measurement = (Double, Double)

measureMEM :: Producer Measurement IO r
measureMEM = forever $ do
  MEMCounters total cached free <- lift readMEMCounters
  yield (fromIntegral (total-cached-free) / fromIntegral total,
         fromIntegral cached / fromIntegral total)

measureCPU :: Producer Measurement IO r
measureCPU = lift readCPUCounters >>= go
  where
    go (CPUCounters user0 sys0 idle0) = do
      m@(CPUCounters user sys idle) <- lift readCPUCounters
      -- TODO(klao): handle overflows!
      let total = fromIntegral $ user + sys + idle - user0 - sys0 - idle0
      when (total > 0) $ yield (fromIntegral (user - user0) / total,
                                fromIntegral (sys - sys0) / total)
      go m

delay :: Pipe a a IO r
delay = execU (threadDelay $ flags_update_interval * 1000)

prettyPrint :: String -> Consumer Measurement IO r
prettyPrint label = forever $ do
  (x1, x2) <- await
  lift $ printf "%s: %4.1f%% %4.1f%%\n" label (x1*100) (x2*100)

collectN :: Int -> Pipe Measurement (Seq Measurement) IO r
collectN n = go S.empty
  where
    go l = do
      m <- await
      let l' = S.take n $ m <| l
      yield l'
      go l'

--------------------------------------------------------------------------------
-- Random helpers

replaceTMVar :: TMVar a -> a -> STM ()
replaceTMVar var v = tryTakeTMVar var >> putTMVar var v


execU :: Monad m => m () -> Pipe a a m r
execU mOp = forever $ do
  lift mOp
  await >>= yield

--------------------------------------------------------------------------------
-- Status Icon handling

data MeasuredIcon
  = MI { miIcon :: Gtk.StatusIcon
       , miPixbuf :: TMVar Gtk.Pixbuf
       }

type Pixel = Word32


createIcon :: IO MeasuredIcon
createIcon = do
  icon <- Gtk.statusIconNew
  pixbufVar <- newEmptyTMVarIO
  _ <- Gtk.on icon Gtk.statusIconSizeChanged $ \size -> do
    pixbuf <- Gtk.pixbufNew Gtk.ColorspaceRgb True 8 size size
    atomically $ replaceTMVar pixbufVar pixbuf
    return True

  return $ MI icon pixbufVar

updateIcon :: MeasuredIcon -> Pixel -> Pixel -> Consumer (Seq Measurement) IO r
updateIcon (MI icon pixbufVar) color1 color2 = forever $ do
  measurements <- await
  pixbuf <- lift $ atomically $ readTMVar pixbufVar
  n <- lift $ Gtk.pixbufGetWidth pixbuf
  lift $ drawPixbuf pixbuf (S.take n measurements) color1 color2
  lift $ Gtk.postGUIAsync $ Gtk.statusIconSetFromPixbuf icon pixbuf

drawPixbuf :: (FoldableWithIndex Int t) => Gtk.Pixbuf -> t Measurement
              -> Pixel -> Pixel -> IO ()
drawPixbuf pb measurements color1 color2 = do
  -- Clear the pixbuf first
  Gtk.pixbufFill pb 0 0 0 0
  h <- Gtk.pixbufGetHeight pb
  w <- Gtk.pixbufGetWidth pb
  rs <- (`div` 4) <$> Gtk.pixbufGetRowstride pb
  pixels <- Gtk.pixbufGetPixels pb :: IO (Gtk.PixbufData Int Pixel)
  -- TODO(klao): do this much more efficiently
  let
    setP x y c = MA.writeArray pixels ((h-1-y)*rs + (w-1-x)) c
    drawC x y0 y1 c = forM_ [y0..y1-1] $ \y -> setP x y c
    toH m = max 0 $ min h $ round $ fromIntegral h * m
    drawM x (m1,m2) = do
      drawC x 0 y1 color1
      drawC x y1 y2 color2
        where
          y1 = toH m1
          y2 = toH (m1+m2)

  imapMOf_ ifolded drawM measurements


main :: IO ()
main = do
  nonGtkArgs <- Gtk.unsafeInitGUIForThreadedRTS
  [] <- withArgs nonGtkArgs $ $initHFlags "Show CPU and MEM graph with tray icons"

  when flags_show_cpu $ do
    -- Colors are in ABGR (RGBA little-endian)
    let blue1 = 0xffff4400
        blue2 = 0x88ff4400
    cpuIcon <- createIcon
    _ <- forkIO $ runEffect $ measureCPU >-> delay >-> collectN 100
                            >-> updateIcon cpuIcon blue1 blue2
    return ()

  when flags_show_mem $ do
    let green1 = 0xff44ff00
        green2 = 0x8844ff00
    memIcon <- createIcon
    _ <- forkIO $ runEffect $ measureMEM >-> delay >-> collectN 100
                            >-> updateIcon memIcon green1 green2
    return ()

  -- Properly handle Ctrl-C:
  _ <- installHandler sigINT (Catch $ Gtk.postGUIAsync Gtk.mainQuit) Nothing
  Gtk.mainGUI
