{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Create a map of the lines in the file to allow fast seeking later.
-- Specifically, for each line, we output:
--
-- - the byte offset from the start of the file of the start of the line
-- - the length of the line in number of bytes (including the line terminator, if any)
-- - the type of line terminator that ended the line, if any
-- - the non-decoded bytes of that line.
--
-- There is an associated file format to serialize this data, based on CSV.
-- See documentation for 'display'.
--
-- Currently, we only support utf8-encoded text with Unix line-endings (LF).
module Text.Newline.LineMap
  ( Line(..)
  , display
  , breakLines_unixUtf8
  , breakLine_unixUtf8
  ) where

import Prelude hiding (length)

import Text.Newline (Newline,pattern Unix)

import qualified Data.ByteString.Lazy as LBS

-- | Holds a detected line.
-- The main result type for this module.
data Line a = Line
  { startOffset :: {-# UNPACK #-} !Int -- ^ offset in bytes of the start of the line from the start of the input file
  , content :: a -- ^ generally, does not include newline
  , nlType :: Maybe Newline -- ^ the terminator for this line, if any
  , length :: {-# UNPACK #-} !Int -- ^ length of the line in bytes, including the line terminator
  }
  deriving (Functor)

-- | Render contents for a linemap file.
--
-- The format is simply a three-colum CSV with header row.
-- The columns are offset, length, and terminator, as above.
-- Offset and length are decimal-encoded unsigned integers.
-- The terminator column must hold one of the following strings:
--
-- - @unix@ for LF (ASCII 0x0A),
-- - @dos@ for CRLF (ASCOO 0x0D 0x0A),
-- - @eof@ for end of file/input.
--
-- The output CSV does not require quoting,
--   so the output actually abides by RFC 4180
--   (with the exception that I'm using LF instead of CRLF, sigh).
display :: [Line a] -> String
display ls0 = "offset,length,terminator\n" ++ go ls0
  where
  go [] = ""
  go (l:ls) = go1 l ++ display ls
  go1 l = concat
    [ show l.startOffset
    , ","
    , show l.length
    , ","
    , case l.nlType of
      Nothing -> "eof"
      Just Unix -> "unix"
      _ -> "<UNKNOWN>"
    , "\n"
    ]

-- | Split input into lines.
-- Assumes utf8-encoded text with LF (ASCII 0x0A) line terminators.
-- See 'breakLine_unixUtf8' to take a single line.
--
-- Does not include newlines in any 'Line' 'content'.
breakLines_unixUtf8 ::
     LBS.ByteString -- ^ all bytes of a file
  -> [Line LBS.ByteString]
breakLines_unixUtf8 = go 0
  where
  go _ bs | LBS.null bs = []
  go off bs =
    let (l, bs') = breakLine_unixUtf8 off bs
        off' = off + l.length
     in l : go off' bs'

-- | Take one line of input, and also return the remaining input.
-- Assumes utf8-encoded text with LF (ASCII 0x0A) line terminators.
-- See 'breakLines_unixUtf8' to produce a list of all lines.
--
-- Does not include newlines in any 'Line' 'content'.
breakLine_unixUtf8 ::
     Int -- ^ byte offset within file of input
  -> LBS.ByteString -- ^ non-empty input bytes
  -> (Line LBS.ByteString, LBS.ByteString) -- ^ resuling line and remaining input
breakLine_unixUtf8 off bs =
  let (pre, atpost) = LBS.break (==0x0A) bs
   in case LBS.uncons atpost of
    Nothing -> (l, atpost)
      where
      l = Line
        { startOffset = off
        , content = pre
        , nlType = Nothing
        , length = fromIntegral $ LBS.length pre
        }
    Just (0x0A, post) -> (l, post)
      where
      l = Line
        { startOffset = off
        , content = pre
        , nlType = Just Unix
        , length = fromIntegral $ LBS.length pre + 1
        }
    Just (c, _) -> errorWithoutStackTrace $
      "internal error: newline delimited by byte " <> show c
