{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specify newline character sequences and algorithms generic over them.
--
-- Note that this module only understands simple character strings as newlines,
-- whereas some encodings have complex algorithmically-defined newlines.
-- Of course, most users will be interested in only Unix- or Windows-style newline.
--
-- Information for the newline definitions comes from https://en.wikipedia.org/wiki/Newline
module Text.Newline
  ( -- * Newline Type
    Newline(..)
  -- ** Common Newlines
  , pattern Unix
  , pattern Windows
  , pattern ClassicMac
  , pattern PrePosixQnx
  , pattern RiscOsSpool
  , pattern IbmMainframe
  -- * Text Operations
  -- ** Split Lines
  , breakLine
  -- ** Join Lines
  , linesUnix
  , linesBy
  , unlinesBy
  -- ** Conversion
  , pattern NlText
  , toText
  , fromText
  , toString
  ) where

import Data.Foldable (toList)
import Data.Maybe (catMaybes,fromJust)
import Data.String (IsString(..))
import Data.Text (Text)

import qualified Data.Text as T


-- | Specification for a newline character sequence
data Newline = OtherNl {-# UNPACK #-} !Char {-# UNPACK #-} !Text
  deriving (Eq)

instance Show Newline where
  show Unix = "Unix"
  show Windows = "Windows"
  show ClassicMac = "ClassicMac"
  show PrePosixQnx = "PrePosixQnx"
  show RiscOsSpool = "RiscOsSpool"
  show IbmMainframe = "IbmMainframe"
  show nl = toString nl

-- TODO a Read instance

------------ Text Operations ------------

-- | Equivalent to 'linesBy [Unix]'.
linesUnix :: Text -> [Text]
linesUnix "" = []
linesUnix str =
  let (pre, atpost) = T.break (=='\n') str
   in case T.uncons atpost of
        Nothing -> [pre]
        Just ('\n', post) -> pre : linesUnix post
        Just (c, _) -> errorWithoutStackTrace $
          "internal error in Text.Newline.linesUnix (expecting '\\n', but found" ++ show c ++ ")"

-- | Split text into lines where any of the given 'Newline' values are considered as a newline.
-- Returns not just the text of each line, but also the matched newline itself.
-- Returns 'Nothing' for the 'Newline' for a line that was ended by the end of input.
linesBy :: [Newline] -> Text -> [(Text, Maybe Newline)]
linesBy valid = go
  where
  go str = case breakLine valid str of
    (str', Nothing) -> [(str', Nothing)]
    (str', Just (nl, rest)) -> (str', Just nl) : go rest

-- | Split one line from the input.
-- Also returns the newline that was matched and any following text.
-- If no newline was matched, then all the input is placed in the first return value.
breakLine :: [Newline] -> Text -> (Text, Maybe (Newline, Text))
breakLine valid str =
  let (pre, atpost) = T.break (`elem` nlStarts) str
   in case T.uncons atpost of
        Nothing -> (str, Nothing)
        Just (at, post) -> case takeSomeNl valid atpost of
          Nothing ->
            let preAt = pre <> T.singleton at
                (pre', post') = breakLine valid post
             in (preAt <> pre', post')
          Just (nl, post') -> (pre, Just (nl, post'))
  where
  nlStarts = (\(OtherNl c _) -> c) <$> valid

takeSomeNl :: [Newline] -> Text -> Maybe (Newline, Text)
takeSomeNl valid str = case catMaybes $ flip takeNl str <$> valid of
  [] -> Nothing
  (it:_) -> Just it

takeNl :: Newline -> Text -> Maybe (Newline, Text)
takeNl nl@(NlText t) str = if t `T.isPrefixOf` str
  then Just (nl, T.drop (T.length t) str)
  else Nothing

-- | Join lines by inserting newlines between them.
--
-- Mirrors 'Prelude.unlines', but allows different line endings.
unlinesBy :: (IsString str, Monoid str, Foldable f) => Newline -> f str -> str
unlinesBy nl = go . toList
  where
  go [] = mempty
  go ts = foldr1 (\x xs -> x <> fromString (toString nl) <> xs) ts

{-# COMPLETE NlText #-}
-- | Construct/deconstruct 'Newline' as a whole 'Text'.
-- The constructor is partial: it is undefined constructed with an empty string.
pattern NlText :: Text -> Newline
pattern NlText t <- (toText -> t)
  where
  NlText t = unsafeFromText t

-- | Convert a newline specification to the text it matches.
toText :: Newline -> Text
toText (OtherNl c t) = T.cons c t

-- | Convert text into a newline specification that matches it.
fromText :: Text -> Maybe Newline
fromText t = uncurry OtherNl <$> T.uncons t

-- | As 'fromText', but is partial in the case of empty input.
unsafeFromText :: Text -> Newline
unsafeFromText = fromJust . fromText

-- | As 'toText', but targeting the 'String' type.
toString :: Newline -> String
toString (OtherNl c str) = c : T.unpack str

------------ Shortcuts ------------

-- | For Unix and Unix-like systems.
-- It's by far the most common (and easy-to-recognize) newline,
--   so when in doubt, don't generate anything else.
pattern Unix :: Newline
pattern Unix = OtherNl '\n' ""

-- | For DOS and DOS-like systems, including Microsoft Windows.
-- Still common, and in being so, is a pain.
pattern Windows :: Newline
pattern Windows = OtherNl '\r' "\n"


-- | For a variety of older machines, such as
--   Commodore 8-bit machines, ZX Spectrum, TRS-80,
--   Apple II series, the classic Mac OS,
--   and the MIT Lisp Machine.
pattern ClassicMac :: Newline
pattern ClassicMac = OtherNl '\r' ""

-- | For QNX version <4
pattern PrePosixQnx :: Newline
pattern PrePosixQnx = OtherNl '\RS' ""

-- | For RISC OS spoolet text output.
--
-- Reportedly also used for Acorn BBC, but that machine is also listed as using `"\r"`.
-- The manual (http://stardot.org.uk/mirrors/www.bbcdocs.com/filebase/essentials/BBC%20Microcomputer%20Advanced%20User%20Guide.pdf)
-- does back up this assertion, though.
pattern RiscOsSpool :: Newline
pattern RiscOsSpool = OtherNl '\n' "\r"

-- | EBCDIC systems
-- — mainly IBM mainframe systems, including z/OS (OS/390) and IBM i (OS/400) —
-- use NL (New Line, 0x15)[8] as the character combining the functions of line feed and carriage return.
-- The equivalent Unicode character (0x85) is called NEL (Next Line).
--
-- Citation: IBM System/360 Reference Data Card, Publication GX20-1703, IBM Data Processing Division, White Plains, NY
pattern IbmMainframe :: Newline
pattern IbmMainframe = OtherNl '\x85' ""
