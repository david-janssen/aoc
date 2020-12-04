module Util
  ( loadDat
  )
where


import RIO

import qualified RIO.Text as T

loadDat :: Text -> IO Text
loadDat = readFileUtf8 . ("/home/david/aoc/dat/" <>) . T.unpack
