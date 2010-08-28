-----------------------------------------------------------------------------
--
-- Module      :  Text.TSV.ByteString
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Amtal <alex.kropivny@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Text.TSV.ByteString (
      Cell, Row, CSV
    , parseCSV
) where

import qualified Data.ByteString.Char8 as S
import Data.ByteString.Char8 (ByteString)

type Cell = ByteString
type Row = [Cell]
type CSV = [Row]

-- | This never actually fails, and provides zero guarantees about
-- column and row length consistency.
-- Will do for now.
parseCSV :: ByteString -> CSV
parseCSV = filter (/= [])
         . fmap (S.split '\t')
         . fmap removeTailR . S.split '\n'
    where
        removeTailR :: ByteString -> ByteString
        removeTailR s = if s /= S.empty && S.last s == '\r'
                            then S.take (S.length s -1) s
                            else s
