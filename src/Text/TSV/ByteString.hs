-- | Parser for tab separated value files.
--
-- Follows naming conventions of bytestring-csv package, but treats adjacent separators
-- as enclosing empty cells.
--
-- TSV files are a variant of comma separated value files, usually used to store tables
-- of data organized into columns and rows.
module Text.TSV.ByteString
    ( Row
    , parseTSV
) where

import qualified Data.ByteString.Char8 as S
import Data.ByteString.Char8 (ByteString)

-- | A single row consists of a bunch of cells.
type Row = [ByteString]

-- | Parses the contents of a TSV file.
--
-- Treats @\\t@ as column separators and @\\n@ or @\\r\\n@ as row separators.
-- Several separators in a row are treated as surrounding empty columns/rows.
--
-- No column/row count consistency enforced.
parseTSV :: ByteString -> [Row]
parseTSV = filter (/= [])
         . fmap (S.split '\t')
         . fmap removeTailR . S.split '\n'
    where
        removeTailR :: ByteString -> ByteString
        removeTailR s = if s /= S.empty && S.last s == '\r'
                            then S.take (S.length s -1) s
                            else s
