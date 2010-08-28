-----------------------------------------------------------------------------
--
-- Module      :  Text.Tables
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

module Text.Tables (
    Decoder, runDecoder,
    CsvField(..),
    skip
) where

import Data.Maybe (fromJust, catMaybes)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import MonadLib
import MonadLib.Monads
import Control.Applicative

type Row = [ByteString]
type Table = [Row]

-- | Monad for parsing lists of ByteString into data structures.
-- Parsers for individual cells are combined to build larger types, until a parser
-- for an entire table is assembled.
newtype Decoder a = MkDecoder {
    unDecoder :: State [ByteString] a
} deriving (Monad,Functor)

-- | The applicative interface allows to quickly define parsers for trivial tables.
instance Applicative Decoder where
    pure = return
    (<*>)= ap

-- | Parse a single row of a table. Since flat files are usually loaded on startup,
-- little effort is made to elegantly handle failure. If something's wrong with the
-- tables, the program won't run long anyway.
runDecoder :: Row -> Decoder a -> a
runDecoder bs = fst . runState bs
              . unDecoder

-- | Class of types that can be parsed by a decoder.
--
-- Class is used to give @dec@ode a polymorphic return type, which allows simple
-- tables to be parsed purely based on the order and types of the fields of the
-- equivalent data type.
class Field a where
    dec :: Decoder a


-- | The simplest type to parse. All others can be implemented from it.
instance Field ByteString where
    dec = MkDecoder $ do
        bs <- get
        let (a:rest) = bs -- error handle later
        set rest
        return a

-- | Default empty-value behavior is 0. Trailing non-numeric characters are ignored.
instance Field Int where
    dec = do n <- dec; return $ maybe 0 fst $ S.readInt n

-- | "1" is true, everything else false.
instance Field Bool where
    -- Maybe I shouldn't export this, and let people define their own definitions?
    dec = dec >>= \s->return $ if s==S.pack "1" then True else False

-- | Empty strings are Nothing.
instance Field (Maybe ByteString) where
    dec = do
        s <- dec
        if (s==S.empty) then return Nothing else return $ Just s

-- | Any non-numeric characters are Nothing. (Except a - at the start.)
instance Field (Maybe Int) where
    dec = dec >>= return . f where
        f :: ByteString -> Maybe Int
        f s = do
            (n,rest) <- S.readInt s -- if there's a string tail, int parse is
            if (rest==S.empty) then Just n else Nothing -- considered to fail

-- | Grouping adjacent related columns.
instance (Field a, Field b) => Field (a,b) where
    dec = (,) <$> dec <*> dec
instance (Field a, Field b, Field c) => Field (a,b,c) where
    dec = do a<-dec; b<-dec; c<-dec; return (a,b,c)

-- | Skipping columns of no interest.
skip :: Int -> Decoder ()
skip n = replicateM n (dec::Decoder ByteString) >> return ()
