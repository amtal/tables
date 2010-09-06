{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
-- | Library for loading data from flat files.
-- Inspired by the 'Records are Categories' Galois Tech Talk:
-- <http://www.galois.com/blog/2010/05/27/tech-talk-categories-are-databases/>
--
-- Database tables and Haskell record types have uncanny similarities.
-- A powerful analogy can be drawn:
--
-- * table rows are like instances of a type
--
-- * entire tables are like the type itself
--
-- * columns are functions from a type to the column type
--
-- We use this analogy to easily generate text-to-types parsers.

module Text.Tables (
    -- * Type-polymorphic parser
    Field(..),
    -- * Utility functions
    skip,
    -- * Row parser monad
    Decoder, runDecoder

    -- * Example: \/etc\/passwd
    -- $passwdeg

    -- * Example: 41-column TSV file
    -- $gemseg
) where

import Data.Maybe (fromJust, catMaybes)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import MonadLib
import MonadLib.Monads
import Control.Applicative

-- | Monad for parsing lists of ByteString into data structures.
-- Parsers for individual cells are combined to build larger types, until a parser
-- for an entire table is assembled.
newtype Decoder a = MkDecoder {
    unDecoder :: State [ByteString] a
} deriving (Monad,Functor)
instance Applicative Decoder where pure = return; (<*>) = ap

-- | Parse a single row of a table. Since flat files are usually loaded on startup,
-- little effort is made to elegantly handle failure. If something's wrong with the
-- tables, the program won't run long anyway.
runDecoder :: Decoder a -> [ByteString] -> a
runDecoder d bs = fst . runState bs . unDecoder $ d

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

-- | \"1\" is true, everything else false.
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



-- $passwdeg
-- The \/etc\/passwd file has a clear table structure, and an equivalent type:
--
-- > data Passwd = Passwd
-- >    { pwdAccount :: ByteString
-- >    , pwdHash :: Hash
-- >    , pwdUid, pwdGid :: Int
-- >    , pwdDescription :: ByteString
-- >    , pwdHome :: ByteString
-- >    , pwdShell :: ByteString
-- >    } deriving (Show,Eq)
-- >
-- > instance Field Passwd where
-- >    dec = Passwd<$>dec<*>dec<*>dec<*>dec<*>dec<*>dec<*>dec
--
-- We also defined a parser that @dec@odes rows in \/etc\/passwd, purely based on
-- the types and order of elements in Passwd. It was easy because of the close
-- relation between table and type:
--
-- * All possible table entries are represented by the Passwd type.
--
-- * Specific entries are represented by type instances.
--
-- * Columns of the table are represented by the pwdFoo functions.
--
-- Note that we defined 'dec' in terms of 'dec'. We can compose column parsers from
-- smaller column parsers. (There's a 'Field' instance for tuples that does this.)
-- We can also extract additional information through custom 'Field' instances.
--
-- For example, the @pwdHash@ column is just a 'ByteString' but by convention the
-- values ''*'' and ''x'' have special meaning. Why not make that clear?
--
-- > data Hash = ShadowFile | InactiveAcct | Hash ByteString deriving (Show,Eq)
-- >
-- > instance Field Hash where
-- >    dec = dec >>= return . lup where
-- >        lup "x" = ShadowFile -- (needs -XOverloadedStrings)
-- >        lup "*" = InactiveAcct
-- >        lup other = Hash other
--
-- You could take this further - @pwdHome@ and @pwdShell@ aren't 'ByteString's,
-- they're paths. What path-specific sanitization might you want to do in such a
-- situation? When dealing with massive, 100-column tables, this kind of type
-- composition is a lifesaver. Even with a simple passwd file, it quickly produces
-- nice results:
--
-- >>> let splitPasswd = map (map pack . split ':') . split '\n'
-- >>> table <- readFile "/etc/passwd" >>= return . splitPasswd
-- >>> let passwds = map (runDecoder dec) table
-- >>> passwds !! 0
-- Passwd { pwdAccount = "root", pwdHash = ShadowFile, pwdUid = 0, pwdGid = 0,
-- pwdDescription = "root", pwdHome = "/root", pwdShell = "/bin/bash"}
--

-- $gemseg
--
-- Take a tab-separated file with 41 columns, used to store game data. Many of the
-- columns form patterns, the file is a flat representation of structured data!
--
-- Here's a type equivalent that describes all 41 columns with 11 properties:
--
-- > data Gem = Gem
-- >     { name :: ByteString
-- >     , letter :: Maybe ByteString
-- >     , transform :: Maybe Int
-- >     , code :: ByteString
-- >     , nummods :: Int
-- >     , weaponMods :: [Mod]
-- >     , helmMods :: [Mod]
-- >     , shieldMods :: [Mod]
-- >     } deriving (Show,Eq)
-- >
-- > data Mod = Mod
-- >     { modCode :: ByteString
-- >     , modParam :: Int
-- >     , modRange :: (Int,Int)
-- >     } deriving (Show,Eq)
--
-- Most of the structure is defined by the type: the parser is defined via
-- a boilerplate
-- @instance Field Type where dec = TypeConstr\<$\>dec\<*\>dec\<*\>dec\<*\>...@
-- line.
--
-- Some information can't fit into the type, however. The lists, for instance,
-- represent repeating columns - but how many? Here, using 'dec' to parse isn't
-- enough. And while it's clear what @Maybe Foo@ means, which specific value of
-- @Foo@ counts as @Nothing@?
--
-- > instance Field Gem where
-- >     dec = Gem<$>dec<*>dec<*>dec<*>dec<*>dec<*>mods<*>mods<*>mods where
-- >         -- `[Mod]` is parsed by parsing 3 `Maybe Mod` values and reducing
-- >         mods = replicateM 3 dec >>= return . catMaybes
-- >
-- > instance Field (Maybe Mod) where
-- >     dec = do
-- >         mod <- Mod <$> dec <*> dec <*> dec
-- >         -- here, again, we add additional info not present in the type of Mod:
-- >         return $ guard (modCode mod /= S.empty) >> Just mod

