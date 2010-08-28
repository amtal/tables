Make loading hideous CSV files with hundred of columns as easy as:

-- | gems.txt 
gems :: IO [Gem]
gems = load "gems.txt" -- tab-separated file with 41 columns, hundreds of rows
                       -- many of the columns form patterns, this is a flat
                       -- representation of structured data!

-- haskell type equivalent of gems.txt
-- contains equal amount of information, but doesn't need 41 columns to do it
data Gem = Gem
    { name :: ByteString
    , letter :: Maybe ByteString
    , transform :: Maybe Int
    , code :: ByteString
    , nummods :: Int
    , weaponMods :: [Mod]
    , helmMods :: [Mod]
    , shieldMods :: [Mod]
    } deriving (Show,Eq)

data Mod = Mod
    { modCode :: ByteString
    , modParam :: Int
    , modRange :: (Int,Int)
    } deriving (Show,Eq)

-- Most of the structure is defined by the type: it is filled with columns via
-- a boilerplate `instance Field Type where TypeConstr<$>dec<*>dec<*>dec<*>...` 
-- line.
instance Field Gem where
    dec = Gem<$>dec<*>dec<*>dec<*>dec<*>dec<*>mods<*>mods<*>mods where
        -- some information isn't present at the type: here we specify that
        -- `[Mod]` is parsed by parsing 3 `Maybe Mod` values
        mods = replicateM 3 dec >>= return . catMaybes

instance Field (Maybe Mod) where
    dec = do
        mod <- Mod <$> dec <*> dec <*> dec
        -- here, again, we add additional info not present in the type of Mod:
        return $ guard (modCode mod /= S.empty) >> Just mod


Inspired by that "databases are categories" presentation from Galois. (TODO: write more about that.)
