{-# LANGUAGE RecordWildCards   #-}
{-|
Module      : NLP.Morphosyntax.Analyzer
Description : Language-agnostic analyzer for positional morphosyntactic tags
Copyright   : (c) Vjeran Crnjak, 2014
License     : BSD3
Maintainer  : vjeran.crnjak@gmail.com
Stability   : experimental
Portability : portable


Implementation of a space-efficient morphosyntactic analyzer.

It solves a problem of providing a set of possible tags for a given word.
Instead of just matching on the word-set pair, one can assume that suffixes of
an unknown word also hold some information about the set.

This library provides the functionality of that kind of analysis. One example of
where this might be useful is 'concraft' tagging library. Before the POS-tagging
one needs to have a set of possible tags for a word from which the correct one is
disambiguated.

For a sufficiently large construction corpus this analyzer might only benefit from
additional regular expressions for punctuation and number matching. There is a
possibility of returning a set of possible tags that isn't complete - the set doesn't
contain a correct tag. If construction corpus isn't sufficiently large, there might
be a fair amount of incomplete sets on unseen named entities (person names, corporation
names etc.).

If one needs the analyzer to be less aggressive, it is recommended to extend the
functionality and remove the sets of possible tags from words which might be named (ex.
capitalized words in the middle of a sentence). This is present mostly in use cases
where part-of-speech tags of a language contain information whether a word represents a
named entity or not, so if this is not a case, there will be no need to extend the
current functionality.

A simple example of using @GHCi@ for construction:

> :set -XOverloadedStrings
> import qualified Data.Text.IO as T
> import qualified Data.Tagset.Positional as P
> f <- readFile "tagset.cfg"
> let tset = P.parseTagset "tagset1" f
> f <- T.readFile "fulldict.txt"
> let train = map (\(word:tags) -> (word, map (P.parseTag tset) tags)) . map T.words . filter (not . T.null) . T.lines $ f
> let an = create tset (AConf 3 [] M.empty) train
> save "analyzer.gz" an

It is assumed that tag attributes are separated with @:@ for 'P.parseTag'. One could write a
different parsing function.
-}
module NLP.Morphosyntax.Analyzer
(
-- * Model
  Analyzer
, elem
, getTags
, save
, load
, create
, emptyConf
-- * Token matching
, Matcher(..)
-- * Configuration
, AConf(..)
) where

import qualified Codec.Compression.GZip as GZip
import           Control.Applicative    ((<$>), (<*>))
import           Control.Arrow          (first, second, (&&&), (***))
import           Control.Monad          (when)
import           Data.Binary            (Binary, get, put, putWord8, getWord8)
import qualified Data.Binary            as Binary
import qualified Data.ByteString.Lazy   as BL
import qualified Data.DAWG.Static       as D
import qualified Data.IntMap            as IntMap
import qualified Data.IntSet            as IntSet
import qualified Data.Map               as M
import           Data.Maybe             (mapMaybe)
import qualified Data.Set               as S
import qualified Data.Tagset.Positional as P
import qualified Data.Text              as T
import           Data.Tuple             (swap)

import           Data.Char

import           Text.Regex.TDFA        ((=~))
import           Text.Regex.TDFA.Text   ()

import           Prelude                hiding (elem, notElem)


---------------------
-- Morphological analyzer
---------------------


modelVersion :: String
modelVersion = "0.2.0.2"

-- | Replaces the need of writing regular expressions for simple matching.
-- Matching on punctuation, number, alphanumeric, upper-case tokens or
-- regular expressions.
data Matcher = Punct  -- ^ Matches a token with all punctuation characters.
             | Number -- ^ Matches a token with all unicode numeral characters.
             | AlphaNum -- ^ Matches a token with all alphanumeric characters.
             | AnyUpper -- ^ Matches a token with at least one uppercase characther.
             | AllUpper -- ^ Matches a token with all uppercase characters.
             | AnyLower -- ^ Matches a token with at least one lowercase characther.
             | AllLower -- ^ Matches a token with all lowercase characters.
             | Capital  -- ^ Matches a capitalized token.
             | RegExpr T.Text -- ^ Matches on a regular expression.
     deriving (Eq, Show, Ord)

instance Binary Matcher where
    put Punct    = putWord8 0
    put Number   = putWord8 1
    put AlphaNum = putWord8 2
    put AnyUpper = putWord8 3
    put AllUpper = putWord8 4
    put AnyLower = putWord8 5
    put AllLower = putWord8 6
    put Capital  = putWord8 7
    put (RegExpr x) = putWord8 8 >> put x
    get =
        getWord8 >>=
          \x -> case x of
            0   -> return Punct
            1   -> return Number
            2   -> return AlphaNum
            3   -> return AnyUpper
            4   -> return AllUpper
            5   -> return AnyLower
            6   -> return AllLower
            7   -> return Capital
            8   -> RegExpr <$> Binary.get
            _   -> error "Unable to parse the matchers - resulting in non-parsable analyzer."

-- | Configuration for the analyzer.
data AConf = AConf
  {
    -- | If word isn't known this is the smallest suffix length that will be matched.
    suffixLen        :: Int
    -- | A list of regular expressions (POSIX) and accompanying set of tags.
    -- If a word matches a regular expression, the accompanying set of tags
    -- will be given as the set of possible tags.
  , regexMatch       :: [(Matcher, S.Set P.Tag)]
    -- | Provides the analyzer with the ability to analyze the word on a single 'P.POS'-tag in
    -- case incomplete construction corpus is present. (Ex. Croatian adjectives and pronouns)
    -- It might be the case that words that can be adjectives can
    -- also be pronouns. If the analyzer isn't thorough enough (the provided
    -- construction data doesn't have all cases covered) one would also like that
    -- words that are adjectives are also interpreted as being pronouns. What can happen
    -- is, an unknown word has a very long suffix that matches an adjective, but it can
    -- also be a pronoun. In that case one would like pronoun tags too.
    -- If your construction data is very large this doesn't have to be used.
  , separationLayout :: M.Map P.POS (S.Set P.POS)
  } deriving (Eq, Show)

instance Binary AConf where
  put AConf{..} = do
    put suffixLen
    put regexMatch
    put separationLayout
  get = AConf <$> get <*> get <*> get

-- | Can be used for dummy analyzer building.
emptyConf :: AConf
emptyConf = AConf 0 [] M.empty

{-|
  This is a layout of conflicts that POS tags might have.
  If there are conflicts the specialized DAWGs are used to resolve them.
  For example. It might be the case that words that can be adjectives can
  also be pronouns. If the analyzer isn't thorough enough (the provided
  construction data doesn't have all cases covered) one would also like that
  words that are adjectives are also interpreted as being pronouns. If a word
  has only data about being an adjective but one wants it to be treated as a
  pronouns too (in some contexts) this is a useful thing to set up. What can happen
  is that a word has a very long suffix which matches an adjective but it can
  also be a pronoun. In that case one would like pronoun tags too.
-}
data ConstLayout = CSL {
    -- | Contains for each POS a set of POS for which the specialized DAWGs can be fetched.
    sdawgs    :: M.Map P.POS (S.Set P.POS)
    -- | Map containing all the specialized DAWGs.
    -- * Specialized DAWG contains only words linked with a POS which is the key in the map.
  , posToDawg :: M.Map P.POS (D.DAWG Char Int IntSet.IntSet)
  } deriving (Eq, Show)

instance Binary ConstLayout where
  put CSL{..} = do
    put sdawgs
    put posToDawg
  get = CSL <$> get <*> get

-- | Representation of the analyzer.
data Analyzer = Analyzer -- TODO Change to Analyzer t, to remove the P.Tag dependency
  {
    -- | This field represents the possible tagset of words used in this analyzer.
    tagset   :: P.Tagset
    -- | Analyzer configuration, check 'AConf'.
  , conf     :: AConf
    -- | Maps 'Int' to 'P.Tag'.
    -- Used primarily for space compression.
  , numToTag :: IntMap.IntMap P.Tag
    -- | Compressed map of words to their possible set of tags.
  , dawg     :: D.DAWG Char Int IntSet.IntSet
    -- | Check 'ConstLayout' for detailed info.
  , csl      :: ConstLayout
  } deriving Eq


instance Binary Analyzer where
    put Analyzer{..} = do
        put modelVersion
        put tagset
        put conf
        put numToTag
        put dawg
        put csl
    get = do
        comp <- get
        let (x1:y1:_) = words $ map (\x -> if x == '.' then ' ' else x) comp
        let (x2:y2:_) = words $ map (\x -> if x == '.' then ' ' else x) modelVersion
        when (x1 /= x2 || y1 /= y2) $ error $
            "Incompatible analyzer code version: " ++ comp ++
            ", expected: " ++ modelVersion
        -- ^^ Models will be compatible if they match in the first two version numbers
        Analyzer <$> get <*> get <*> get <*> get <*> get

-- | Gives back a set of 'P.Tag' given the indices.
toTags :: Analyzer -> IntSet.IntSet -> S.Set P.Tag
toTags Analyzer{..} =
  S.fromList . map (numToTag IntMap.!) . IntSet.toList

-- | Gives a set of possible tags for a given word. It is possible that
-- the set of possible tags is empty.
getTags :: Analyzer -> T.Text -> S.Set P.Tag
getTags a@Analyzer{..} w
  | not $ S.null rm = rm
  | otherwise       = ts
  where rw = T.reverse w
        ts = (if M.null (sdawgs csl)   -- if no separation layout is given
              then id                  -- just get the tags
              else expandTags a rw ) $ -- otherwise expand them if necessary
             toTags a $ getPureTags a rw
        rm = matchOn conf w

-- | Matches all the provided matchers and takes the accompanying set
-- of the first one that matches.
matchOn :: AConf -> T.Text -> S.Set P.Tag
matchOn AConf{..} w =
  let testR = map (first $ flip matchOnMatcher w) regexMatch
  in case dropWhile (not . fst) testR of
       []        -> S.empty
       ((_,x):_) -> x

matchOnMatcher :: Matcher -> T.Text -> Bool
matchOnMatcher Punct          = T.all isPunctuation
matchOnMatcher Number         = T.all isNumber
matchOnMatcher AlphaNum       = T.all isAlphaNum
matchOnMatcher AnyUpper       = T.any isUpper
matchOnMatcher AllUpper       = T.all isUpper
matchOnMatcher AnyLower       = T.any isLower
matchOnMatcher AllLower       = T.all isLower
matchOnMatcher Capital        = isUpper . T.head
matchOnMatcher (RegExpr regex)= (=~ regex)

-- | Returns a set of tags from the 'D.DAWG' without any conditions or adjustments.
-- @w@ should be in reversed form for 'D.DAWG'.
getPureTags :: Analyzer -> T.Text -> IntSet.IntSet
getPureTags a@Analyzer{..} w =
  case (lookUp wl, lookUp wu) of
    (Nothing, Nothing) -> suffixSet a w -- trying to fetch sets of word suffixes
    (Just ss, Nothing) -> ss
    (Nothing, Just ss) -> ss
    (Just ss, Just sp) -> IntSet.union ss sp
  where
    lookUp = flip D.lookup dawg . T.unpack
    wu = w
    wl = T.toLower w

-- | Adds the possible tags from specialized dawgs containing MSDs with
-- one kind of POS attribute.
expandTags :: Analyzer -> T.Text -> S.Set P.Tag -> S.Set P.Tag
expandTags a@Analyzer{..} w st = S.union st . toTags a $ getNew csl
  where allpos = S.map P.pos st -- get all POS being present in set of possible tags
        getNew CSL{..} =
          let npos = S.unions . mapMaybe (`M.lookup` sdawgs) $ S.toList allpos -- get all the conflicting pos
              uqpos= S.toList $ S.difference npos allpos -- get only pos tags which weren't fetched (aren't present in the current set of possible tags)
          in IntSet.unions $ map (\pos -> getPureTags (a {dawg = posToDawg M.! pos}) w) uqpos

suffixSet :: Analyzer -> T.Text -> IntSet.IntSet
suffixSet Analyzer{..} w = numTags
    where suffixes = reverse . drop (suffixLen conf) $ T.inits w -- only interested in proper suffixes (suffixLen)
          -- | @dawgs@ starts with the largest matched suffix
          dawgs    = dropWhile ((==0) . D.size) $ map ((`D.submap` dawg) . T.unpack) suffixes -- fusion?
          numTags
            | null dawgs = IntSet.empty
            | otherwise  = IntSet.unions . D.elems $ head dawgs

-- | Save analyzer in a file. Data is compressed using the gzip format.
save :: FilePath -> Analyzer -> IO ()
save path = BL.writeFile path . GZip.compress . Binary.encode

-- | Load analyzer from a file.
load :: FilePath -> IO Analyzer
load path = do
    x <- Binary.decode . GZip.decompress <$> BL.readFile path
    x `seq` return x

-- | Creates a morphological analyzer given a tagset, a list of regex for additional matching,
-- smallest suffix length and a construction corpus.
create :: P.Tagset                -- ^ Tagset used in the construction corpus.
       -> AConf                   -- ^ Configuration of the analyzer.
       -> [(T.Text, [P.Tag])]     -- ^ Construction corpus.
       -> Analyzer                -- ^ Morphological analyzer.
create tset cnf@AConf{..} xs =
  Analyzer { tagset     = tset
           , conf       = cnf
           , numToTag   = ntt
           , dawg       = dwg
           , csl        = cnls
           }
  where
    -- | Reverses the word and converts the tag to its unique number.
    revNttn   = (T.unpack . T.reverse) *** (IntSet.fromList . map (ttn M.!)) -- fusion?
    -- | Constructs the 'D.DAWG' given the
    construct = D.weigh . D.fromListWith IntSet.union
    dwg       = construct $ map revNttn xs -- reverse the word (suffixes) and convert tag to num constructing the dawg
    allTags   = S.unions $ map (S.fromList . snd) xs -- set of all tags
    ntt       = IntMap.fromAscList . zip [1..] $ S.toAscList allTags -- number to tag
    ttn       = M.fromList . map swap $ IntMap.toAscList ntt -- tag to number
    cnls      =
      if M.null separationLayout
        then CSL separationLayout M.empty
        else
      -- TODO Do everything explicitly with one pass over xs
      -- this procedure was changed and is this dirty because
      -- [(T.Text, P.Tag)] was consuming too much memory.
      let allpos    = S.toList . S.unions $ M.elems separationLayout
          forp pos  = construct . map revNttn   $ -- constructs a dawg containing only given pos
                      filter (not . null . snd) $ -- filters all the words which don't have a pos
                      map (second $ filter ((==pos) . P.pos)) xs -- filters the pos out of the words
          alldwgs   = M.fromList $ map (id &&& forp) allpos
      in  CSL separationLayout alldwgs

-- TODO
-- The optimal data structure for fetching is suffix dawg but construction can be long
-- Optimal structure for fetching and constructing is a generalized suffix tree.
-- Could change the construction using a suffix to set mapping, this could potentially
-- speed up the retrieval of tags (step of set unions is avoided (suffixSet function) -- submap procedure).
--    1. Data.List.tails is called on each word and tags associated with
--       the word are paired with it.
--    2. For each suffix we check if it exists as a word to avoid errors
--          Ex. "10%" has a suffix '%' which is a punctuation, not a numeral
--              that describes a percentage.
--    3. Then we find all possible sets of tags for each suffix, map them to
--       integers and avoid using the S.unions after submap
-- If a proper data structure is used no additional suffixes would be stored
-- only original words and for each a mapping to a set id. + reverse is removed

-- | Checks whether a word is in the analyzer. If it is the set of tags
-- returned by the 'getTags' will be non-empty.
elem :: T.Text -> Analyzer -> Bool
elem ts Analyzer{..} =
  case D.lookup (T.unpack . T.reverse $ ts) dawg of
    Nothing -> False
    Just _  -> True

-- | Transforms a given string to a model suited string.
-- Ex. Nsmnn -> N:s:m:n:n, or Vmp-sf -> V:m:p:9:s:f, all
-- '-' to '9'.
transformToConfig :: T.Text -> T.Text
transformToConfig = T.intersperse ':' . T.map toNine
  where toNine :: Char -> Char
        toNine x = if x == '-' || x == '='  then '9' else x