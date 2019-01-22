{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Frontend.Examples.DataTable.Types where

------------------------------------------------------------------------------
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Bifunctor.Tannen
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Compose
import           Data.List
import           Data.List.NonEmptyZipper
import           Data.Map (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.Vanishing
import qualified Text.Fuzzy as F
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Common
import           Text.Regex.TDFA.Text
------------------------------------------------------------------------------
import           Common.Examples.DataTable.PackageData
------------------------------------------------------------------------------

data Column = Col1 | Col2 | Col3 | Col4 | Col5
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

data SortOrder = Ascending | Descending
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

orderClass :: SortOrder -> Text
orderClass Ascending = "ascending"
orderClass Descending = "descending"

flipSortOrder :: SortOrder -> SortOrder
flipSortOrder Ascending = Descending
flipSortOrder Descending = Ascending

type SortInfo = (SortOrder, Column)

data Needle = StrNeedle Text | FuzzyNeedle Text | RegexNeedle Regex

data SearchAlgo = SubstringAlgo | FuzzyAlgo | RegexAlgo
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

showAlgo :: SearchAlgo -> Text
showAlgo SubstringAlgo = "Substring"
showAlgo FuzzyAlgo = "Fuzzy"
showAlgo RegexAlgo = "Regex"

data FlightStatus a = InFlight | ReturnedError String | ReturnedValue a

eitherToFlight :: Either String a -> FlightStatus a
eitherToFlight (Left s) = ReturnedError s
eitherToFlight (Right a) = ReturnedValue a

data SearchFields = SearchFields
    { _sfName          :: Bool
    , _sfSynopsis      :: Bool
    , _sfDescription   :: Bool
    , _sfCategories    :: Bool
    , _sfAuthor        :: Bool
    , _sfMaintainer    :: Bool
    }

data SearchInfo = SearchInfo
    { _siNeedle        :: Needle
    , _siFields        :: SearchFields
    , _siCaseSensitive :: Bool
    , _siSearchAlgo    :: SearchAlgo
    }

makeLenses ''SearchFields
makeLenses ''SearchInfo

noFields :: SearchFields
noFields = SearchFields False False False False False False

defFields :: SearchFields
defFields = SearchFields True True False False False False

defSI :: SearchInfo
defSI = SearchInfo (StrNeedle "") defFields True SubstringAlgo

