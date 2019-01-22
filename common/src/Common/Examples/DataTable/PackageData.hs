{-# LANGUAGE OverloadedStrings #-}

module Common.Examples.DataTable.PackageData where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Aeson as A
import           Data.Scientific
import qualified Data.Vector as V
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
------------------------------------------------------------------------------

data PackageData = PackageData
    { packageDataName         :: Text
    , packageDataSynopsis     :: Text
    , packageDataDescription  :: Text
    , packageDataCategory     :: Text
    , packageDataAuthor       :: Text
    , packageDataMaintainer   :: Text
    , packageDataDownloads    :: Maybe Int
    , packageDataLastUploaded :: Maybe Day
    } deriving (Eq,Ord,Show,Read)

instance ToJSON PackageData where
  toJSON (PackageData n s d c a m nd lu) =
    A.Array $ V.fromList [ A.String n, A.String s, A.String d
                         , A.String c, A.String a, A.String m
                         , mkNum nd
                         , maybe (A.String "") formatDay lu]

mkNum :: Integral a => Maybe a -> Value
mkNum Nothing = A.Null
mkNum (Just nd) = A.Number $ scientific (fromIntegral nd) 0

instance FromJSON PackageData where
  parseJSON (A.Array v) =
    case V.toList v of
      [A.String n, A.String s, A.String d, A.String c,
       A.String a, A.String m, A.Number nd, A.String lu] ->
        return $ PackageData n s d c a m (toBoundedInteger nd) (parseDay lu)
      vs -> fail $ unlines
        [ "PackageData must be an array of six strings.  Got: "
        , show vs
        ]
  parseJSON invalid = typeMismatch "PackageData" invalid

formatDay :: Day -> Value
formatDay = A.String . T.pack . formatTime defaultTimeLocale "%F"

parseDay :: Text -> Maybe Day
parseDay t = parseTimeM True defaultTimeLocale "%F" $ T.unpack t
