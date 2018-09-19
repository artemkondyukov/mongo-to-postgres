{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Arrow                          ((&&&))
import           Data.Aeson                             (FromJSON (..),
                                                         Result (..),
                                                         ToJSON (..), decode,
                                                         fromJSON, object,
                                                         withObject, (.:), (.=))
import           Data.Char                              (intToDigit)
import           Data.Either                            (rights)
import           Data.Text                              (Text (..), pack, unpack)
import qualified Database.MongoDB                       as MongoDB
import           Database.MongoDB.Wrapper               (fromDocument)
import           Database.PostgreSQL.Simple             (Only (..),
                                                         connectPostgreSQL,
                                                         execute, query)
import qualified Database.PostgreSQL.Simple             as PostgreSQL
import           Database.PostgreSQL.Simple.SqlQQ       (sql)
import           Database.PostgreSQL.Simple.ToRow       (toRow)
import           Network.Monique.Worker.StructureSearch (CompoundInfo (..),
                                                         CompoundQuantity (..))
import           Numeric                                (showIntAtBase)

data ChemDeptDBUnit = ChemDeptDBUnit {
  getFingerprint :: [Int],
  getSmiles      :: Text,
  getCompInf     :: CompoundInfo
} deriving (Eq, Show)

serializeUnit :: String -> String
serializeUnit "Millilitre" = "ml"
serializeUnit "Litre"      = "l"
serializeUnit "Milligram"  = "mg"
serializeUnit "Gram"       = "g"
serializeUnit "Kilogram"   = "kg"
serializeUnit "Mole"       = "mol"
serializeUnit "Micromole"  = "µmol"

instance FromJSON ChemDeptDBUnit where
  parseJSON = withObject "ChemDept base" $ \o -> ChemDeptDBUnit <$>
                                                 fpValuesParser o <*>
                                                 o .: "smiles" <*>
                                                 o .: "comp_inf"
    where
      fpLabels = pack . ("f" ++) . show <$> ([1..64] :: [Int])
      fpValuesParser o = mapM (o .:) fpLabels

instance ToJSON ChemDeptDBUnit where
  toJSON (ChemDeptDBUnit fp smi info) = object ([ "smiles"   .= smi
                                                , "comp_inf" .= info
                                                ] ++ zipWith (.=) fpLabels fp)
    where
      fpLabels = pack . ("f" ++) . show <$> ([1..64] :: [Int])

fromResult:: FromJSON a => Result a -> Either String a
fromResult (Error msg)      = Left ("*** Error: extracting DBUnit failed.\n" ++ msg)
fromResult (Success dBUnit) = Right dBUnit

intToBitString :: Int -> String
intToBitString n = let shortString = showIntAtBase 2 intToDigit n "" in
  replicate (64 - length shortString) '0' ++ shortString

putUnitQuery :: PostgreSQL.Connection -> ChemDeptDBUnit -> IO ()
putUnitQuery conn (ChemDeptDBUnit fp smi info) = do
    execute conn [sql|insert into Compounds ("smiles") values (?)|] $ toRow [smi]
    compoundId_ <- query conn [sql|select id from Compounds where smiles = ?|] $ toRow [smi] :: IO [Only Int]
    let cIdStr = case compoundId_ of
          (n:_) -> show $ fromOnly n
          _ -> error $ "*** Error: Compound has not been added to the table: " ++ unpack smi

    let (cAmount, cUnit) = (show . amount &&& serializeUnit . show . unit) $ compoundQuantityWh info
    cUnitId_ <- query conn [sql|select id from Units where unit = ?|] $ toRow [cUnit] :: IO [Only Int]
    let cUnitId = case cUnitId_ of
          (i:_) -> show $ fromOnly i
          _ -> error $ "*** Error: No unit in the database: " ++ cUnit

    let availabilityString = [cIdStr, cAmount, cUnitId]
    _ <- execute conn [sql|insert into Availabilities ("compound", "amount", "unit") values (?, ?, ?)|] availabilityString
    let fingerprintString = [cIdStr, concatMap intToBitString fp]
    print $ concatMap intToBitString fp
    _ <- execute conn [sql|insert into RDKFingerprints ("compound", "fingerprint") values (?, ?)|] fingerprintString
    pure ()

main :: IO ()
main = do
  pipe <- MongoDB.connect $ MongoDB.host "db.bb.biocad.ru"
  docs <- MongoDB.access pipe MongoDB.master "ChemDeptAll" allCompounds
  let unit = rights $ (fromResult <$>) fromJSON . fromDocument <$> take 5 docs :: [ChemDeptDBUnit]
  print unit

  conn <- connectPostgreSQL "host='localhost' port=5432 dbname='postgres' user='postgres' password='12345'"
  mapM_ (putUnitQuery conn) unit

allCompounds :: MongoDB.Action IO [MongoDB.Document]
allCompounds = MongoDB.rest =<< MongoDB.find (MongoDB.select [] "smilesToCompInfoFing")
