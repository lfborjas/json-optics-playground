{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}

module JsonOpticsPractice.Horoscope where

import Data.Aeson
import Optics
import GHC.Generics (Generic)
import Data.Aeson.Types (parseMaybe)
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson.QQ.Simple
import Data.Aeson.Optics 
import Text.Read (readMaybe)
import qualified Data.Text as T

data Planet
  = Sun
  | Moon
  | Mercury
  | Venus
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune
  | Pluto
  | MeanNode
  | TrueNode
  | MeanApog
  | OscuApog
  | Earth
  | Chiron
  deriving (Show, Read)

instance FromJSON Planet where
  parseJSON = withText "Planet" $ \s -> 
    case readMaybe (T.unpack s) of
      Nothing -> fail "Invalid Planet"
      Just p -> pure p
      
data SpeedData = SpeedData
  { unSplit :: Double }
  deriving (Show, Generic, FromJSON)

data LongitudeData = LongitudeData
  { zodiacSign :: String }
  deriving (Show, Generic, FromJSON)

data HouseData = HouseData
  { label :: String }
  deriving (Show, Generic, FromJSON)

data PlanetPosition = PlanetPosition
  { planet :: Planet
  , speed :: SpeedData
  , longitude :: LongitudeData
  , houseNumber :: HouseData
  }
  deriving (Show, Generic, FromJSON)

data Horoscope = Horoscope
  {planetPositions :: [PlanetPosition]}
  deriving (Show, Generic, FromJSON)

dataDecoded :: Maybe Horoscope
dataDecoded =
  -- using the `Ixed Value` instance from aeson-optics.
  -- note that `key` also works!
  fromJSONValue =<< (testData ^? ix "data" % ix "horoscope")
  where
    fromJSONValue = parseMaybe parseJSON

-- | optic to find all planets in a given sign
inSign :: String -> Optic' A_Fold '[()] (Maybe Horoscope) PlanetPosition
inSign signName = _Just % #planetPositions % folded % filteredBy (#longitude % #zodiacSign % only signName)

-- | return a tuple of planets and their house labels
-- >>> flip runReader dataDecoded inHouses
-- [(Sun,"IC"),(Moon,"III"),(Saturn,"III"),(Uranus,"III"),(Neptune,"III")]
inHouses :: Reader (Maybe Horoscope) [(Planet, String)]
inHouses = do
  magnifyMany (inSign "Capricorn") $ do
    pos <- ask
    return [(pos ^. #planet, pos ^. (#houseNumber % #label))]

-- Same as 'inHouses', but using @gview@ to approximate the examples from:
-- https://chrispenner.ca/posts/traversal-systems  
-- >>> flip runReader dataDecoded inHouses2
-- [(Sun,"IC"),(Moon,"III"),(Saturn,"III"),(Uranus,"III"),(Neptune,"III")]
inHouses2 :: Reader (Maybe Horoscope) [(Planet, String)]
inHouses2 = do
  magnifyMany (inSign "Capricorn") $ do
    pl <- gview #planet
    magnify #houseNumber $ do
      lbl <- gview #label
      return [(pl, lbl)]

makeRetrograde :: State (Maybe Horoscope) ()
makeRetrograde = do
  zoomMany (_Just % #planetPositions % traversed) $ do
    modifying #speed setRetrograde
  where
    setRetrograde d@SpeedData{unSplit} =
      if unSplit > 0 then d{unSplit = negate unSplit} else d

-- >>> madeRetrograde
-- [-1.0196359986110741,-13.947422051450813,-1.1903897069829164,-1.251451014550458,-0.5273338733007417,-4.477182214727865e-2,-0.11711016716347317,-5.89714837583241e-2,-3.7757418150684646e-2,-2.328425047258476e-2,-5.2901520421361925e-2,-0.11093337702602891,-6.345430815724024e-2]
madeRetrograde :: [Double]
madeRetrograde = execState makeRetrograde dataDecoded ^.. _Just % #planetPositions % folded % #speed % #unSplit

testData :: Value
testData = [aesonQQ|
{
  "data": {
    "horoscope": {
      "planetPositions": [
        {
          "planet": "Sun",
          "speed": {
            "unSplit": 1.0196359986110741
          },
          "longitude": {
            "zodiacSign": "Capricorn"
          },
          "houseNumber": {
            "label": "IC"
          }
        },
        {
          "planet": "Moon",
          "speed": {
            "unSplit": 13.947422051450813
          },
          "longitude": {
            "zodiacSign": "Capricorn"
          },
          "houseNumber": {
            "label": "III"
          }
        },
        {
          "planet": "Mercury",
          "speed": {
            "unSplit": 1.1903897069829164
          },
          "longitude": {
            "zodiacSign": "Aquarius"
          },
          "houseNumber": {
            "label": "IC"
          }
        },
        {
          "planet": "Venus",
          "speed": {
            "unSplit": 1.251451014550458
          },
          "longitude": {
            "zodiacSign": "Sagittarius"
          },
          "houseNumber": {
            "label": "III"
          }
        },
        {
          "planet": "Mars",
          "speed": {
            "unSplit": 0.5273338733007417
          },
          "longitude": {
            "zodiacSign": "Aries"
          },
          "houseNumber": {
            "label": "Desc"
          }
        },
        {
          "planet": "Jupiter",
          "speed": {
            "unSplit": -0.04477182214727865
          },
          "longitude": {
            "zodiacSign": "Taurus"
          },
          "houseNumber": {
            "label": "VIII"
          }
        },
        {
          "planet": "Saturn",
          "speed": {
            "unSplit": 0.11711016716347317
          },
          "longitude": {
            "zodiacSign": "Capricorn"
          },
          "houseNumber": {
            "label": "III"
          }
        },
        {
          "planet": "Uranus",
          "speed": {
            "unSplit": 0.0589714837583241
          },
          "longitude": {
            "zodiacSign": "Capricorn"
          },
          "houseNumber": {
            "label": "III"
          }
        },
        {
          "planet": "Neptune",
          "speed": {
            "unSplit": 0.037757418150684646
          },
          "longitude": {
            "zodiacSign": "Capricorn"
          },
          "houseNumber": {
            "label": "III"
          }
        },
        {
          "planet": "Pluto",
          "speed": {
            "unSplit": 0.02328425047258476
          },
          "longitude": {
            "zodiacSign": "Scorpio"
          },
          "houseNumber": {
            "label": "II"
          }
        },
        {
          "planet": "MeanNode",
          "speed": {
            "unSplit": -0.052901520421361925
          },
          "longitude": {
            "zodiacSign": "Pisces"
          },
          "houseNumber": {
            "label": "V"
          }
        },
        {
          "planet": "OscuApog",
          "speed": {
            "unSplit": 0.11093337702602891
          },
          "longitude": {
            "zodiacSign": "Virgo"
          },
          "houseNumber": {
            "label": "XII"
          }
        },
        {
          "planet": "Chiron",
          "speed": {
            "unSplit": -0.06345430815724024
          },
          "longitude": {
            "zodiacSign": "Cancer"
          },
          "houseNumber": {
            "label": "IX"
          }
        }
      ]
    }
  }
}
|]
