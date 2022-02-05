{-#LANGUAGE OverloadedLabels#-}
{-#LANGUAGE DeriveAnyClass #-}
{-#LANGUAGE DuplicateRecordFields #-}
{-#LANGUAGE TemplateHaskell#-}

{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module JsonOpticsPractice.GeocodeCity where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Optics
import Optics.Operators
import GHC.Generics (Generic)

data GeocodeCity = GeocodeCity
  {
    country :: String
  , latitude :: Double
  , longitude :: Double
  , name :: String
  , countryCode :: String
  , region :: String
  , population :: Integer
  , timezone :: String
  , district :: Maybe String
  } deriving (Show, Generic)
  
instance FromJSON GeocodeCity

-- lil exercise: display all results from Germany, sorted by population, in the format CITY [region]

test :: BL.ByteString
test = "[{\"country\":\"Germany\",\"latitude\":50.11552,\"name\":\"Frankfurt am Main\",\"countryCode\":\"DE\",\"region\":\"Hesse\",\"population\":650000,\"timezone\":\"Europe/Berlin\",\"longitude\":8.68417,\"district\":\"Regierungsbezirk Darmstadt\"},{\"country\":\"Germany\",\"latitude\":52.34714,\"name\":\"Frankfurt (Oder)\",\"countryCode\":\"DE\",\"region\":\"Brandenburg\",\"population\":51691,\"timezone\":\"Europe/Berlin\",\"longitude\":14.55062,\"district\":null},{\"country\":\"United States\",\"latitude\":38.20091,\"name\":\"Frankfort\",\"countryCode\":\"US\",\"region\":\"Kentucky\",\"population\":27830,\"timezone\":\"America/New_York\",\"longitude\":-84.87328,\"district\":\"Franklin County\"},{\"country\":\"Ukraine\",\"latitude\":49.91978,\"name\":\"Ivano-Frankove\",\"countryCode\":\"UA\",\"region\":\"Lviv\",\"population\":6223,\"timezone\":\"Europe/Kiev\",\"longitude\":23.72913,\"district\":null},{\"country\":\"Germany\",\"latitude\":51.35609,\"name\":\"Bad Frankenhausen\",\"countryCode\":\"DE\",\"region\":\"Thuringia\",\"population\":8824,\"timezone\":\"Europe/Berlin\",\"longitude\":11.09977,\"district\":null},{\"country\":\"Germany\",\"latitude\":50.91297,\"name\":\"Frankenberg\",\"countryCode\":\"DE\",\"region\":\"Saxony\",\"population\":16850,\"timezone\":\"Europe/Berlin\",\"longitude\":13.04011,\"district\":null},{\"country\":\"United States\",\"latitude\":43.33169,\"name\":\"Frankenmuth\",\"countryCode\":\"US\",\"region\":\"Michigan\",\"population\":5025,\"timezone\":\"America/Detroit\",\"longitude\":-83.73802,\"district\":\"Saginaw County\"},{\"country\":\"Ukraine\",\"latitude\":48.92312,\"name\":\"Ivano-Frankivsk\",\"countryCode\":\"UA\",\"region\":\"Ivano-Frankivsk Oblast\",\"population\":236602,\"timezone\":\"Europe/Kiev\",\"longitude\":24.71248,\"district\":\"Ivano-Frankivsk urban Raion\"},{\"country\":\"United States\",\"latitude\":36.10182,\"name\":\"Franklinton\",\"countryCode\":\"US\",\"region\":\"North Carolina\",\"population\":2135,\"timezone\":\"America/New_York\",\"longitude\":-78.45805,\"district\":\"Franklin County\"},{\"country\":\"United States\",\"latitude\":37.89783,\"name\":\"West Frankfort\",\"countryCode\":\"US\",\"region\":\"Illinois\",\"population\":8067,\"timezone\":\"America/Chicago\",\"longitude\":-88.93146,\"district\":\"Franklin County\"}]"

--decoded :: Value
-- >>> decoded
-- Just [GeocodeCity {country = "Germany", latitude = 50.11552, longitude = 8.68417, name = "Frankfurt am Main", countryCode = "DE", region = "Hesse", population = 650000, timezone = "Europe/Berlin", district = Just "Regierungsbezirk Darmstadt"},GeocodeCity {country = "Germany", latitude = 52.34714, longitude = 14.55062, name = "Frankfurt (Oder)", countryCode = "DE", region = "Brandenburg", population = 51691, timezone = "Europe/Berlin", district = Nothing},GeocodeCity {country = "United States", latitude = 38.20091, longitude = -84.87328, name = "Frankfort", countryCode = "US", region = "Kentucky", population = 27830, timezone = "America/New_York", district = Just "Franklin County"},GeocodeCity {country = "Ukraine", latitude = 49.91978, longitude = 23.72913, name = "Ivano-Frankove", countryCode = "UA", region = "Lviv", population = 6223, timezone = "Europe/Kiev", district = Nothing},GeocodeCity {country = "Germany", latitude = 51.35609, longitude = 11.09977, name = "Bad Frankenhausen", countryCode = "DE", region = "Thuringia", population = 8824, timezone = "Europe/Berlin", district = Nothing},GeocodeCity {country = "Germany", latitude = 50.91297, longitude = 13.04011, name = "Frankenberg", countryCode = "DE", region = "Saxony", population = 16850, timezone = "Europe/Berlin", district = Nothing},GeocodeCity {country = "United States", latitude = 43.33169, longitude = -83.73802, name = "Frankenmuth", countryCode = "US", region = "Michigan", population = 5025, timezone = "America/Detroit", district = Just "Saginaw County"},GeocodeCity {country = "Ukraine", latitude = 48.92312, longitude = 24.71248, name = "Ivano-Frankivsk", countryCode = "UA", region = "Ivano-Frankivsk Oblast", population = 236602, timezone = "Europe/Kiev", district = Just "Ivano-Frankivsk urban Raion"},GeocodeCity {country = "United States", latitude = 36.10182, longitude = -78.45805, name = "Franklinton", countryCode = "US", region = "North Carolina", population = 2135, timezone = "America/New_York", district = Just "Franklin County"},GeocodeCity {country = "United States", latitude = 37.89783, longitude = -88.93146, name = "West Frankfort", countryCode = "US", region = "Illinois", population = 8067, timezone = "America/Chicago", district = Just "Franklin County"}]
decoded :: Maybe [GeocodeCity]
decoded = decode test

-- >>> allNames
-- ["Frankfurt am Main","Frankfurt (Oder)","Frankfort","Ivano-Frankove","Bad Frankenhausen","Frankenberg","Frankenmuth","Ivano-Frankivsk","Franklinton","West Frankfort"]
allNames :: [String]
allNames = decoded ^.. (_Just % folded % #name)
