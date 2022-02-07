{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}
module JsonOpticsPractice.GeneralizingTraversals where

-- | Translation of this article
-- https://chrispenner.ca/posts/traversal-systems
-- to the optics library, using Generic deriving of optics
-- as overloaded labels.

import Data.Aeson.QQ.Simple
import Data.Aeson
import qualified Data.Map as M
import GHC.Generics (Generic)
import Optics
import Optics.Operators
import Data.Aeson.Types (parseEither)
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (for_)
import Control.Monad.Trans.Maybe
import Data.Maybe (maybeToList)
import Data.Aeson.Optics
import qualified Data.Text as T

data Company = Company
  { staff :: [Employee],
    salaries :: M.Map Int Int
  }
  deriving (Show, Generic, FromJSON)

data Pet = Pet
  { petName :: String,
    petType :: String
  }
  deriving (Show, Generic)

instance FromJSON Pet where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = dropPrefix "pet"}

data Employee = Employee
  { employeeId :: Int,
    employeeName :: String,
    employeePets :: [Pet]
  }
  deriving (Show, Generic)

instance FromJSON Employee where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = dropPrefix "employee"}

dropPrefix :: String -> String -> String
dropPrefix p = drop (length $ p <> "_") . camelTo2  '_'


-- EX. 1: all pets subset:
-- >>> ex1
-- [Pet {petName = "Rocky", petType = "cat"},Pet {petName = "Inigo", petType = "cat"}]
ex1 :: [Pet]
ex1 =
  company
  & toListOf (#staff % folded % #employeePets % folded % filteredBy (#petType % only "cat"))

-- EX. 2: keeping references, the hard way
-- NOTE(luis) wasn't able to make this work yet, not sure what the equivalents of
-- the equivalent of `(<.)` seems to be (<%) in optics, though % does the right thing, too.
-- I couldn't find a good equivalent of `withIndex` so had to break the optics
-- composition early and apply a map instead -- hopefully it'll come to me!
-- >>> owners
-- ["Rocky belongs to bob","Bullwinkle belongs to bob","Inigo belongs to sally"]
owners :: [String]
owners =
  company &
    itoListOf (#staff % folded % reindexed employeeName (selfIndex % #employeePets % folded % #petName))
    & map (\(eName, pName) -> pName <> " belongs to " <> eName)
    -- 

-- EX. 2.1: keeping references, the monadic way
-- NOTE(luis) this is _almost_ like the example, except that we
-- have to use `magnifyMany` for the resulting folds, and `gview`
-- both are instances of "unsound things one gets away with in Lens"
-- >>> runReader owners' company
-- ["Rocky belongs to bob","Bullwinkle belongs to bob","Inigo belongs to sally"]
owners' :: Reader Company [String]
owners' = do
  magnifyMany (#staff % folded) $ do
    personName <- gview #employeeName
    magnifyMany (#employeePets % folded) $ do
      animalName <- gview #petName
      return [animalName <> " belongs to " <> personName]


-- Using the (->) Monad directly: note that we can again use @view@
-- since we don't need the generalized one that works in Reader:
-- NB: the article says that we can skip the indentation... but that
-- didn't seem to work, my IDE complained that the do blocks were empty??
-- >>> owners'' company
-- ["Rocky belongs to bob","Bullwinkle belongs to bob","Inigo belongs to sally"]
owners'' :: Company -> [String]
owners'' = do
  magnifyMany (#staff % folded) $ do
    eName <- view #employeeName
    magnifyMany (#employeePets % folded) $ do
      pName <- view #petName
      return [pName <> " belongs to " <> eName]

-- EX.3 stateful updates
-- https://hackage.haskell.org/package/lens-5.1/docs/Control-Lens-Combinators.html#v:filteredBy
-- NOTE: filteredBy _really_ freaks out if we give it a `folded` optic, since that produces a Fold
-- and "A_Fold cannot be used as An_AffineFold"; fortunately `filtered` can take a function
-- that extracts an affine fold from a boolean function, so once again I had to split the optics
-- >>> execState salaryBump company
-- Company {staff = [Employee {employeeId = 1, employeeName = "bob", employeePets = [Pet {petName = "Rocky", petType = "cat"},Pet {petName = "Bullwinkle", petType = "dog"}]},Employee {employeeId = 2, employeeName = "sally", employeePets = [Pet {petName = "Inigo", petType = "cat"}]}], salaries = fromList [(1,17),(2,15)]}
salaryBump :: State Company ()
salaryBump = do
  ids <- gets $ toListOf
          (#staff
          % traversed
          % filtered hasDog--NOT: filteredBy (#employeePets % folded % #petType % only "dog")
          % #employeeId
          )
  for_ ids $ \id' ->
    modifying (#salaries % ix id') (+5)

hasDog :: Employee -> Bool
hasDog e = not . null $ e ^.. #employeePets % folded % #petType % only "dog"

-- EX. 3.1 stateful updates, but using zoom
-- >>> execState salaryBump' company
-- Company {staff = [Employee {employeeId = 1, employeeName = "bob", employeePets = [Pet {petName = "Rocky", petType = "cat"},Pet {petName = "Bullwinkle", petType = "dog"}]},Employee {employeeId = 2, employeeName = "sally", employeePets = [Pet {petName = "Inigo", petType = "cat"}]}], salaries = fromList [(1,17),(2,15)]}
salaryBump' :: State Company ()
salaryBump' = do
  ids <- zoomMany (#staff
                  % traversed
                  -- NOTE(luis) using unsafeFiltered because otherwise we're told
                  -- that A_Fold cannot be used as A_Traversal
                  % unsafeFiltered hasDog
                  ) $ do
                    guses #employeeId (:[])
  for_ ids $ \id' ->
    modifying (#salaries % ix id') (+5)

-- NOTE(luis) renaming this operator because optics already exports %>
-- also, note that we're accepting both indexed and unindexed traversals
infixr 0 %>?
(%>?) :: Optic' A_Traversal ix s e ->  MaybeT (State e) a -> MaybeT (State s) [a]
l %>? m = do
  -- unlike the blog: using zoomMany because
  -- A_Traversal cannot be used as A_Lens
  zoomMany l $ do
    a <- lift $ runMaybeT m
    -- rewrote: (maybe [] (:[]) a)
    return $ maybeToList a

--- EX. 3.2 using the wonky MaybeT phrasing to prune earlier in the traversal
-- >>> flip execState company . runMaybeT $ salaryBump''
-- Company {staff = [Employee {employeeId = 1, employeeName = "bob", employeePets = [Pet {petName = "Rocky", petType = "cat"},Pet {petName = "Bullwinkle", petType = "dog"}]},Employee {employeeId = 2, employeeName = "sally", employeePets = [Pet {petName = "Inigo", petType = "cat"}]}], salaries = fromList [(1,17),(2,15)]}
salaryBump'' :: MaybeT (State Company) ()
salaryBump'' = do
  ids <- #staff % traversed %>? do
          isDog <- #employeePets % traversed %>? do
              guses #petType (== "dog")
          guard (or isDog)
          use #employeeId
  for_ ids $ \id' ->
    modifying (#salaries % ix id') (+5)

-- EX. 4 working with JSON directly:
-- >>> flip execState companyV . runMaybeT $ salaryBumpJSON
-- Object (fromList [("salaries",Object (fromList [("1",Number 17.0),("2",Number 15.0)])),("staff",Array [Object (fromList [("id",Number 1.0),("name",String "bob"),("pets",Array [Object (fromList [("name",String "Rocky"),("type",String "cat")]),Object (fromList [("name",String "Bullwinkle"),("type",String "dog")])])]),Object (fromList [("id",Number 2.0),("name",String "sally"),("pets",Array [Object (fromList [("name",String "Inigo"),("type",String "cat")])])])])])
salaryBumpJSON :: MaybeT (State Value) ()
salaryBumpJSON = do
  ids <- key "staff" % values %>? do
          isDog <- key "pets" % values %>? do
            pType <- guse (key "type" % _String)
            -- NOTE(luis) the blog post compares with a string directly,
            -- I _think_ that has to be an error, since the lens variants
            -- also would yield a Maybe at this point?
            pure $ pType == Just "dog"
          guard (or isDog)
          guse (key "id" % _Integer)
  --pure ()
  -- NOTE(luis) the blog post uses ids as if they were [Integer] (or rather, [Text]),
  -- but they're in fact [Maybe ...]
  for_ (ids ^.. traversed % _Just) $ \id' ->
    modifying (key "salaries" % key (T.pack . show $ id') % _Integer) (+5)

-- >>> company'
-- Right (Company {staff = [Employee {employeeId = 1, employeeName = "bob", employeePets = [Pet {petName = "Rocky", petType = "cat"},Pet {petName = "Bullwinkle", petType = "dog"}]},Employee {employeeId = 2, employeeName = "sally", employeePets = [Pet {petName = "Inigo", petType = "cat"}]}], salaries = fromList [(1,12),(2,15)]})
company' :: Either String Company
company' =
  parsed companyV
  where
    parsed = parseEither parseJSON

-- | naughty shortcut to work with the same value as the blog:
-- >>> company
-- Company {staff = [Employee {employeeId = 1, employeeName = "bob", employeePets = [Pet {petName = "Rocky", petType = "cat"},Pet {petName = "Bullwinkle", petType = "dog"}]},Employee {employeeId = 2, employeeName = "sally", employeePets = [Pet {petName = "Inigo", petType = "cat"}]}], salaries = fromList [(1,12),(2,15)]}
company :: Company
company =
  let (Right parsed) = parseEither parseJSON companyV
  in parsed

companyV :: Value
companyV = [aesonQQ|
{
    "staff":
      [
        { "id": 1
        , "name": "bob"
        , "pets": [
              { "name": "Rocky"
              , "type": "cat"
              },
              { "name": "Bullwinkle"
              , "type": "dog"
              }
            ]
        },
        { "id": 2
        , "name": "sally"
        , "pets": [
              { "name": "Inigo"
              , "type": "cat"
              }
            ]
        }
      ],
    "salaries": {
        "1": 12,
        "2": 15
    }
}
|]
