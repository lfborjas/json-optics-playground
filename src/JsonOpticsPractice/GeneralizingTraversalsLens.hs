{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE RankNTypes #-}
module JsonOpticsPractice.GeneralizingTraversalsLens where

import qualified Data.Map as M
import Control.Lens.TH
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (for_)
import Control.Monad.Trans.Maybe
import Data.Maybe (maybeToList)
import Data.Aeson
-- NOTE(luis): make sure to use lens-aeson: the aeson-lens package is not the right one!!
import Data.Aeson.Lens


data Company = Company { _staff :: [Employee]
                       , _salaries :: M.Map Int Int
                       } deriving Show
data Pet = Pet { _petName :: String
               , _petType :: String
               } deriving Show
data Employee = Employee { _employeeId :: Int
                         , _employeeName :: String
                         , _employeePets :: [Pet]
                         } deriving Show

makeLenses ''Company
makeLenses ''Pet
makeLenses ''Employee

company :: Company
company = Company [ Employee 1 "bob" [Pet "Rocky" "cat", Pet "Bullwinkle" "dog"]
                  , Employee 2 "sally" [Pet "Inigo" "cat"]
                  ] (M.fromList [ (1, 12)
                                , (2, 15)
                                ])

-- EX. 1
-- >>> ex1
-- [Pet {_petName = "Rocky", _petType = "cat"},Pet {_petName = "Inigo", _petType = "cat"}]
ex1 :: [Pet]
ex1 =
  company
  & toListOf (staff . folded . employeePets . folded . filteredBy (petType . only "cat"))

-- EX. 2
-- >>> owners
-- ["Rocky belongs to bob","Bullwinkle belongs to bob","Inigo belongs to sally"]
owners :: [String]
owners =
  company ^..
    (staff . folded . reindexed _employeeName selfIndex <. employeePets . folded . petName)
    -- _ :: ((String, String) -> Const (Endo [String]) (String, String))
    --  -> Indexed String String (Const (Endo [String]) String)
    . withIndex
    . to (\(eName, pName) -> pName <> " belongs to " <> eName)

-- EX. 2.1
-- >>> runReader owners' company
-- ["Rocky belongs to bob","Bullwinkle belongs to bob","Inigo belongs to sally"]
owners' :: Reader Company [String]
owners' = do
    magnify (staff . folded) $ do
        personName <- view employeeName
        magnify (employeePets . folded) $ do
            animalName <- view petName
            return [animalName <> " belongs to " <> personName]

-- EX 2.2.
-- NOTE(luis) definitely had to indent
-- >>> owners'' company
-- ["Rocky belongs to bob","Bullwinkle belongs to bob","Inigo belongs to sally"]
owners'' :: Company -> [String]
owners'' = do
  magnify (staff . folded) $ do
    eName <- view employeeName
    magnify (employeePets . folded) $ do
      pName <- view petName
      return [pName <> " belongs to " <> eName]

-- EX. 3
-- >>> execState salaryBump company
-- Company {_staff = [Employee {_employeeId = 1, _employeeName = "bob", _employeePets = [Pet {_petName = "Rocky", _petType = "cat"},Pet {_petName = "Bullwinkle", _petType = "dog"}]},Employee {_employeeId = 2, _employeeName = "sally", _employeePets = [Pet {_petName = "Inigo", _petType = "cat"}]}],
-- _salaries = fromList [(1,17),(2,15)]}
salaryBump :: State Company ()
salaryBump = do
    ids <- gets $ toListOf
            ( staff
            . traversed
            . filteredBy (employeePets . traversed . petType . only "dog")
            . employeeId
            )
    for_ ids $ \id' ->
        salaries . ix id' += 5
-- EX 3.1
-- >>> execState salaryBump company
-- Company {_staff = [Employee {_employeeId = 1, _employeeName = "bob", _employeePets = [Pet {_petName = "Rocky", _petType = "cat"},Pet {_petName = "Bullwinkle", _petType = "dog"}]},Employee {_employeeId = 2, _employeeName = "sally", _employeePets = [Pet {_petName = "Inigo", _petType = "cat"}]}], _salaries = fromList [(1,17),(2,15)]}
salaryBump' :: State Company ()
salaryBump' = do
    ids <- zoom ( staff
                . traversed
                . filteredBy (employeePets . traversed . petType . only "dog")
                ) $ do
              uses employeeId (:[])
    for_ ids $ \id' ->
        salaries . ix id' += 5

-- NOTE(luis) requires RankNTypes
infixr 0 %>
(%>) :: Traversal' s e -> MaybeT (State e) a -> MaybeT (State s) [a]
l %> m = do
    zoom l $ do
        -- Catch and embed the current branch so we don't fail the whole program
        a <- lift $ runMaybeT m
        return (maybeToList a)

-- EX. 3.2
-- >>> flip execState company . runMaybeT $ salaryBump''
-- Company {_staff = [Employee {_employeeId = 1, _employeeName = "bob", _employeePets = [Pet {_petName = "Rocky", _petType = "cat"},Pet {_petName = "Bullwinkle", _petType = "dog"}]},Employee {_employeeId = 2, _employeeName = "sally", _employeePets = [Pet {_petName = "Inigo", _petType = "cat"}]}], _salaries = fromList [(1,17),(2,15)]}
salaryBump'' :: MaybeT (State Company) ()
salaryBump'' = do
    ids <- staff . traversed %> do
            isDog <- employeePets . traversed %> do
                       uses petType (== "dog")
            guard (or isDog)
            use employeeId
    for_ ids $ \id' ->
        salaries . ix id' += 5

salaryBumpJSON :: MaybeT (State Value) ()
salaryBumpJSON = do
    ids <- key "staff" . values %> do
            isDog <- key "pets" . values %> do
              pType <- use (key "type" . _String)
              return $ pType == "dog"
            guard (or isDog)
            use (key "id" . _String)
    for_ ids $ \id' ->
        key "salaries" . key id' . _Integer += 5
