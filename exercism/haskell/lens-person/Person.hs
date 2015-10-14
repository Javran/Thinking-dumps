{-# LANGUAGE TemplateHaskell #-}
module Person where

import Data.Time.Calendar
import Control.Lens

data Name = Name
  { _foreNames :: String -- Space separated
  , _surName   :: String
  }

data Address = Address
  { _street :: String
  , _houseNumber :: Int
  , _place :: String -- Village / city
  , _country :: String
  }

data Born = Born
  { _bornAt :: Address
  , _bornOn :: Day
  }

-- Valid values of Gregorian are those for which 'Data.Time.Calendar.fromGregorianValid'
-- returns Just.
data Gregorian = Gregorian
  { _year :: Integer
  , _month :: Int
  , _day :: Int
  }

data Person = Person
  { _name :: Name
  , _born :: Born
  , _address :: Address
  }

makeLenses ''Name
makeLenses ''Address
makeLenses ''Born
makeLenses ''Person

-- Implement these.

bornStreet :: Born -> String
bornStreet = (^. (bornAt . street))

setCurrentStreet :: String -> Person -> Person
setCurrentStreet newStreet = (& (address . street) .~ newStreet)

setBirthMonth :: Int -> Person -> Person
setBirthMonth bm = (& (born . bornOn) %~ modifyMonth bm)
  where
    modifyMonth :: Int -> Day -> Day
    modifyMonth newM d = fromGregorian yyyy newM dd
      where
        (yyyy,_,dd) = toGregorian d

-- | Transform both birth and current street names.
renameStreets :: (String -> String) -> Person -> Person
renameStreets = undefined
