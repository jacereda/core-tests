module Test.Data.Enum where

import Data.Maybe
import Data.Enum
import Debug.Trace
import Control.Monad.Eff
import Test.QuickCheck

data TestEnum = A | B | C

instance eqTestEnum :: Eq TestEnum where
  (==) A A = true
  (==) B B = true
  (==) C C = true
  (==) _ _ = false

  (/=) a b = not (a == b)

instance ordTestEnum :: Ord TestEnum where
  compare a b = fromEnum a `compare` fromEnum b

instance enumTestEnum :: Enum TestEnum where
  cardinality = Cardinality 3

  firstEnum = A

  lastEnum = C

  succ A = Just B
  succ B = Just C
  succ C = Nothing

  pred A = Nothing
  pred B = Just A
  pred C = Just B

  toEnum x = defaultToEnum succ firstEnum x

  fromEnum x = defaultFromEnum pred x

main = do

  let ty = Just 0

  trace "succ should return the next enum value"
  assert $ succ A == Just B
  assert $ succ B == Just C
  
  trace "succ should return nothing for the last enum value in a sequence"
  assert $ succ C == Nothing
  
  trace "pred should return the previous enum value"
  assert $ pred B == Just A
  assert $ pred C == Just B
  
  trace "pred should return nothing for the first enum value in a sequence"
  assert $ pred A == Nothing

  trace "toEnum should return the first element for 0"
  assert $ toEnum 0 == Just A

  trace "toEnum should return the last element for 2"
  assert $ toEnum 2 == Just C

  trace "toEnum should return nothing for an out of range value"
  assert $ toEnum 3 == Nothing :: Maybe TestEnum
  assert $ toEnum (-1) == Nothing  :: Maybe TestEnum

  trace "fromEnum should return the corresponding ordinal"
  assert $ fromEnum A == 0
  assert $ fromEnum C == 2  


assert :: Boolean -> QC Unit
assert = quickCheck' 1
