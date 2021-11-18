-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.ZH.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Quantity.Helpers
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Quantity.Types as TQuantity

quantities :: [(Text, String, TQuantity.Unit)]
quantities =
  [ ("<quantity> milliGram", "(毫克|mg|MG)", TQuantity.MilliGram )
  , ("<quantity> grams", "(克|g|G)", TQuantity.Gram)
  , ("<quantity> kiloGram", "(千克|kg|KG|公斤)", TQuantity.KiloGram )
  , ("<quantity> ton", "(吨|t|T)", TQuantity.Ton )
  , ("<quantity> jin", "(斤)", TQuantity.Jin )
  , ("<quantity> liang", "(两|兩)", TQuantity.Liang )
  ]

opsMap :: HashMap Text (Double -> Double)
opsMap = HashMap.fromList
  [ ( "千克" , (* 1))
  , ( "公斤",  (* 1))
  , ( "kg",   (* 1))
  , ( "KG",   (* 1))
  , ( "斤",    (* 1))
  , ( "两",    (* 1))
  , ( "兩",    (* 1))
  , ( "毫克",  (* 1))
  , ( "mg",   (* 1))
  , ( "MG",   (* 1))
  , ( "吨",   (* 1))
  , ( "t",    (* 1))
  , ( "T",    (* 1))

  ]


ruleNumeralQuantities :: [Rule]
ruleNumeralQuantities = map go quantities
  where
    getValue :: Text -> Double -> Double
    getValue match = HashMap.lookupDefault id (Text.toLower match) opsMap

    go :: (Text, String, TQuantity.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern =
        [ Predicate isPositive
        , regex regexPattern
        ]
      , prod = \case
        (Token Numeral nd:
         Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token Quantity $ quantity u value
          where value = getValue match $ TNumeral.value nd
        _ -> Nothing
      }

ruleCattyTael :: Rule
ruleCattyTael = Rule
  { name = "<quantity> catty <quantity> tael"
  , pattern =
    [ Predicate isPositive
    , regex "斤"
    , numberBetween 1 10
    , regex "两|兩"
    ]
  , prod = \case
    (Token Numeral TNumeral.NumeralData{TNumeral.value = x}:
     Token RegexMatch (GroupMatch _):
     Token Numeral TNumeral.NumeralData{TNumeral.value = y}:
     Token RegexMatch (GroupMatch _):
     _) -> Just . Token Quantity $ quantity TQuantity.Jin (x + y / 10)
    _ -> Nothing
  }

ruleCattyHalf :: Rule
ruleCattyHalf = Rule
  { name = "<quantity> catty half"
  , pattern =
    [ Predicate isPositive
    , regex "斤半"
    ]
  , prod = \case
    (Token Numeral TNumeral.NumeralData{TNumeral.value = x}:
     Token RegexMatch (GroupMatch _):
     _) -> Just . Token Quantity $ quantity TQuantity.Jin (x + 0.5)
    _ -> Nothing
  }

ruleTaelHalf :: Rule
ruleTaelHalf = Rule
  { name = "<quantity> tael half"
  , pattern =
    [ Predicate isPositive
    , regex "(两|兩)半"
    ]
  , prod = \case
    (Token Numeral TNumeral.NumeralData{TNumeral.value = x}:
     Token RegexMatch (GroupMatch _):
     _) -> Just . Token Quantity $ quantity TQuantity.Liang (x + 0.5)
    _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleCattyTael
  , ruleCattyHalf
  , ruleTaelHalf
  ]
  ++ ruleNumeralQuantities
