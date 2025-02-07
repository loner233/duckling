-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.ZH.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "s|秒(钟)?", TG.Second)
         , ("minute (grain)", "min|分(钟)?", TG.Minute)
         , ("hour (grain)", "h|小时|小時|个钟头?", TG.Hour)
         , ("day (grain)", "d|天|日", TG.Day)
         , ("week (grain)", "周|週|礼拜|禮拜|星期", TG.Week)
         , ("month (grain)", "月", TG.Month)
         , ("quarter (grain)", "季度", TG.Quarter)
         , ("year (grain)", "年", TG.Year)
         , ("century (grain)", "世纪", TG.Century)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
