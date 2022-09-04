{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Common 
    (VestingDatum,
     beneficiary,
     deadline)
where

import           Ledger               hiding (singleton)

import qualified Plutus.V2.Ledger.Api           as PlutusV2

import qualified PlutusTx


data VestingDatum = VestingDatum
    { beneficiary :: PaymentPubKeyHash
    , deadline    :: PlutusV2.POSIXTime
    }

PlutusTx.unstableMakeIsData ''VestingDatum
