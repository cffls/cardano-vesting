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

module Main where

import           Control.Monad        hiding (fmap)

import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS


import           Codec.Serialise
import           Cardano.Api                    (PlutusScriptV2, writeFileTextEnvelope)
import           Cardano.Api.Shelley            (PlutusScript (PlutusScriptSerialised))

import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..))
import           Ledger               hiding (singleton)

import qualified Plutus.Script.Utils.V2.Scripts as PSU.V2
import qualified Plutus.V2.Ledger.Contexts as PSU.V2

import qualified Plutus.V2.Ledger.Api           as PlutusV2

import           Prelude              (IO)


data VestingDatum = VestingDatum
    { beneficiary :: PaymentPubKeyHash
    , deadline    :: PlutusV2.POSIXTime
    }

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> PlutusV2.ScriptContext -> Bool
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = PSU.V2.txSignedBy info $ unPaymentPubKeyHash $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ PSU.V2.txInfoValidRange info


validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = PSU.V2.mkUntypedValidator mkValidator


scriptSBSV2 :: SBS.ShortByteString
scriptSBSV2 = SBS.toShort . LBS.toStrict $ serialise validator


serialisedScriptV2 :: PlutusScript PlutusScriptV2
serialisedScriptV2 = PlutusScriptSerialised scriptSBSV2

main :: IO ()
main = do
    void $ writeFileTextEnvelope "vesting-plutusV2.plutus" Nothing serialisedScriptV2
