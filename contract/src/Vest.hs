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

import System.Environment

import           Data.String (IsString(fromString))
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

import           Prelude              (IO, putStrLn)
import qualified Ledger.Ada as PlutusV2
import Plutus.V1.Ledger.Bytes (fromHex)

import Common (VestingDatum(..))


minVestFee :: Integer
minVestFee = 0


{-# INLINABLE mkValidator #-}
mkValidator :: PubKeyHash -> VestingDatum -> () -> PlutusV2.ScriptContext -> Bool
mkValidator pkh dat () ctx = traceIfFalse "Fund sent to wrong party or insufficient fund is sent" spentCorrectly &&
                        traceIfFalse "insufficient fee" paidVestFee
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    paidVestFee :: Bool
    paidVestFee = vestFee >= minVestFee

    spentCorrectly :: Bool
    spentCorrectly = paidBeneficiary || (granterCancelled && granterSigned)

    paidBeneficiary :: Bool
    paidBeneficiary = (amtSentToBeneficiaryScript >= minValue || amtSentToBeneficiary >= minValue) && deadlineReached && onlyOneInput

    granterCancelled :: Bool
    granterCancelled = amtSentToGranter >= minValue && cancellable dat > 0 && not deadlineReached

    granterSigned :: Bool
    granterSigned = PSU.V2.txSignedBy info $ granter dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ PSU.V2.txInfoValidRange info

    vestFee :: Integer
    vestFee = amtSentTo pkh

    amtSentToBeneficiary :: Integer
    amtSentToBeneficiary = amtSentTo $ beneficiary dat

    amtSentToBeneficiaryScript :: Integer
    amtSentToBeneficiaryScript = PlutusV2.getLovelace (PlutusV2.fromValue (PSU.V2.valueLockedBy info $ beneficiaryScript dat))

    amtSentToGranter :: Integer
    amtSentToGranter = amtSentTo $ granter dat

    amtSentTo :: PubKeyHash -> Integer
    amtSentTo pkh' = PlutusV2.getLovelace (PlutusV2.fromValue (PSU.V2.valuePaidTo info pkh'))

    onlyOneInput :: Bool
    onlyOneInput = inputCounts == 1

    inputCounts :: Integer
    inputCounts = length $ PSU.V2.txInfoInputs info

    minValue :: Integer
    minValue = minVestValue dat


validator :: PubKeyHash -> PlutusV2.Validator
validator pkh = PlutusV2.mkValidatorScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    where
        wrap = PSU.V2.mkUntypedValidator . mkValidator
        


scriptSBSV2 :: PubKeyHash -> SBS.ShortByteString
scriptSBSV2 pkh = SBS.toShort . LBS.toStrict $ serialise $ validator pkh


serialisedScriptV2 :: PubKeyHash -> PlutusScript PlutusScriptV2
serialisedScriptV2 pkh = PlutusScriptSerialised $ scriptSBSV2 pkh

main :: IO ()
main = do
    args <- getArgs
    if null args
        then do
            putStrLn "Please provide a public key hash to which the fee will be sent!"
        else
            let pkh = fromHex . fromString . head $ args
            in case pkh of
                Left err -> putStrLn $ "Invalid public key hash: " ++ err
                Right (PlutusV2.LedgerBytes bytes) -> do
                    void $ writeFileTextEnvelope "vesting-plutusV2.plutus" Nothing $ serialisedScriptV2 $ PubKeyHash bytes