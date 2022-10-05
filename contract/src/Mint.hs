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
import           Ledger.Value                         as Value

import qualified Plutus.Script.Utils.V2.Scripts as PSU.V2
import qualified Plutus.V2.Ledger.Contexts as PSU.V2

import qualified Plutus.V2.Ledger.Api           as PlutusV2

import           Prelude              (IO, putStrLn)
import qualified Ledger.Ada as ADA
import Plutus.V1.Ledger.Bytes (fromHex)

import Common (VestingDatum(..))


data ContractParam = ContractParam
    { vestPKH   :: PlutusV2.ValidatorHash
    , ownerPKH  :: PlutusV2.PubKeyHash
    , mintFee   :: Integer
    }

PlutusTx.makeLift ''ContractParam
PlutusTx.unstableMakeIsData ''ContractParam

{-# INLINABLE mkValidator #-}
mkValidator :: ContractParam -> BuiltinData -> PlutusV2.ScriptContext -> Bool
mkValidator cp _ ctx =  traceIfFalse "insufficient fee" paidFee &&
                        traceIfFalse "invalid mint amount" checkMintAmount &&
                        traceIfFalse "not signed by token issuer" signedByOwner
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    signedByOwner :: Bool
    signedByOwner = PSU.V2.txSignedBy info $ ownerPKH cp

    extractFiniteUpper :: POSIXTimeRange -> Maybe POSIXTime
    extractFiniteUpper i = case ivTo i of
        UpperBound (Finite v) _ -> Just v
        _ -> Nothing

    diffInDays :: Integer -> Integer -> Integer
    diffInDays a b
        | b > a = divide (b - a) (24 * 60 * 60 * 1000)
        | otherwise = 0

    capDays :: Integer -> Integer
    capDays d
        | d > 3650 = 3650
        | otherwise = d

    getLockedDuration :: PlutusV2.OutputDatum -> Integer
    getLockedDuration d =
        case d of
            PlutusV2.NoOutputDatum -> 0
            PlutusV2.OutputDatumHash _ -> 0
            PlutusV2.OutputDatum d' -> case PlutusTx.fromBuiltinData $ PlutusV2.getDatum d' of
                Just VestingDatum {deadline, cancellable} ->
                    case extractFiniteUpper $ PlutusV2.txInfoValidRange info of
                        Just upper -> setToZeroIfCancellable cancellable $ capDays $ diffInDays (PlutusV2.getPOSIXTime upper) (PlutusV2.getPOSIXTime deadline)
                        Nothing -> 0
                Nothing -> 0

                where setToZeroIfCancellable :: Integer -> Integer -> Integer
                      setToZeroIfCancellable cancellable' days
                          | cancellable' > 0 = 0
                          | otherwise = days

    getMaxMint :: [(PlutusV2.OutputDatum, PlutusV2.Value)] -> Integer
    getMaxMint outs = foldl (\acc (d, v) -> acc + getMaxMintFromOneOutput d v) 0 outs
        where
            getMaxMintFromOneOutput d v = getLockedDuration d * getLovelaceAmount v
            getLovelaceAmount v = ADA.getLovelace (ADA.fromValue v)

    maxMint :: Integer
    maxMint = getMaxMint $ PSU.V2.scriptOutputsAt (vestPKH cp) info

    checkMintAmount :: Bool
    checkMintAmount = case Value.flattenValue (PlutusV2.txInfoMint info) of
       [(cs, tn', amt)] -> cs  == PSU.V2.ownCurrencySymbol ctx && tn' == PlutusV2.TokenName "LOCK" && amt <= maxMint
       _                -> False

    paidFee :: Bool
    paidFee = ADA.getLovelace (ADA.fromValue (PSU.V2.valuePaidTo info $ ownerPKH cp)) >= mintFee cp


policy :: ContractParam -> PlutusV2.MintingPolicy
policy mp = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode mp
  where
    wrap mp' = PSU.V2.mkUntypedMintingPolicy $ mkValidator mp'

script :: PubKeyHash -> PlutusV2.Script
script pkh = PlutusV2.unMintingPolicyScript $ policy ContractParam
    { vestPKH = "8768a8ce0ef6d409f910f6c792af2284a670d4d393fc60fa5920a59e"
    , ownerPKH  = pkh
    , mintFee = 8000000
    }

scriptSBSV2 :: PubKeyHash -> SBS.ShortByteString
scriptSBSV2 pkh = SBS.toShort . LBS.toStrict $ serialise $ script pkh

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
                    void $ writeFileTextEnvelope "mint-plutusV2.plutus" Nothing $ serialisedScriptV2 $ PubKeyHash bytes