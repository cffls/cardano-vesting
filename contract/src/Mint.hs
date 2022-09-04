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

-- import Common (VestingDatum, beneficiary, deadline)


data VestingDatum = VestingDatum
    { beneficiary :: PaymentPubKeyHash
    , deadline    :: PlutusV2.POSIXTime
    }

PlutusTx.unstableMakeIsData ''VestingDatum


mintingFee :: Integer
mintingFee = 10000000

vpkh :: PlutusV2.ValidatorHash
vpkh = PlutusV2.ValidatorHash "174f1166deeee0844aed52352db46f222b7d3caacf07e49ab18bf869"

extractFiniteUpper :: POSIXTimeRange -> Maybe POSIXTime
extractFiniteUpper i = case ivTo i of
    UpperBound (Finite v) _ -> Just v
    _ -> Nothing

diffInDays :: Integer -> Integer -> Integer
diffInDays a b
    | b > a = divide (b - a) (24 * 60 * 60 * 1000)
    | otherwise = 0


{-# INLINABLE mkValidator #-}
mkValidator :: PubKeyHash -> BuiltinData -> PlutusV2.ScriptContext -> Bool
mkValidator pkh _ ctx =  traceIfFalse "invalid mint amount" checkNFTAmount &&
                         traceIfFalse "insufficient fee" paidFee
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    scriptOutputsAtVest :: PlutusV2.ValidatorHash -> PlutusV2.TxInfo -> [PlutusV2.TxOut]
    scriptOutputsAtVest pk p =
        let flt o@PlutusV2.TxOut{PlutusV2.txOutAddress =
            PlutusV2.Address (PlutusV2.ScriptCredential pk') _} | pk == pk' = Just o
            flt _                             = Nothing
        in mapMaybe flt (PlutusV2.txInfoOutputs p)

    getLockedDuration :: PlutusV2.TxOut -> Integer
    getLockedDuration o =
        case PlutusV2.txOutDatum o of
            PlutusV2.NoOutputDatum -> 0
            PlutusV2.OutputDatumHash _ -> 0
            PlutusV2.OutputDatum d -> case PlutusTx.fromBuiltinData $ PlutusV2.getDatum d of
                Just VestingDatum{deadline} ->
                    case extractFiniteUpper $ PlutusV2.txInfoValidRange info of
                        Just upper -> diffInDays (PlutusV2.getPOSIXTime deadline) (PlutusV2.getPOSIXTime upper)
                        Nothing -> 0
                Nothing -> 0

    getMaxMint :: [PlutusV2.TxOut] -> Integer
    getMaxMint outs = foldl (\acc o -> acc + getMaxMintFromOneOutput o) 0 outs
        where 
            getMaxMintFromOneOutput o = getLockedDuration o * getLovelaceAmount o
            getLovelaceAmount o = ADA.getLovelace (ADA.fromValue $ PlutusV2.txOutValue o )

    maxMint :: Integer
    maxMint = getMaxMint $ scriptOutputsAtVest vpkh info

    checkNFTAmount :: Bool
    checkNFTAmount = case Value.flattenValue (PlutusV2.txInfoMint info) of
       [(cs, tn', amt)] -> cs  == PSU.V2.ownCurrencySymbol ctx && tn' == PlutusV2.TokenName "" && amt <= maxMint
       _                -> False

    paidFee :: Bool
    paidFee = ADA.getLovelace (ADA.fromValue (PSU.V2.valuePaidTo info pkh)) >= mintingFee


policy :: PlutusV2.PubKeyHash -> PlutusV2.MintingPolicy
policy mp = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode mp
  where
    wrap mp' = PSU.V2.mkUntypedMintingPolicy $ mkValidator mp'



scriptSBSV2 :: PubKeyHash -> SBS.ShortByteString
scriptSBSV2 pkh = SBS.toShort . LBS.toStrict $ serialise $ policy pkh


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