import Data.Bool

import Ledger (Address, POSIXTime, POSIXTimeRange, PaymentPubKeyHash (unPaymentPubKeyHash), Validator, PubKeyHash, ValidatorHash, pubKeyHashAddress, scriptAddress)
import Ledger.Contexts (ScriptContext (..), TxInfo (..))
import Ledger.Contexts (txSignedBy)
import Ledger.Interval (after)
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Tx (txOutValue, txOutAddress)
import Ledger.Typed.Scripts (ValidatorTypes (..), TypedValidator, mkTypedValidator, wrapValidator, validatorScript, validatorHash)
import Ledger.Value (Value, leq)
import Playground.Contract

import PlutusTx
import PlutusTx.Prelude as Ptx

data Campaign = Campaign { campaignTimePeriod :: POSIXTimeRange } deriving Show

PlutusTx.makeLift ''Campaign

{-# INLINABLE mkValidator #-}
mkValidator :: Campaign -> ThriftDatum -> ThriftRedeemer -> ScriptContext -> Bool
mkValidator cmp dat red ctx = 
    -- Check that campaign has concluded before user is able to withdraw
    traceIfFalse "Campaign not yet concluded" txInfoValidRange ctx `after` campaignTimePeriod cmp Ptx.&&
    -- Check if withdrawal request has been signed by the address which made the initial deposit
    traceIfFalse "Withdrawal transaction not signed by deposit address" scriptContextTxInfo ctx `txSignedBy` dat Ptx.&&
    -- Check that user has deposited enough value to withdraw the requested value of tokens
    traceIfFalse "User has not deposited enough to withdraw this amount" red `leq` sum $ map txOutValue $ filter ((== pubKeyHashAddress dat Nothing) . txOutAddress) $ txInfoOutputs ctx

type ThriftDatum = PubKeyHash
type ThriftRedeemer = Value

data Thrift
instance ValidatorTypes Thrift where
    type instance DatumType Thrift = ThriftDatum
    type instance RedeemerType Thrift = ThriftRedeemer

typedValidator :: Campaign -> TypedValidator Thrift
typedValidator cmp = mkTypedValidator @Thrift
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode cmp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = wrapValidator @ThriftDatum @ThriftRedeemer

validator :: Campaign -> Validator
validator = validatorScript . typedValidator

valHash :: Campaign -> ValidatorHash
valHash = validatorHash . typedValidator

scrAddress :: Campaign -> Address
scrAddress = scriptAddress . validator
