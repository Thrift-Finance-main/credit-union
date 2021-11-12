{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE NamedFieldPuns        #-}

module ProjectSahara where

import           Control.Applicative         (Applicative (pure))
import           Control.Monad               (void)
import           Ledger                      (POSIXTime, POSIXTimeRange, PubKeyHash, ScriptContext (..), TxInfo (..),
                                              Validator, pubKeyHash, txId)
import System.Random
import System.Random.Shuffle
import qualified Ledger
import qualified Ledger.Contexts             as V
import qualified Ledger.Interval             as Interval
import qualified Ledger.Scripts              as Scripts
import qualified Ledger.TimeSlot             as TimeSlot
import qualified Ledger.Typed.Scripts        as Scripts hiding (validatorHash)
import           Ledger.Value                (Value)
import           Playground.Contract
import           Plutus.Contract
import qualified Plutus.Contract.Constraints as Constraints
import qualified Plutus.Contract.Typed.Tx    as Typed
import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Applicative (..), Semigroup (..))
import           Prelude                     (Semigroup (..))
import qualified Prelude                     as Haskell
import qualified Wallet.Emulator             as Emulator
import qualified Ledger.Ada                  as Ada

data Campaign = Campaign
    { campaignStartDate          :: POSIXTime
    -- ^ The date by which the campaign funds can be contributed.
    , campaignEndDate            :: POSIXTime
    -- ^ The date by which the campaign owner has to collect the funds
    , campaignOwner              :: PubKeyHash
    , stakeWalletAddress         :: PubKeyHash
    , maxStakers                 :: Integer
    , minStakers                 :: Integer
    , minStake                   :: Value
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Campaign

-- | Action that can be taken by the participants in this contract. A value of
--   `CampaignAction` is provided as the redeemer. The validator script then
--   checks if the conditions for performing this action are met.
data CampaignAction = Stake | Refund

PlutusTx.unstableMakeIsData ''CampaignAction
PlutusTx.makeLift ''CampaignAction

type CrowdfundingSchema =
        Endpoint "schedule collection" ()
        .\/ Endpoint "contribute" Contribution

newtype Contribution =
  Contribution
    { contribValue :: Value
     -- ^ how much to contribute
    } deriving stock (Haskell.Eq, Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema, ToArgument)

-- | Construct a 'Campaign' value from the campaign parameters,
--   using the wallet's public key.
mkCampaign :: [Haskell.Int] -> POSIXTime -> POSIXTime -> Wallet ->  Campaign
mkCampaign payorder startDate collectionDdl ownerWallet =
    let
      campaignLengthInDays  = 60
      lengthOfCampaignInMillis = Ledger.POSIXTime (campaignLengthInDays * 24 * 60 * 60 * 1000)
    in  Campaign
        { campaignStartDate = startDate
        , campaignEndDate =  startDate + lengthOfCampaignInMillis
        , stakeWalletAddress = pubKeyHash $ Emulator.walletPubKey ownerWallet
        , maxStakers = 4
        , campaignOwner = pubKeyHash $ Emulator.walletPubKey ownerWallet
        , minStakers  = 3
        , minStake  = (Ada.lovelaceValueOf 1)
        }

-- | The 'POSIXTimeRange' during which the funds can be collected
collectionRange :: Campaign -> POSIXTimeRange
collectionRange cmp =
    Interval.interval (campaignStartDate cmp) (campaignEndDate cmp)

-- | The 'POSIXTimeRange' during which a refund may be claimed
refundRange :: Campaign -> POSIXTimeRange
refundRange cmp =
    Interval.from (campaignEndDate cmp)

data Crowdfunding
instance Scripts.ValidatorTypes Crowdfunding where
    type instance RedeemerType Crowdfunding = CampaignAction
    type instance DatumType Crowdfunding = PubKeyHash

typedValidator :: Campaign -> Scripts.TypedValidator Crowdfunding
typedValidator = Scripts.mkTypedValidatorParam @Crowdfunding
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

{-# INLINABLE okToRefund #-}
okToRefund :: Campaign -> PubKeyHash -> TxInfo -> Bool
okToRefund campaign contributor txinfo =
    -- Check that the campaign starts after the refund, i.e. the campaign hasnt started yet
    ((campaignStartDate campaign) `Interval.after` txInfoValidRange txinfo)
    -- Check that the transaction is signed by the contributor
    && (txinfo `V.txSignedBy` contributor)

okToStake:: Campaign -> TxInfo -> Bool
okToStake campaign txinfo =
    -- Check that the campaign starts  after this transaction
    (campaignStartDate campaign) `Interval.after` txInfoValidRange txinfo
    &&((length (txInfoSignatories txinfo)) < (maxStakers campaign))
    -- Check that there are less than Max Stakers in the Stake Crowd
    -- Check that this transaction is NOT signed by the campaign owner, the campaign owner cannot stake!
    && not (txinfo `V.txSignedBy` campaignOwner campaign)

-- | The validator script is of type 'CrowdfundingValidator', and is
-- additionally parameterized by a 'Campaign' definition. This argument is
-- provided by the Plutus client, using 'PlutusTx.applyCode'.
-- As a result, the 'Campaign' definition is part of the script address,
-- and different campaigns have different addresses.
mkValidator :: Campaign -> PubKeyHash -> CampaignAction -> ScriptContext -> Bool
mkValidator c con (Refund) p  = okToRefund c con (scriptContextTxInfo p)
mkValidator c con Stake p = okToStake c (scriptContextTxInfo p)

-- | The validator script that determines whether the campaign owner can
--   retrieve the funds or the contributors can claim a refund.
contributionScript :: Campaign -> Validator
contributionScript = Scripts.validatorScript . typedValidator

-- | The address of a [[Campaign]]
campaignAddress :: Campaign -> Ledger.ValidatorHash
campaignAddress = Scripts.validatorHash . contributionScript

-- | The crowdfunding contract for the 'Campaign'.
crowdfunding :: AsContractError e => Campaign -> Contract () CrowdfundingSchema e ()
crowdfunding c = contribute c `select` scheduleCollection c

-- | A sample campaign
theCampaign :: Campaign
theCampaign =
  Campaign
    { campaignStartDate          = TimeSlot.slotToPOSIXTime 40
       -- ^ The date by which the campaign funds can be contributed.
    , campaignEndDate            = TimeSlot.slotToPOSIXTime 60
    -- ^ The date by which the campaign owner has to collect the funds
    , campaignOwner              = pubKeyHash $ Emulator.walletPubKey (Emulator.Wallet 1)
    , stakeWalletAddress         = pubKeyHash $ Emulator.walletPubKey (Emulator.Wallet 1)
    , maxStakers                 = 4
    , minStakers                 = 3
    , minStake                   = Ada.lovelaceValueOf 1
    }

-- | The "contribute" branch of the contract for a specific 'Campaign'. Exposes
--   an endpoint that allows the user to enter their public key and the
--   contribution. Then waits until the campaign is over, and collects the
--   refund if the funding was not collected.
contribute :: AsContractError e => Campaign -> Contract () CrowdfundingSchema e ()
contribute cmp = do
    Contribution{contribValue} <- endpoint @"contribute"
    contributor <- pubKeyHash <$> ownPubKey
    let inst = typedValidator cmp
        tx = Constraints.mustPayToTheScript contributor contribValue
                <> Constraints.mustValidateIn (Ledger.interval 1 (campaignEndDate cmp))
    txid <- fmap txId (submitTxConstraints inst tx)

    utxo <- watchAddressUntilTime (Scripts.validatorAddress inst) (campaignEndDate cmp)

    -- 'utxo' is the set of unspent outputs at the campaign address at the
    -- collection deadline. If 'utxo' still contains our own contribution
    -- then we can claim a refund.

    let flt Ledger.TxOutRef{txOutRefId} _ = txid Haskell.== txOutRefId
        tx' = Typed.collectFromScriptFilter flt utxo Refund
                <> Constraints.mustValidateIn (refundRange cmp)
                <> Constraints.mustBeSignedBy contributor
    if Constraints.modifiesUtxoSet tx'
    then void (submitTxConstraintsSpending inst utxo tx')
    else pure ()

-- | The campaign owner's branch of the contract for a given 'Campaign'. It
--   watches the campaign address for contributions and collects them if
--   the funding goal was reached in time.
scheduleCollection :: AsContractError e => Campaign -> Contract () CrowdfundingSchema e ()
scheduleCollection cmp = do
    let inst = typedValidator cmp

    -- Expose an endpoint that lets the user fire the starting gun on the
    -- campaign. (This endpoint isn't technically necessary, we could just
    -- run the 'trg' action right away)
    () <- endpoint @"schedule collection"

    _ <- awaitTime $ campaignStartDate cmp
    unspentOutputs <- utxoAt (Scripts.validatorAddress inst)

    let tx = Typed.collectFromScript unspentOutputs Stake
            <> Constraints.mustValidateIn (collectionRange cmp)
    void $ submitTxConstraintsSpending inst unspentOutputs tx

{- note [Transactions in the crowdfunding campaign]

Assume there is a campaign `c :: Campaign` with two contributors
(identified by public key `pc_1` and `pc_2`) and one campaign owner (pco).
Each contributor creates a transaction, `t_1` and `t_2`, whose outputs are
locked by the scripts `contributionScript c pc_1` and `contributionScript
c pc_1` respectively.

There are two outcomes for the campaign.

1. Campaign owner collects the funds from both contributors. In this case
   the owner creates a single transaction with two inputs, referring to
   `t_1` and `t_2`. Each input contains the script `contributionScript c`
   specialised to a contributor. The redeemer script of this transaction
   contains the value `Collect`, prompting the validator script to check the
   branch for `Collect`.

2. Refund. In this case each contributor creates a transaction with a
   single input claiming back their part of the funds. This case is
   covered by the `Refund` branch, and its redeemer script is the
   `Refund` action.

In both cases, the validator script is run twice. In the first case
there is a single transaction consuming both inputs. In the second case there
are two different transactions that may happen at different times.

-}

{- note [PendingTx]

This part of the API (the PendingTx argument) is experimental and subject
to change.

-}

endpoints :: AsContractError e => Contract () CrowdfundingSchema e ()
endpoints = crowdfunding theCampaign

mkSchemaDefinitions ''CrowdfundingSchema

$(mkKnownCurrencies [])

main :: IO ()
main = return ()
