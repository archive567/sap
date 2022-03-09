{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Sap where

import Prelude hiding (lookup)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Aeson.Types
import Data.Aeson.KeyMap
import Data.Text (Text, unpack)
import Control.Monad
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)

sch :: IO (Maybe Value)
sch = decode <$> B.readFile "other/sap.json"

data Pack = StandardPack | ExpansionPack deriving (Generic, Show, Eq)

data Turn = Turn
  {
    animalShopSlots :: Int,
    foodShopSlots :: Int,
    turnId :: Text,
    index :: Int,
    levelUpTier :: Int,
    livesLost :: Int,
    turnName :: Text,
    tiersAvailable :: Int
  } deriving (Show, Generic, Eq)

instance FromJSON Turn where
  parseJSON =
    withObject "Turn"
      (\v ->
        Turn
        <$> v .: "animalShopSlots"
        <*> v .: "foodShopSlots"
        <*> v .: "id"
        <*> v .: "index"
        <*> v .: "levelUpTier"
        <*> v .: "livesLost"
        <*> v .: "name"
        <*> v .: "tiersAvailable")

data Amount = Amount Int | AmountPercent Int | AmountQuestion deriving (Show, Eq, Generic)

instance FromJSON Amount where
  parseJSON (Number n) = pure $ Amount (round n)
  parseJSON (Object o) = AmountPercent <$> o .: "attackDamagePercent"
  parseJSON (String "?") = pure AmountQuestion
  parseJSON invalid    =
        prependFailure "parsing Amount failed, "
            (typeMismatch "Object" invalid)

data TargetType = AdjacentAnimals | AdjacentFriends | All | DifferentTierAnimals | EachEnemy | EachFriend | EachShopAnimal | FirstEnemy | FriendAhead | FriendBehind | HighestHealthFriend | HighestHealthEnemy | LastEnemy | LeftMostFriend | Level2And3Friends | LowestHealthEnemy | PurchaseTarget | RandomEnemy | RandomFriend | RightMostFriend | Self | StrongestFriend | TriggeringEntity deriving (Show, Read, Eq, Generic)

instance FromJSON TargetType where
  parseJSON =
    withText "TargetType" $ \t -> case readMaybe (unpack t) of
        Just x -> pure x
        Nothing -> fail "unknown Target"

data Target = Target { targetType :: TargetType, targetN :: Maybe Int, includingFutures :: Maybe Bool } deriving (Show, Eq, Generic)

instance FromJSON Target where
  parseJSON =
    withObject "Target"
      (\v ->
        Target
        <$> v .: "kind"
        <*> v .:? "n"
        <*> v .:? "includingFutures")

data Team = Friendly | Enemy deriving (Show, Eq, Generic)

instance FromJSON Team where
  parseJSON =
    withText "inner Team" $ \case
        "Friendly" -> pure Friendly
        "Enemy" -> pure Enemy
        _ -> fail "unknown Team"

type Level = Int
type Tier = Int
type Attack = Int
type Health = Int
type Status = Text

newtype DamageModifier = DamageModifier { modifier :: Maybe Int } deriving (Show, Eq, Generic)

instance FromJSON DamageModifier where
  parseJSON (Number n) = pure $ DamageModifier (Just (round n))
  parseJSON Null = pure $ DamageModifier Nothing
  parseJSON invalid =
        prependFailure "parsing DamageModifier failed, "
            (typeMismatch "Object" invalid)

data Effect =
  AllOf [Effect] |
  ApplyStatus Status Target |
  DealDamage Amount Target |
  DiscountFood Amount |
  Evolve Text |
  FaintEffect Target |
  FoodMultiplier Amount |
  GainAbility Target |
  GainExperience Target Amount |
  GainGold Amount |
  ModifyDamage Bool DamageModifier |
  ModifyStats Bool (Maybe Amount) (Maybe Amount) Target |
  OneOf [Effect] |
  ReduceHealth Amount Target |
  RefillShops Text Text |
  RepeatAbility Target Level |
  RespawnPet Attack Health |
  SplashDamage Amount |
  SummonPet Text Team (Maybe Attack) (Maybe Health) |
  SummonRandomPet Level Tier (Maybe Attack) (Maybe Health) |
  Swallow Target |
  TransferAbility Level Target Target |
  TransferStats Bool Bool Target Target (Maybe Amount)
  deriving (Show, Eq, Generic)

instance FromJSON Effect where
  parseJSON = withObject "Effect" $ \o -> do
    e <- o .: "kind"
    withText "kind of Effect"
      (\s -> case s of
          "AllOf" -> AllOf <$> o .: "effects"
          "ApplyStatus" -> ApplyStatus <$> o .: "status" <*> o .: "to"
          "DealDamage" -> DealDamage <$> o .: "amount" <*> o .: "target"
          "DiscountFood" -> DiscountFood <$> o .: "amount"
          "Evolve" -> Evolve <$> o .: "into"
          "Faint" -> FaintEffect <$> o .: "target"
          "FoodMultiplier" -> FoodMultiplier <$> o .: "amount"
          "GainAbility" -> GainAbility <$> o .: "target"
          "GainExperience" -> GainExperience <$> o .: "target" <*> o .: "amount"
          "GainGold" -> GainGold <$> o .: "amount"
          "ModifyDamage" -> ModifyDamage <$> o .: "appliesOnce" <*> o .: "damageModifier"
          "ModifyStats" ->
            ModifyStats <$>
            o .: "untilEndOfBattle" <*>
            o .:? "attackAmount" <*>
            o .:? "healthAmount" <*>
            o .: "target"
          "OneOf" -> OneOf <$> o .: "effects"
          "ReduceHealth" -> ReduceHealth <$> o .: "percentage" <*> o .: "target"
          "RefillShops" -> RefillShops <$>  o .: "food" <*> o .: "shop"
          "RepeatAbility" -> RepeatAbility <$> o .: "target" <*> o .: "level"
          "RespawnPet" -> RespawnPet <$> o .: "baseAttack" <*> o .: "baseHealth"
          "SplashDamage" -> SplashDamage <$> o .:"amount"
          "SummonPet" ->
            SummonPet <$>  o .:"pet" <*> o .: "team" <*> o .:? "withAttack" <*> o .:? "withHealth"
          "SummonRandomPet" ->
            SummonRandomPet <$>
            o .: "level" <*>
            o .: "tier" <*>
            o .:? "baseAttack" <*>
            o .:? "baseHealth"
          "Swallow" -> Swallow <$>  o .: "target"
          "TransferAbility" ->
            TransferAbility <$> o .: "level" <*> o .: "from" <*> o .: "to"
          "TransferStats" ->
            TransferStats <$>
            o .: "copyAttack" <*>
            o .: "copyHealth" <*>
            o .: "from" <*>
            o .: "to" <*>
            o .:? "percentage"
          x -> fail ("unknown effect kind key:" <> show x)) e

newtype Emoji = Emoji { char :: Char } deriving (Show, Generic, Eq)

instance FromJSON Emoji where
    parseJSON =
      join .
      withObject "outer emoji"
      (\v -> withObject "inner emoji"
       (\v' -> Emoji <$> v' .: "unicodeCodePoint") <$>
       (v .: "image"))

newtype Name = Name { label :: Text } deriving (Show, Generic, Eq)

instance FromJSON Name where
    parseJSON = withObject "name" (.: "name")

data Trigger = AfterAttack | BeforeAttack | Buy | BuyAfterLoss | BuyFood | BuyTier1Animal | CastsAbility | EatsShopFood | EndOfTurn | EndOfTurnWith3PlusGold | EndOfTurnWith4OrLessAnimals | EndOfTurnWithLvl3Friend | Faint | Hurt | KnockOut | LevelUp | Sell | StartOfBattle | StartOfTurn | Summoned deriving (Show, Generic, Eq, Read)

instance FromJSON Trigger where
  parseJSON =
    withText "Trigger" $ \t -> case readMaybe (unpack t) of
        Just x -> pure x
        Nothing -> fail "unknown Trigger"

data Ability = Ability
  {
    -- | The text description of the ability.
    description :: Text,
    -- | What behaviour (by the trigger entity) will initiate the ability.
    trigger :: Trigger,
    -- | Which entity will trigger the effect.
    triggeredBy :: Target,
    -- | What the effect does.
    effect :: Effect,
    untilEndOfBattle :: Maybe Bool
  }

instance FromJSON Ability where
  parseJSON =
    withObject "Ability"
      (\v ->
        Ability
        <$> v .: "description"
        <*> v .: "trigger"
        <*> v .: "triggeredBy"
        <*> v .: "effect"
        <*> v .:? "untilEndOfBattle")

{-
data Probability = Probability
  {
    probKind :: ProbKind,
    turn :: Turn,
    perShop :: PerShop,
    perSlot :: PerShop
  } deriving (Eq, Show, Generic)

instance FromJSON Probability

data ProbKind deriving (Eq, Show, Generic)

data PerShop = PerShop { standardPack :: Double, expansionPack1 :: Double } deriving (Eq, Show, Generic)

-}

-- TODO:
data Pet = Pet
  {
    -- | The name of the pet.
    petName :: Name,
    -- | The tier the pet appears in.
    tier :: Int,
    -- | The standard starting attack points for the pet.
    baseAttack :: Int,
    -- | The standard starting health points for the pet.
    baseHealth :: Int,
    -- | Which packs the pet appears in.
    packs :: [Pack],
    -- | The ability the pet has at level 1.
    level1Ability :: Ability,
    -- | The ability the pet has at level 2.
    level2Ability :: Ability,
    -- | The ability the pet has at level 3.
    level3Ability :: Ability
  }

data Food
