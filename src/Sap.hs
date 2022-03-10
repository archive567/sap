{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE FlexibleInstances #-}

module Sap where

import Prelude hiding (lookup)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Aeson.Types
import Data.Aeson.KeyMap
import Data.Text (Text, unpack, pack)
import Control.Monad
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)
import Data.Bifunctor

sch :: IO (Maybe Value)
sch = decode <$> B.readFile "other/sap.json"

sch' :: IO (Maybe SapData)
sch' = decode <$> B.readFile "other/sap.json"

data SapData = SapData
  { foods :: [(Key, Food)],
    pets :: [(Key, Pet)],
    statuses :: [(Key, Status)],
    turns :: [(Key, Turn)]
  } deriving (Eq, Show, Generic)

instance FromJSON SapData where
  parseJSON =
    withObject "SapData"
      (\v ->
        SapData
        <$> kv' v "foods"
        <*> kv' v "pets"
        <*> kv' v "statuses"
        <*> kv' v "turns")

kv' :: (FromJSON a) => Object -> Key -> Parser [(Key, a)]
kv' v l =
  maybe
  (fail "label not found")
  (withObject "kv'"
   (\o -> pure $
     [(k,a)|
       (k,Right a) <-
         second (parseEither parseJSON) <$> toList o]))
  (lookup l v)

data Pack = StandardPack | ExpansionPack1 | EasterEgg deriving (Generic, Show, Eq)

instance FromJSON Pack where
  parseJSON =
    withText "Pack" $ \case
        "StandardPack" -> pure StandardPack
        "ExpansionPack1" -> pure ExpansionPack1
        "EasterEgg" -> pure EasterEgg
        _ -> fail "unknown Pack"

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

data TargetType = AdjacentAnimals | AdjacentFriends | All | DifferentTierAnimals | EachEnemy | EachFriend | EachShopAnimal | FirstEnemy | FriendAhead | FriendBehind | HighestHealthFriend | HighestHealthEnemy | LastEnemy | LeftMostFriend | Level2And3Friends | LowestHealthEnemy | Player | PurchaseTarget | RandomEnemy | RandomFriend | RightMostFriend | Self | StrongestFriend | TriggeringEntity deriving (Show, Read, Eq, Generic)

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
    withText "Team" $ \case
        "Friendly" -> pure Friendly
        "Enemy" -> pure Enemy
        _ -> fail "unknown Team"

type Level = Int

data Tier = TierN Int | TierSummoned deriving (Eq, Show, Generic)

instance FromJSON Tier where
  parseJSON (Number n) = pure $ TierN (round n)
  parseJSON (String s) = case s of
    "Summoned" -> pure TierSummoned
    _ -> fail "unknown Tier"
  parseJSON invalid    =
        prependFailure "parsing Amount failed, "
            (typeMismatch "Object" invalid)

data Attack = Attack Int | AttackQuestion deriving (Show, Eq, Generic)

instance FromJSON Attack where
  parseJSON (Number n) = pure $ Attack (round n)
  parseJSON (String "?") = pure AttackQuestion
  parseJSON invalid    =
        prependFailure "parsing Attack failed, "
            (typeMismatch "Object" invalid)

data Health = Health Int | HealthQuestion deriving (Show, Eq, Generic)

instance FromJSON Health where
  parseJSON (Number n) = pure $ Health (round n)
  parseJSON (String "?") = pure HealthQuestion
  parseJSON invalid    =
        prependFailure "parsing Health failed, "
            (typeMismatch "Object" invalid)

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
      (\case
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

newtype Emoji = Emoji { char :: String } deriving (Show, Generic, Eq)

instance FromJSON Emoji where
    parseJSON =
      withObject "Emoji"
      (\v -> Emoji <$> v .: "unicodeCodePoint")

newtype Name = Name { label :: Text } deriving (Show, Generic, Eq)

instance FromJSON Name

data Trigger = AfterAttack | BeforeAttack | Buy | BuyAfterLoss | BuyFood | BuyTier1Animal | CastsAbility | EatsShopFood | EndOfTurn | EndOfTurnWith3PlusGold | EndOfTurnWith4OrLessAnimals | EndOfTurnWithLvl3Friend | Faint | Hurt | KnockOut | LevelUp | Sell | StartOfBattle | StartOfTurn | Summoned | WhenAttacking | WhenDamaged deriving (Show, Generic, Eq, Read)

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
  } deriving (Eq, Show, Generic)

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

data Probability = Probability
  {
    probKind :: ProbKind,
    turn :: Text,
    perShop :: Maybe PerShop,
    perSlot :: PerShop
  } deriving (Eq, Show, Generic)

instance FromJSON Probability where
  parseJSON =
    withObject "Probability"
      (\v ->
        Probability
        <$> v .: "kind"
        <*> v .: "turn"
        <*> v .:? "perShop"
        <*> v .: "perSlot")

data ProbKind = ProbShop | ProbLevelUp deriving (Eq, Show, Generic)

instance FromJSON ProbKind where
  parseJSON =
    withText "ProbKind" $ \case
        "shop" -> pure ProbShop
        "levelup" -> pure ProbLevelUp
        _ -> fail "unknown ProbKind"

data PerShop = PerShop { standardPack :: Maybe Double, expansionPack1 :: Maybe Double } deriving (Eq, Show, Generic)

instance FromJSON PerShop where
  parseJSON =
    withObject "PerShop"
      (\v ->
        PerShop
        <$> v .:? "StandardPack"
        <*> v .:? "ExpansionPack1")

-- TODO:
data Pet = Pet
  {
    -- | The name of the pet.
    petName :: Text,
    petEmoji :: Emoji,
    -- | The tier the pet appears in.
    tier :: Tier,
    -- | The standard starting attack points for the pet.
    baseAttack :: Attack,
    -- | The standard starting health points for the pet.
    baseHealth :: Health,
    -- | Which packs the pet appears in.
    packs :: [Pack],
    -- | The ability the pet has at level 1.
    level1Ability :: Maybe Ability,
    -- | The ability the pet has at level 2.
    level2Ability :: Maybe Ability,
    -- | The ability the pet has at level 3.
    level3Ability :: Maybe Ability,
    petProbabilities :: Maybe [Probability],
    petStatus :: Maybe Status,
    petNotes :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON Pet where
  parseJSON =
    withObject "Pet"
      (\v ->
        Pet
        <$> v .: "name"
        <*> v .: "image"
        <*> v .: "tier"
        <*> v .: "baseAttack"
        <*> v .: "baseHealth"
        <*> v .: "packs"
        <*> v .:? "level1Ability"
        <*> v .:? "level2Ability"
        <*> v .:? "level3Ability"
        <*> v .:? "probabilities"
        <*> v .:? "status"
        <*> v .:? "notes")

data Food = Food
  {
    foodName :: Text,
    foodEmoji :: Emoji,
    foodCost :: Maybe Int,
    foodTier :: Tier,
    foodPacks :: [Pack],
    foodProbabilities :: Maybe [Probability],
    foodNotes :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON Food where
  parseJSON =
    withObject "Food"
      (\v ->
        Food
        <$> v .: "name"
        <*> v .: "image"
        <*> v .:? "cost"
        <*> v .: "tier"
        <*> v .: "packs"
        <*> v .:? "probabilities"
        <*> v .:? "notes")
