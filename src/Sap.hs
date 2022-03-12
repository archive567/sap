{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Sap where

import Control.Monad
import Control.Monad.State.Lazy
import Data.Aeson
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A
import Data.Aeson.Types
import Data.Bifunctor
import Data.Bool
import qualified Data.ByteString.Lazy as B
import Data.Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text, unpack)
import qualified Data.Vector as V
import GHC.Generics
import Optics.Core
import Optics.Zoom
import System.Random
import Text.Read (readMaybe)
import Prelude

type TurnKey = Key
type PetKey = Key
type FoodKey = Key
type StatusKey = Key

mkValue :: IO (Maybe Value)
mkValue = decode <$> B.readFile "other/sap.json"

data SapState = SapState
  { foods :: Map.Map FoodKey Food,
    pets :: Map.Map PetKey Pet,
    statuses :: Map.Map StatusKey Status,
    turns :: Map.Map TurnKey Turn,
    gen :: StdGen,
    pack :: Pack
  }
  deriving (Show, Generic, Eq)

sapState :: Pack -> IO SapState
sapState p = do
  s <- either fail pure =<< mkSapData
  g <- initStdGen
  pure $ SapState (foods_ s) (pets_ s) (statuses_ s) (turns_ s) g p

mkSapData :: IO (Either String SapData)
mkSapData = eitherDecode <$> B.readFile "other/sap.json"

data SapData = SapData
  { foods_ :: Map.Map FoodKey Food,
    pets_ :: Map.Map PetKey Pet,
    statuses_ :: Map.Map StatusKey Status,
    turns_ :: Map.Map TurnKey Turn
  }
  deriving (Eq, Show, Generic)

instance FromJSON SapData where
  parseJSON =
    withObject
      "SapData"
      ( \v ->
          SapData
            <$> kvParser v "foods"
            <*> kvParser v "pets"
            <*> kvParser v "statuses"
            <*> kvParser v "turns"
      )

-- | key-value parser.
kvParser :: (FromJSON a) => Object -> Key -> Parser (Map.Map Key a)
kvParser v l =
  maybe
    (fail "label not found")
    (withObject "kv" (innerKV 2))
    (A.lookup l v)

innerKV :: FromJSON b => Int -> Object -> Parser (Map.Map Key b)
innerKV n o =
  bool
    (fail ("innerKV: " <> show (take n errs)))
    (pure x)
    (Prelude.null errs)
  where
    res = second (parseEither parseJSON) <$> A.toList o
    errs = [(k, e) | (k, Left e) <- res]
    x = Map.fromList [(k, e) | (k, Right e) <- res]

data Pack = StandardPack | ExpansionPack1 | EasterEgg deriving (Generic, Show, Eq)

instance FromJSON Pack where
  parseJSON =
    withText "Pack" $ \case
      "StandardPack" -> pure StandardPack
      "ExpansionPack1" -> pure ExpansionPack1
      "EasterEgg" -> pure EasterEgg
      _ -> fail "unknown Pack"

data Turn = Turn
  { animalShopSlots :: Int,
    foodShopSlots :: Int,
    turnId :: Text,
    index :: Int,
    levelUpTier :: Int,
    livesLost :: Int,
    turnName :: Text,
    tiersAvailable :: Int
  }
  deriving (Show, Generic, Eq)

instance FromJSON Turn where
  parseJSON =
    withObject
      "Turn"
      ( \v ->
          Turn
            <$> v .: "animalShopSlots"
            <*> v .: "foodShopSlots"
            <*> v .: "id"
            <*> v .: "index"
            <*> v .: "levelUpTier"
            <*> v .: "livesLost"
            <*> v .: "name"
            <*> v .: "tiersAvailable"
      )

data Amount = Amount Int | AmountPercent Int | AmountQuestion deriving (Show, Eq, Generic)

instance FromJSON Amount where
  parseJSON (Number n) = pure $ Amount (round n)
  parseJSON (Object o) = AmountPercent <$> o .: "attackDamagePercent"
  parseJSON (String "?") = pure AmountQuestion
  parseJSON invalid =
    prependFailure
      "parsing Amount failed, "
      (typeMismatch "Object" invalid)

data TargetType = AdjacentAnimals | AdjacentFriends | All | DifferentTierAnimals | EachEnemy | EachFriend | EachShopAnimal | FirstEnemy | FriendAhead | FriendBehind | HighestHealthFriend | HighestHealthEnemy | LastEnemy | LeftMostFriend | Level2And3Friends | LowestHealthEnemy | Player | PurchaseTarget | RandomEnemy | RandomFriend | RightMostFriend | Self | StrongestFriend | TriggeringEntity deriving (Show, Read, Eq, Generic)

instance FromJSON TargetType where
  parseJSON =
    withText "TargetType" $ \t -> case readMaybe (unpack t) of
      Just x -> pure x
      Nothing -> fail "unknown Target"

data Target = Target {targetType :: TargetType, targetN :: Maybe Int, includingFutures :: Maybe Bool} deriving (Show, Eq, Generic)

instance FromJSON Target where
  parseJSON =
    withObject
      "Target"
      ( \v ->
          Target
            <$> v .: "kind"
            <*> v .:? "n"
            <*> v .:? "includingFutures"
      )

data Team = Friendly | Enemy deriving (Show, Eq, Generic)

instance FromJSON Team where
  parseJSON =
    withText "Team" $ \case
      "Friendly" -> pure Friendly
      "Enemy" -> pure Enemy
      _ -> fail "unknown Team"

type Level = Int

data Tier = TierN Int | TierSummoned deriving (Eq, Show, Generic)

type TierNumber = Int

instance FromJSON Tier where
  parseJSON (Number n) = pure $ TierN (round n)
  parseJSON (String s) = case s of
    "Summoned" -> pure TierSummoned
    _ -> fail "unknown Tier"
  parseJSON invalid =
    prependFailure
      "parsing Amount failed, "
      (typeMismatch "Object" invalid)

data Attack = Attack Int | AttackQuestion deriving (Show, Eq, Generic)

fromAttack :: Attack -> Int
fromAttack (Attack n) = n
fromAttack AttackQuestion = error "AttackQuestion"

instance FromJSON Attack where
  parseJSON (Number n) = pure $ Attack (round n)
  parseJSON (String "?") = pure AttackQuestion
  parseJSON invalid =
    prependFailure
      "parsing Attack failed, "
      (typeMismatch "Object" invalid)

data Health = Health Int | HealthQuestion deriving (Show, Eq, Generic)

fromHealth :: Health -> Int
fromHealth (Health n) = n
fromHealth HealthQuestion = error "HealthQuestion"

instance FromJSON Health where
  parseJSON (Number n) = pure $ Health (round n)
  parseJSON (String "?") = pure HealthQuestion
  parseJSON invalid =
    prependFailure
      "parsing Health failed, "
      (typeMismatch "Object" invalid)

newtype DamageModifier = DamageModifier {modifier :: Maybe Int} deriving (Show, Eq, Generic)

instance FromJSON DamageModifier where
  parseJSON (Number n) = pure $ DamageModifier (Just (round n))
  parseJSON Null = pure $ DamageModifier Nothing
  parseJSON invalid =
    prependFailure
      "parsing DamageModifier failed, "
      (typeMismatch "Object" invalid)

data Effect
  = AllOf [Effect]
  | ApplyStatus Text Target
  | DealDamage Amount Target
  | DiscountFood Amount
  | Evolve Text
  | FaintEffect Target
  | FoodMultiplier Amount
  | GainAbility Target
  | GainExperience Target Amount
  | GainGold Amount
  | ModifyDamage Bool DamageModifier
  | ModifyStats Bool (Maybe Amount) (Maybe Amount) Target
  | OneOf [Effect]
  | ReduceHealth Amount Target
  | RefillShops Text Text
  | RepeatAbility Target Level
  | RespawnPet Attack Health
  | SplashDamage Amount
  | SummonPet Text Team (Maybe Attack) (Maybe Health)
  | SummonRandomPet Level Tier (Maybe Attack) (Maybe Health)
  | Swallow Target
  | TransferAbility Level Target Target
  | TransferStats Bool Bool Target Target (Maybe Amount)
  deriving (Show, Eq, Generic)

instance FromJSON Effect where
  parseJSON = withObject "Effect" $ \o -> do
    e <- o .: "kind"
    withText
      "kind of Effect"
      ( \case
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
            ModifyStats
              <$> o .: "untilEndOfBattle"
              <*> o .:? "attackAmount"
              <*> o .:? "healthAmount"
              <*> o .: "target"
          "OneOf" -> OneOf <$> o .: "effects"
          "ReduceHealth" -> ReduceHealth <$> o .: "percentage" <*> o .: "target"
          "RefillShops" -> RefillShops <$> o .: "food" <*> o .: "shop"
          "RepeatAbility" -> RepeatAbility <$> o .: "target" <*> o .: "level"
          "RespawnPet" -> RespawnPet <$> o .: "baseAttack" <*> o .: "baseHealth"
          "SplashDamage" -> SplashDamage <$> o .: "amount"
          "SummonPet" ->
            SummonPet <$> o .: "pet" <*> o .: "team" <*> o .:? "withAttack" <*> o .:? "withHealth"
          "SummonRandomPet" ->
            SummonRandomPet
              <$> o .: "level"
              <*> o .: "tier"
              <*> o .:? "baseAttack"
              <*> o .:? "baseHealth"
          "Swallow" -> Swallow <$> o .: "target"
          "TransferAbility" ->
            TransferAbility <$> o .: "level" <*> o .: "from" <*> o .: "to"
          "TransferStats" ->
            TransferStats
              <$> o .: "copyAttack"
              <*> o .: "copyHealth"
              <*> o .: "from"
              <*> o .: "to"
              <*> o .:? "percentage"
          x -> fail ("unknown effect kind key:" <> show x)
      )
      e

newtype Emoji = Emoji {char :: String} deriving (Show, Generic, Eq)

instance FromJSON Emoji where
  parseJSON =
    withObject
      "Emoji"
      (\v -> Emoji <$> v .: "unicodeCodePoint")

newtype Name = Name {label :: Text} deriving (Show, Generic, Eq)

instance FromJSON Name

data Trigger = AfterAttack | BeforeAttack | Buy | BuyAfterLoss | BuyFood | BuyTier1Animal | CastsAbility | EatsShopFood | EndOfTurn | EndOfTurnWith3PlusGold | EndOfTurnWith4OrLessAnimals | EndOfTurnWithLvl3Friend | Faint | Hurt | KnockOut | LevelUp | Sell | StartOfBattle | StartOfTurn | Summoned | WhenAttacking | WhenDamaged deriving (Show, Generic, Eq, Read)

instance FromJSON Trigger where
  parseJSON =
    withText "Trigger" $ \t -> case readMaybe (unpack t) of
      Just x -> pure x
      Nothing -> fail "unknown Trigger"

data Ability = Ability
  { -- | The text description of the ability.
    description :: Text,
    -- | What behaviour (by the trigger entity) will initiate the ability.
    trigger :: Trigger,
    -- | Which entity will trigger the effect.
    triggeredBy :: Target,
    -- | What the effect does.
    effect :: Effect,
    untilEndOfBattle :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON Ability where
  parseJSON =
    withObject
      "Ability"
      ( \v ->
          Ability
            <$> v .: "description"
            <*> v .: "trigger"
            <*> v .: "triggeredBy"
            <*> v .: "effect"
            <*> v .:? "untilEndOfBattle"
      )

data Probability = Probability
  { probKind :: ProbKind,
    turn :: Text,
    perShop :: Maybe PerShop,
    perSlot :: PerShop
  }
  deriving (Eq, Show, Generic)

instance FromJSON Probability where
  parseJSON =
    withObject
      "Probability"
      ( \v ->
          Probability
            <$> v .: "kind"
            <*> v .: "turn"
            <*> v .:? "perShop"
            <*> v .: "perSlot"
      )

data ProbKind = ProbShop | ProbLevelUp deriving (Eq, Show, Generic)

instance FromJSON ProbKind where
  parseJSON =
    withText "ProbKind" $ \case
      "shop" -> pure ProbShop
      "levelup" -> pure ProbLevelUp
      _ -> fail "unknown ProbKind"

data PerShop = PerShop {standardPack :: Maybe Double, expansionPack1 :: Maybe Double} deriving (Eq, Show, Generic)

per :: Pack -> PerShop -> Double
per StandardPack p = fromJust (standardPack p)
per ExpansionPack1 p = fromJust (expansionPack1 p)
per EasterEgg _ = 0

instance FromJSON PerShop where
  parseJSON =
    withObject
      "PerShop"
      ( \v ->
          PerShop
            <$> v .:? "StandardPack"
            <*> v .:? "ExpansionPack1"
      )

data Pet = Pet
  { -- | The name of the pet.
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
    petStatus :: Maybe Text,
    petNotes :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Pet where
  parseJSON =
    withObject
      "Pet"
      ( \v ->
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
            <*> v .:? "notes"
      )

data Food = Food
  { foodName :: Text,
    foodEmoji :: Emoji,
    foodCost :: Maybe Int,
    foodTier :: Tier,
    foodPacks :: [Pack],
    foodProbabilities :: Maybe [Probability],
    foodNotes :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Food where
  parseJSON =
    withObject
      "Food"
      ( \v ->
          Food
            <$> v .: "name"
            <*> v .: "image"
            <*> v .:? "cost"
            <*> v .: "tier"
            <*> v .: "packs"
            <*> v .:? "probabilities"
            <*> v .:? "notes"
      )

data Status = Status
  { statusName :: Text,
    statusEmoji :: Emoji,
    statusAbility :: Ability
  }
  deriving (Eq, Show, Generic)

instance FromJSON Status where
  parseJSON =
    withObject
      "Status"
      ( \v ->
          Status
            <$> v .: "name"
            <*> v .: "image"
            <*> v .: "ability"
      )

-- * Board

data Board = Board
  { hearts :: Hearts,
    deck :: Deck,
    shop :: Shop
  }
  deriving (Generic, Eq, Show)

type Hearts = Int

newtype Deck = Deck
  { deckV :: V.Vector (Maybe DeckPet)
  }
  deriving (Generic, Eq, Show)

data DeckPet = DeckPet
  { deckPet :: Key,
    attack :: Int,
    health :: Int,
    dpStatus :: Maybe Status
  }
  deriving (Generic, Eq, Show)

data Shop = Shop
  { petShop :: PetShop,
    foodShop :: FoodShop,
    petShopSize :: PetShopSize,
    foodShopSize :: FoodShopSize,
    petShopBoosts :: PetShopBoosts
  }
  deriving (Generic, Eq, Show)

type PetShopSize = Int

type FoodShopSize = Int

data Frozen = Frozen | UnFrozen deriving (Generic, Eq, Show)

newtype PetShop = PetShop
  { petShopV :: V.Vector (Maybe (Frozen, DeckPet))
  }
  deriving (Generic, Eq, Show)

newtype FoodShop = FoodShop
  { foodShopV :: V.Vector (Maybe (Frozen, FoodKey))
  }
  deriving (Generic, Eq, Show)

data PetShopBoosts = PetShopBoosts Int Int deriving (Generic, Eq, Show)

blankBoard :: TurnKey -> State SapState Board
blankBoard t = do
  s <- get
  pure $ Board initialHearts blankDeck (blankShop (animalShopSlots $ (Map.!) (turns s) t) (foodShopSlots $ (Map.!) (turns s) t))

initialHearts :: Hearts
initialHearts = 10

blankDeck :: Deck
blankDeck = Deck (V.replicate 5 Nothing)

blankShop :: PetShopSize -> FoodShopSize -> Shop
blankShop pss fss = Shop (PetShop (V.replicate pss Nothing)) (FoodShop (V.replicate fss Nothing)) pss fss (PetShopBoosts 0 0)

type TurnNumber = Int

petSlotProbs :: TurnKey -> State SapState [(PetKey, Double)]
petSlotProbs t = ps <$> get
  where
    ps s = second (per (pack s) . perSlot . head . Prelude.filter ((== t) . A.fromText . turn) . fromJust . petProbabilities) <$> Map.toList (tierPets s)
    tierPets s =
      Map.filter
        ( \x ->
            (List.elem (pack s) . packs $ x)
              && (isJust . petProbabilities $ x)
              && (toTierNumber (tier x) <= (turns s Map.! t & index))
        )
        $ pets s

toTierNumber :: Tier -> Int
toTierNumber (TierN t) = t
toTierNumber TierSummoned = 99

foodSlotProbs :: TurnKey -> State SapState [(FoodKey, Double)]
foodSlotProbs t = fs <$> get
  where
    fs s = second (per (pack s) . perSlot . head . Prelude.filter ((== t) . A.fromText . turn) . fromJust . foodProbabilities) <$> Map.toList (tierFoods s)
    tierFoods s =
      Map.filter
        ( \x ->
            (List.elem (pack s) . foodPacks $ x)
              && (isJust . foodProbabilities $ x)
              && (toTierNumber (foodTier x) <= (turns s Map.! t & index))
        )
        $ foods s

cumProbs :: [(a, Double)] -> [(a, Double)]
cumProbs xs = zip (fst <$> xs) (fmap (/ sum (snd <$> xs)) (scanl1 (+) (snd <$> xs)))

-- | Uniform random variate in (0,1)
rvu :: (RandomGen g) => State g Double
rvu = do
  g <- get
  let (x, g') = uniformR (0, 1) g
  put g'
  pure x

rvus :: (RandomGen g) => Int -> State g [Double]
rvus n = replicateM n rvu

rva :: (Foldable t, RandomGen g) => t (a, Double) -> State g a
rva xs = do
  p <- rvu
  pure $ maybe (error "wtf") fst (find ((> p) . snd) xs)

rvas :: (Foldable t, RandomGen g) => Int -> t (a, Double) -> State g [a]
rvas n xs = replicateM n (rva xs)

mkDeckPet :: PetShopBoosts -> PetKey -> State SapState DeckPet
mkDeckPet (PetShopBoosts a h) k = do
  s <- get
  pure $
    DeckPet k
    (a + fromAttack (baseAttack ((Map.!) (pets s) k)))
    (h + fromHealth (baseHealth ((Map.!) (pets s) k)))
    Nothing

roll :: TurnKey -> Board -> State SapState Board
roll t b = do
  newPetShop <- mapM mkPet (view (#shop % #petShop % #petShopV) b)
  newFoodShop <- mapM mkFood (view (#shop % #foodShop % #foodShopV) b)
  pure $
    b
      & #hearts %~ (\x -> x - 1)
      & #shop % #petShop % #petShopV .~ newPetShop
      & #shop % #foodShop % #foodShopV .~ newFoodShop
  where
    psb = view (#shop % #petShopBoosts) b

    mkPet slot =
      bool (pure slot) (fmap (Just . (UnFrozen,)) (newShopPet t psb)) (isNothing slot || Just UnFrozen == fmap fst slot)

    mkFood slot =
      bool (pure slot) (fmap (Just . (UnFrozen,)) (newShopFood t)) (isNothing slot || Just UnFrozen == fmap fst slot)

newShopPet :: TurnKey -> PetShopBoosts -> State SapState DeckPet
newShopPet t psb = do
  xs <- (fmap cumProbs . petSlotProbs) t
  k <- zoom #gen (rva xs)
  mkDeckPet psb k

newShopFood :: TurnKey -> State SapState FoodKey
newShopFood t = do
  xs <- (fmap cumProbs . foodSlotProbs) t
  zoom #gen (rva xs)
