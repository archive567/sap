{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use if" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sap where

import Control.Monad
import Control.Monad.State.Lazy
import Data.Aeson hiding (Key)
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A
import Data.Aeson.Types hiding (Key)
import Data.Bifunctor
import Data.Bool
import qualified Data.ByteString.Lazy as B
import Data.Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text, unpack, pack)
import qualified Data.Vector as V
import GHC.Generics hiding (to, from)
import Optics.Core hiding (to)
import Optics.Zoom
import System.Random
import Text.Read (readMaybe)
import Prelude
import GHC.Exts

newtype Key a = Key { aKey :: A.Key } deriving (Eq, Show, Ord, Generic, IsString, FromJSON)

data FoodKey
data PetKey
data StatusKey
data TurnKey

instance Pretty (Key FoodKey) where
  pretty k = do
    s <- get
    (Map.!) (view #foods s) k & view (#foodEmoji % #char) & pack & pure

instance Pretty (Key PetKey) where
  pretty k = do
    s <- get
    (Map.!) (view #pets s) k & view (#petEmoji % #char) & pack & pure

instance Pretty (Key StatusKey) where
  pretty k = do
    s <- get
    (Map.!) (view #statuses s) k & view (#statusEmoji % #char) & pack & pure

instance Pretty (Key TurnKey) where
  pretty k = do
    s <- get
    (Map.!) (view #turns s) k & view #turnName & pure

mkValue :: IO (Maybe Value)
mkValue = decode <$> B.readFile "other/sap.json"

data SapState = SapState
  { foods :: Map.Map (Key FoodKey) Food,
    pets :: Map.Map (Key PetKey) Pet,
    statuses :: Map.Map (Key StatusKey) Status,
    turns :: Map.Map (Key TurnKey) Turn,
    gen :: StdGen,
    sapPack :: Pack
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
  { foods_ :: Map.Map (Key FoodKey) Food,
    pets_ :: Map.Map (Key PetKey) Pet,
    statuses_ :: Map.Map (Key StatusKey) Status,
    turns_ :: Map.Map (Key TurnKey) Turn
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
kvParser :: (FromJSON a) => Object -> Key k -> Parser (Map.Map (Key k) a)
kvParser v l =
  maybe
    (fail "label not found")
    (withObject "kv" (innerKV 2))
    (A.lookup (aKey l) v)

innerKV :: FromJSON a => Int -> Object -> Parser (Map.Map (Key k) a)
innerKV n o =
  bool
    (fail ("innerKV: " <> show (take n errs)))
    (pure x)
    (Prelude.null errs)
  where
    res = second (parseEither parseJSON) <$> A.toList o
    errs = [(k, e) | (k, Left e) <- res]
    x = Map.fromList [(Key k, e) | (k, Right e) <- res]

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
  | ApplyStatus (Key StatusKey) Target
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
    foodNotes :: Maybe Text,
    foodAbility :: Ability
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
            <*> v .: "ability"
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
    shop :: Shop,
    boardTurn :: (Key TurnKey)
  }
  deriving (Generic, Eq, Show)

type Hearts = Int

newtype Deck = Deck
  { deckV :: V.Vector (Maybe DeckPet)
  }
  deriving (Generic, Eq, Show)

data DeckPet = DeckPet
  { deckPet :: (Key PetKey),
    attack :: Int,
    health :: Int,
    -- until end of battle
    attackUeob :: Int,
    healthUeob :: Int,
    dpStatus :: Maybe (Key StatusKey)
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
  { foodShopV :: V.Vector (Maybe (Frozen, (Key FoodKey)))
  }
  deriving (Generic, Eq, Show)

data PetShopBoosts = PetShopBoosts Int Int deriving (Generic, Eq, Show)

blankBoard :: (Key TurnKey) -> State SapState Board
blankBoard t = do
  s <- get
  pure $ Board initialHearts blankDeck (blankShop (animalShopSlots $ (Map.!) (turns s) t) (foodShopSlots $ (Map.!) (turns s) t)) t

initialHearts :: Hearts
initialHearts = 10

blankDeck :: Deck
blankDeck = Deck (V.replicate 5 Nothing)

blankShop :: PetShopSize -> FoodShopSize -> Shop
blankShop pss fss = Shop (PetShop (V.replicate pss Nothing)) (FoodShop (V.replicate fss Nothing)) pss fss (PetShopBoosts 0 0)

type TurnNumber = Int

petSlotProbs :: (Key TurnKey) -> State SapState [((Key PetKey), Double)]
petSlotProbs t = ps <$> get
  where
    ps s = second (per (sapPack s) . perSlot . head . Prelude.filter ((== t) . Key . A.fromText . turn) . fromJust . petProbabilities) <$> Map.toList (tierPets s)
    tierPets s =
      Map.filter
        ( \x ->
            (List.elem (sapPack s) . packs $ x)
              && (isJust . petProbabilities $ x)
              && (toTierNumber (tier x) <= (turns s Map.! t & index))
        )
        $ pets s

toTierNumber :: Tier -> Int
toTierNumber (TierN t) = t
toTierNumber TierSummoned = 99

foodSlotProbs :: (Key TurnKey) -> State SapState [((Key FoodKey), Double)]
foodSlotProbs t = fs <$> get
  where
    fs s = second (per (sapPack s) . perSlot . head . Prelude.filter ((== t) . Key . A.fromText . turn) . fromJust . foodProbabilities) <$> Map.toList (tierFoods s)
    tierFoods s =
      Map.filter
        ( \x ->
            (List.elem (sapPack s) . foodPacks $ x)
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

mkDeckPet :: PetShopBoosts -> (Key PetKey) -> State SapState DeckPet
mkDeckPet (PetShopBoosts a h) k = do
  s <- get
  pure $
    DeckPet k
    (a + fromAttack (baseAttack ((Map.!) (pets s) k)))
    (h + fromHealth (baseHealth ((Map.!) (pets s) k)))
    0
    0
    Nothing

roll :: Board -> State SapState Board
roll b = do
  newPetShop <- mapM mkPet (view (#shop % #petShop % #petShopV) b)
  newFoodShop <- mapM mkFood (view (#shop % #foodShop % #foodShopV) b)
  pure $
    b
      & #shop % #petShop % #petShopV .~ newPetShop
      & #shop % #foodShop % #foodShopV .~ newFoodShop
  where
    psb = view (#shop % #petShopBoosts) b

    mkPet slot =
      bool (pure slot) (fmap (Just . (UnFrozen,)) (newShopPet (boardTurn b) psb)) (isNothing slot || Just UnFrozen == fmap fst slot)

    mkFood slot =
      bool (pure slot) (fmap (Just . (UnFrozen,)) (newShopFood (boardTurn b))) (isNothing slot || Just UnFrozen == fmap fst slot)

midRoll :: Board -> State SapState (Maybe Board)
midRoll b
  | view #hearts b == 0 = pure Nothing
  | otherwise = (fmap (Just . over #hearts (\x -> x - 1))) (roll b)

newShopPet :: (Key TurnKey) -> PetShopBoosts -> State SapState DeckPet
newShopPet t psb = do
  xs <- (fmap cumProbs . petSlotProbs) t
  k <- zoom #gen (rva xs)
  mkDeckPet psb k

newShopFood :: (Key TurnKey) -> State SapState (Key FoodKey)
newShopFood t = do
  xs <- (fmap cumProbs . foodSlotProbs) t
  zoom #gen (rva xs)

newTurn :: Board -> State SapState Board
newTurn b = do
  let b' = b & over #boardTurn incTurn & set #hearts initialHearts
  roll b'

incTurn :: (Key TurnKey) -> (Key TurnKey)
incTurn "turn-1" = "turn-2"
incTurn "turn-2" = "turn-3"
incTurn "turn-3" = "turn-4"
incTurn "turn-4" = "turn-5"
incTurn "turn-5" = "turn-6"
incTurn "turn-6" = "turn-7"
incTurn "turn-7" = "turn-8"
incTurn "turn-8" = "turn-9"
incTurn "turn-9" = "turn-10"
incTurn "turn-10" = "turn-11"
incTurn "turn-11" = "turn-12"
incTurn _ = "turn-12"

startBoard :: State SapState Board
startBoard = roll =<< blankBoard "turn-1"

shuffle :: Int -> Int -> Board -> Either Text Board
shuffle from to b
  | isNothing from' = Left ("No DeckPet @ " <> pack (show from))
  | to == from = Left "Shuffle NoOp"
  | otherwise = Right (shuffleDeck from to b)
  where
    from' = view (#deck % #deckV) b V.! from

del_ :: Int -> V.Vector a -> V.Vector a
del_ x v = V.take x v <> V.drop (x+1) v

one_ :: Int -> V.Vector a -> V.Vector a
one_ x v = V.drop x v & V.take 1

ins_ :: Int -> V.Vector a -> V.Vector a -> V.Vector a
ins_ x v v' = V.take x v <> v' <> V.drop x v

shuffleDeck :: Int -> Int -> Board -> Board
shuffleDeck from to b =
  b & (#deck % #deckV) %~ (\v ->
    ins_ to (del_ from v) (one_ from v))

deckSize :: Int
deckSize = 5

leftCompact_ :: V.Vector (Maybe a) -> V.Vector (Maybe a)
leftCompact_ d = d' <> V.replicate (deckSize - V.length d') Nothing
    where
      d' = V.filter isJust d

recruit :: Int -> Int -> Board -> Either Text Board
recruit from to b
  | isNothing from' = Left ("No ShopPet @ " <> pack (show from))
  | not vacancy = Left "Deck is full"
  | otherwise = Right recruit_
  where
    from' = view (#shop % #petShop % #petShopV) b V.! from
    vacancy = not $ V.all isJust (view (#deck % #deckV) b)
    recruit_ =
      over (#shop % #petShop % #petShopV) (V.// [(from, Nothing)]) $
      over #hearts (\x -> x - 3) $
      over (#deck % #deckV)
      (\v -> V.take deckSize (ins_ to (leftCompact_ v) (V.singleton (snd <$> from')))) b

eat :: Int -> Maybe Int -> Board -> State SapState (Either Text Board)
eat from to b
  | isNothing from' = pure (Left ("No ShopFood @ " <> pack (show from)))
  | otherwise = do
      e <- effect'
      applyEat e from to b
  where
    from' = snd <$> view (#shop % #foodShop % #foodShopV) b V.! from

    effect' = do
      s <- get
      pure $ view (#foodAbility % #effect) $ (Map.!) (foods s) $
        fromMaybe (error "wtf") from'

applyEat :: Effect -> Int -> Maybe Int -> Board -> State SapState (Either Text Board)
applyEat eff from to b = do
  applyEatEffect eff to (over (#shop % #foodShop % #foodShopV) (V.// [(from, Nothing)]) b)

applyEatEffect :: Effect -> Maybe Int -> Board -> State SapState (Either Text Board)
applyEatEffect eff to b = case eff of
  ApplyStatus st (Target PurchaseTarget _ _) ->
    pure $
    Right
    (over (#deck % #deckV)
     (\v -> v V.//
       [(to',
          (set #dpStatus (Just st)) <$> (v V.! to'))
       ]) b)
  ModifyStats ueob (Just (Amount h)) (Just (Amount a)) (Target PurchaseTarget _ _) ->
    pure $
    Right
    (over (#deck % #deckV)
     (\v -> v V.//
       [(to',
          (change' <$> (v V.! to')))
       ]) b)
    where
      change' = case ueob of
        False -> over #attack (+a) . over #health (+h)
        True -> over #attackUeob (+a) . over #healthUeob (+h)
  e -> error (show e <> " TBI")
  where
    to' = fromMaybe (error "bad from") to

freezePet :: Int -> Board -> Either Text Board
freezePet i b
  | isNothing i' = Left ("No ShopPet @ " <> pack (show i))
  | Just Frozen == (fst <$> i') = Left "Already frozen"
  | otherwise = Right (over (#shop % #petShop % #petShopV) (\v -> v V.//
       [(i,
          (first (const Frozen) <$> (v V.! i)))
       ]) b)
  where
    i' = view (#shop % #petShop % #petShopV) b V.! i

unfreezePet :: Int -> Board -> Either Text Board
unfreezePet i b
  | isNothing i' = Left ("No ShopPet @ " <> pack (show i))
  | Just UnFrozen == (fst <$> i') = Left "Not frozen"
  | otherwise = Right (over (#shop % #petShop % #petShopV) (\v -> v V.//
       [(i,
          (first (const UnFrozen) <$> (v V.! i)))
       ]) b)
  where
    i' = view (#shop % #petShop % #petShopV) b V.! i

freezeFood :: Int -> Board -> Either Text Board
freezeFood i b
  | isNothing i' = Left ("No Food @ " <> pack (show i))
  | Just Frozen == (fst <$> i') = Left "Already frozen"
  | otherwise = Right (over (#shop % #foodShop % #foodShopV) (\v -> v V.//
       [(i,
          (first (const Frozen) <$> (v V.! i)))
       ]) b)
  where
    i' = view (#shop % #foodShop % #foodShopV) b V.! i

unfreezeFood :: Int -> Board -> Either Text Board
unfreezeFood i b
  | isNothing i' = Left ("No Food @ " <> pack (show i))
  | Just UnFrozen == (fst <$> i') = Left "Not frozen"
  | otherwise = Right (over (#shop % #foodShop % #foodShopV) (\v -> v V.//
       [(i,
          (first (const UnFrozen) <$> (v V.! i)))
       ]) b)
  where
    i' = view (#shop % #foodShop % #foodShopV) b V.! i

type PetTierList = [[(Key PetKey)]]

matchPT :: PetTierList -> [((Key PetKey), a)] -> State SapState (Maybe a)
matchPT pt l = fmap (fmap (l' Map.!)) $
  zoom #gen $
  foldr (\xs acc -> bool (pick xs) acc (null xs)) (pure Nothing) pt'
  where
    pick list = bool (Just <$> rva (cumProbs $ (,1) <$> list)) (pure $ listToMaybe list) (length list ==1)
    pt' = List.filter (`elem` (Map.keys l')) <$> pt
    l' = Map.fromList l

recruitPet :: PetTierList -> Board -> State SapState (Maybe Board)
recruitPet tl b
  | view #hearts b < 3 = pure Nothing
  | otherwise = do
      p <- pick
      case p of
        Nothing -> pure Nothing
        Just p' -> pure $ either (const Nothing) Just (recruit p' 0 b)
  where
    pick = matchPT tl (shopPetsI b)

recruitPetAlways :: PetTierList -> Board -> State SapState (Maybe Board)
recruitPetAlways tl b
  | view #hearts b == 0 = pure (Just b)
  | view #hearts b < 3 = recruitPetAlways tl `mPlug` midRoll b
  | otherwise = mPlug (recruitPetAlways tl) (recruitPet tl b)

mPlug :: (Board -> State SapState (Maybe Board)) -> State SapState (Maybe Board) -> State SapState (Maybe Board)
mPlug f mb = do
  b' <- mb
  case b' of
    Nothing -> pure Nothing
    Just b'' -> f b''

shopPetsI :: Board -> [((Key PetKey), Int)]
shopPetsI b = V.ifoldl' (\acc i a -> maybe acc (\x -> (x,i):acc) a) [] $
  fmap (fmap (deckPet . snd)) (view (#shop % #petShop % #petShopV) b)


-- * Battle

data Battle a =
  Battle {
  deckL :: Deck,
  deckR :: Deck,
  battleTurn :: Int
  } deriving (Eq, Show, Generic)

data BeforeStart

battle :: Deck -> Deck -> Battle BeforeStart
battle d d' = Battle d d' 0

class Pretty a where
  pretty :: a -> State SapState Text
