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
{-# HLINT ignore "Redundant flip" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

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
import Optics.Core
import Optics.Zoom
import System.Random
import Text.Read (readMaybe)
import Prelude
import GHC.Exts
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

class Pretty a where
  pretty :: a -> State SapState Text

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

prettyShow :: (Pretty a) => SapState -> State SapState a -> IO ()
prettyShow s a = Text.putStrLn $ flip evalState s (pretty =<< a)

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

instance Pretty Board where
  pretty (Board h d s t) = do
    h' <- pretty h
    d' <- pretty d
    s' <- pretty s
    t' <- pretty t
    pure $ Text.intercalate " " [d', s', h', t']

newtype Hearts = Hearts { nhearts :: Int } deriving (Eq, Ord, Show, Generic)

subn :: Int -> Text
subn x = pack $ (Char.chr . (+8320) . (\c -> c - 48) . Char.ord) <$> show x

instance Pretty Hearts where
  pretty h = pure $ "\129505" <> subn (view #nhearts h)

-- | A blank position on the deck is mostly a UI feature, and makes no difference to any gameplay.
-- Because a player can move from one blank position to another, however, the concept is retained so this can be represented.
--
newtype Deck = Deck
  { deckV :: V.Vector (Maybe DeckPet)
  }
  deriving (Generic, Eq, Show)

instance Pretty Deck where
  pretty (Deck v) = do
    v' <- mapM (maybe (pure "_") pretty) (V.toList v)
    pure $ Text.intercalate " " v'

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

att :: DeckPet -> Int
att dp = view #attack dp + view #attackUeob dp

hea :: DeckPet -> Int
hea dp = view #health dp + view #healthUeob dp

instance Pretty DeckPet where
  pretty dp = do
    p <- pretty (view #deckPet dp)
    s <- maybe (pure "") pretty (view #dpStatus dp)
    pure $
      subn (view #attack dp + view #attackUeob dp) <>
      p <>
      subn (view #health dp + view #healthUeob dp) <>
      s

data Shop = Shop
  { petShop :: PetShop,
    foodShop :: FoodShop,
    petShopSize :: PetShopSize,
    foodShopSize :: FoodShopSize,
    petShopBoosts :: PetShopBoosts
  }
  deriving (Generic, Eq, Show)

instance Pretty Shop where
  pretty (Shop ps fs _ _ _) = do
    ps' <- pretty ps
    fs' <- pretty fs
    pure $ ps' <> " " <> fs'

type PetShopSize = Int

type FoodShopSize = Int

data Frozen = Frozen | UnFrozen deriving (Generic, Eq, Show)

newtype PetShop = PetShop
  { petShopV :: V.Vector (Maybe (Frozen, DeckPet))
  }
  deriving (Generic, Eq, Show)

instance Pretty PetShop where
  pretty (PetShop v) = do
    v' <- mapM (maybe (pure "_")
                (\(f,k) -> (bool "" frozen (f==Frozen) <>) <$> pretty k)) (V.toList v)
    pure $ Text.intercalate " " v'

newtype FoodShop = FoodShop
  { foodShopV :: V.Vector (Maybe (Frozen, (Key FoodKey)))
  }
  deriving (Generic, Eq, Show)

frozen :: Text
frozen = "/9617"

instance Pretty FoodShop where
  pretty (FoodShop v) = do
    v' <- mapM (maybe (pure "_")
                (\(f,k) -> (bool "" frozen (f==Frozen) <>) <$> pretty k)) (V.toList v)
    pure $ Text.intercalate " " v'

data PetShopBoosts = PetShopBoosts Int Int deriving (Generic, Eq, Show)

blankBoard :: (Key TurnKey) -> State SapState Board
blankBoard t = do
  s <- get
  pure $ Board initialHearts blankDeck (blankShop (animalShopSlots $ (Map.!) (turns s) t) (foodShopSlots $ (Map.!) (turns s) t)) t

initialHearts :: Hearts
initialHearts = Hearts 10

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

-- | uniform (0,n-1)
rvi :: (RandomGen g) => Int -> State g Int
rvi n = do
  g <- get
  let (a,g') = uniformR (0 ,n-1) g
  put g'
  pure a

-- | reducing finite population n samples
--
-- @rvis 52 2@ produces a list containing a random variate between 0 and 51, and an rv between 0 and 50.
--
-- >>> let xs = evalState (rvis 52 7) (mkStdGen 42)
-- >>> xs
-- [48,23,31,15,16,18,17]
--
rvis :: (RandomGen g) => Int -> Int -> State g [Int]
rvis n k = sequence (rvi . (n -) <$> [0 .. (k - 1)])

-- | Creates sample without replacement given an 'rvis' process.
--
-- Computation treats rvis output as indices.
--
-- isomorphic to fst . shuffle 52
--
-- >>> rvs52 = flip evalState (mkStdGen 42) $ rvis 52 52
-- >>> ishuffle rvs52
-- [48,23,32,15,17,20,19,28,11,39,5,18,41,38,37,2,12,16,44,40,29,0,21,4,6,26,22,7,45,25,33,46,14,43,9,3,30,1,13,50,10,36,31,49,35,24,51,47,34,27,8,42]
--
-- TODO: refactor the sort
ishuffle :: (Ord e, Num e) => [e] -> [e]
ishuffle as = reverse $ go as []
  where
    go [] s = s
    go (x0 : xs) s = go xs (x1 : s)
      where
        x1 = foldl' (\acc d -> bool acc (acc + 1) (d <= acc)) x0 (List.sort s)

-- | deal n cards from a fresh, shuffled, standard pack.
--
dealN :: (RandomGen g) => Int -> Int -> State g [Int]
dealN k n = ishuffle <$> rvis k n

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
  | view #hearts b == Hearts 0 = pure Nothing
  | otherwise = (fmap (Just . over #hearts (\(Hearts x) -> Hearts (x - 1)))) (roll b)

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
shuffle from to' b
  | isNothing from' = Left ("No DeckPet @ " <> pack (show from))
  | to' == from = Left "Shuffle NoOp"
  | otherwise = Right (shuffleDeck from to' b)
  where
    from' = view (#deck % #deckV) b V.! from

merge :: Int -> Int -> Board -> Either Text Board
merge from to' b
  | isNothing f = Left ("No DeckPet @ " <> pack (show from))
  | isNothing t = Left ("No DeckPet @ " <> pack (show to'))
  | t /= f = Left "No merge for Different Pets"
  | otherwise = Right (mergeDeck from to' b)
  where
    f = view (bPet from) b
    t = view (bPet to') b

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

-- FIXME: not sure what happens to side effect
mergeDeck :: Int -> Int -> Board -> Board
mergeDeck from to' b =
  case (view (bPet from) b, view (bPet to') b) of
    (Just f, Just t) ->
      over bDeck (\v -> ins_ from (V.singleton (Just x)) (del_ to' $ del_ from v)) b
      where
        x =
          set #attack (max (view #attack f) (view #attack t) + 1) $
          set #attack (max (view #attack f) (view #attack t) + 1) $
          set #dpStatus st
          f
        st = listToMaybe $ catMaybes [view #dpStatus f, view #dpStatus t]
    _ -> b

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
      over #hearts (\(Hearts x) -> Hearts (x - 3)) $
      over (#deck % #deckV)
      (\v -> V.take deckSize (ins_ to (leftCompact_ v) (V.singleton (snd <$> from')))) b

-- * board effects
-- FIXME:
-- abstract this pattern
bPet :: Int -> Lens' Board (Maybe DeckPet)
bPet pos = lens (toBPet_ pos) (fromBPet_ pos)

fromBPet_ :: Int -> Board -> Maybe DeckPet -> Board
fromBPet_ k b p = over bDeck (\v -> V.unsafeUpd v [(k,p)]) b

toBPet_ :: Int -> Board -> Maybe DeckPet
toBPet_ k = (V.! k) . view bDeck

bDeck :: Lens' Board (V.Vector (Maybe DeckPet))
bDeck = #deck % #deckV

bPetShop :: Lens' Board (V.Vector (Maybe (Frozen, DeckPet)))
bPetShop = #shop % #petShop % #petShopV

sPet :: Int -> Lens' Board (Maybe (Frozen, DeckPet))
sPet pos = lens (toSPet_ pos) (fromSPet_ pos)

fromSPet_ :: Int -> Board -> Maybe (Frozen, DeckPet) -> Board
fromSPet_ k b p = over bPetShop (\v -> V.unsafeUpd v [(k,p)]) b

toSPet_ :: Int -> Board -> Maybe (Frozen, DeckPet)
toSPet_ k = (V.! k) . view bPetShop

-- forgets index positions in the round-trip
bDeckI :: Lens' Board (V.Vector DeckPet)
bDeckI = bDeck % bDeckI_

bDeckI_ :: Iso' (V.Vector (Maybe DeckPet)) (V.Vector DeckPet)
bDeckI_ = iso V.catMaybes (\v -> fmap Just v <> V.replicate (5 - V.length v) Nothing)

bSize :: Getter Board Int
bSize = to (V.length . V.filter isJust . view bDeck)

kth :: Int -> Int -> Board -> Int
kth self kth b =
  view bDeck b &
  V.zip (V.fromList [0..]) &
  V.filter (\(k,p) -> k /= self && isJust p) &
  (V.! kth) &
  fst

applyBoardEffectsBy :: Trigger -> (Target -> Bool) -> (Int -> Maybe DeckPet -> Bool) -> Board -> State SapState Board
applyBoardEffectsBy t target f b = do
  es <- boardEffectsBy t target f b
  foldr (>=>) pure (uncurry applyBoardEffect <$> es) b

-- FIXME:
--
-- rationalise these effectsby functions
boardEffectsBy :: Trigger -> (Target -> Bool) -> (Int -> Maybe DeckPet -> Bool) -> Board -> State SapState [(Effect, Int)]
boardEffectsBy t target f bd = do
  s <- get
  pure $ (\l -> [(e,i) | (i,Just e) <- l]) $ (second (fmap (view #effect) . view #level1Ability . (Map.!) (pets s) . deckPet)) <$> (List.filter ((\a -> ((Just True ==) $ fmap (target . view #triggeredBy) a) && ((Just True ==) . (fmap (\x -> view #trigger x == t)) $ a)) . view #level1Ability . (Map.!) (pets s) . deckPet . snd) $ (\x -> [(i,p)| (i, Just p) <- x ]) $ List.filter (uncurry f) $ zip [(0::Int)..] . V.toList . view bDeck $ bd)

applyBoardEffect :: Effect -> Int -> Board -> State SapState Board
-- otter
-- FIXME: use beaver logic below
applyBoardEffect (ModifyStats False (Just (Amount a)) (Just (Amount h)) (Target RandomFriend (Just 1) Nothing)) r b = do
  let n = view bSize b - 1
  k <- zoom #gen (rvi n)
  pure $
    over (bPet (kth r k b)) (fmap (over #health (h+))) $
    over (bPet (kth r k b)) (fmap (over #attack (a+)))
    b
-- horse
applyBoardEffect (ModifyStats True (Just (Amount a)) (Just (Amount h)) (Target TriggeringEntity Nothing Nothing)) r b = do
  pure $
    over (bPet r) (fmap (over #healthUeob (h+))) $
    over (bPet r) (fmap (over #attackUeob (a+))) $
    b
-- duck
applyBoardEffect (ModifyStats False Nothing (Just (Amount h)) (Target EachShopAnimal Nothing Nothing)) _ b = do
  pure $ over bPetShop (fmap (fmap (second (over #health (+h))))) b
-- beaver
applyBoardEffect (ModifyStats False Nothing (Just (Amount h)) (Target RandomFriend (Just k) Nothing)) r b = do
  -- FIXME: seems risky to assume pet is still in the list
  let n = view bSize b - 1
  ks <- zoom #gen (dealN n k)
  pure $
    foldl' (flip ($)) b (fmap (\k -> over (bPet (kth r k b)) (fmap (over #health (h+)))) ks)

applyBoardEffect eff _ _ = error ("applyBoardEffect: NYI: " <> show eff)

buy :: Int -> Int -> Board -> State SapState (Either Text Board)
buy from to b = do
  case recruit from to b of
    Left e -> pure (Left e)
    Right b' -> Right <$>
      (applyBoardEffectsBy Summoned ((==EachFriend) . view #targetType) (\k _ -> k/=to) =<<
       applyBoardEffectsBy Buy ((==Self) . view #targetType) (\k _ -> k==to)
       b')

sell :: Int -> Board -> State SapState (Either Text Board)
sell from b = do
  case view (bPet from) b of
    Nothing -> pure (Left "No pet at deck location")
    Just _ -> Right <$> do
      set (bPet from) Nothing <$>
        (applyBoardEffectsBy Sell ((==Self) . view #targetType) (\k _ -> k==from)
         b)

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

-- * Strategy

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
  | view (#hearts % #nhearts) b < 3 = pure Nothing
  | otherwise = do
      p <- pick
      case p of
        Nothing -> pure Nothing
        Just p' -> either (const Nothing) Just <$> buy p' 0 b
  where
    pick = matchPT tl (shopPetsI b)

recruitPetAlways :: PetTierList -> Board -> State SapState (Maybe Board)
recruitPetAlways tl b
  | view (#hearts % #nhearts) b == 0 = pure (Just b)
  | view (#hearts % #nhearts) b < 3 = recruitPetAlways tl `mPlug` midRoll b
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

-- indexes of live pets in deck
-- number of live pets in deck
-- number of live pets in (friendly|enemy) deck
-- over (friendly|enemy) i'th live

-- | A battle is a one-way trip: nathing that happens in a battle effects the Deck
data BattlePet = BattlePet
  { bpet :: (Key PetKey),
    battack :: Int,
    bhealth :: Int,
    bstatus :: Maybe (Key StatusKey)
  }
  deriving (Generic, Eq, Ord, Show)

instance Pretty BattlePet where
  pretty bp = do
    p <- pretty (view #bpet bp)
    s <- maybe (pure "") pretty (view #bstatus bp)
    pure $
      subn (view #battack bp) <>
      p <>
      subn (view #bhealth bp) <>
      s

fromDeckPet :: DeckPet -> BattlePet
fromDeckPet p = BattlePet (view #deckPet p) (att p) (hea p) (view #dpStatus p)


newtype BattleDeck = BattleDeck
  { bdeckV :: V.Vector BattlePet
  }
  deriving (Generic, Eq, Ord, Show)

instance Pretty BattleDeck where
  pretty (BattleDeck v) = do
    v' <- mapM pretty (V.toList v)
    pure $ Text.intercalate " " v'

prettyReverse :: BattleDeck -> State SapState Text
prettyReverse (BattleDeck v) = do
    v' <- mapM pretty (V.toList v)
    pure $ Text.intercalate " " (reverse v')

newtype Battle =
  Battle {
    decks :: (BattleDeck, BattleDeck)
  } deriving (Eq, Show, Generic, Ord)

instance Pretty Battle where
  pretty (Battle (l,r)) = do
    l' <- prettyReverse l
    r' <- pretty r
    pure $ Text.intercalate "|" [l', r']

mkBattle :: Board -> Board -> Battle
mkBattle b b' =
  Battle $ (,)
  ((view (#deck % #deckV) b) & fmap (fmap fromDeckPet) & V.toList & catMaybes & V.fromList & BattleDeck)
  ((view (#deck % #deckV) b') & fmap (fmap fromDeckPet) & V.toList & catMaybes & V.fromList & BattleDeck)

data Side = SideLeft | SideRight deriving (Eq, Show, Generic)

data Pos = Pos { posSide :: Side, posIndex :: Int } deriving (Eq, Show, Generic)

other :: Side -> Side
other SideLeft = SideRight
other SideRight = SideLeft

bdDeck :: Side -> Lens' Battle (V.Vector BattlePet)
bdDeck SideLeft = #decks % _1 % #bdeckV
bdDeck SideRight = #decks % _2 % #bdeckV

bdFront :: Side -> Lens' Battle BattlePet
bdFront s = bdPet (Pos s 0)

bdSize :: Side -> Getter Battle Int
bdSize SideLeft = to (V.length . view (bdDeck SideLeft))
bdSize SideRight = to (V.length . view (bdDeck SideRight))

bdPet :: Pos -> Lens' Battle BattlePet
bdPet pos = lens (toPet_ pos) (fromPet_ pos)

fromPet_ :: Pos -> Battle -> BattlePet -> Battle
fromPet_ (Pos s k) b p = over (bdDeck s) (\v -> V.unsafeUpd v [(k,p)]) b

toPet_ :: Pos -> Battle -> BattlePet
toPet_ (Pos s k) = ((V.! k) . view (bdDeck s))

bdFaints :: Side -> Getter Battle (V.Vector Int)
bdFaints s = to (V.findIndices ((<1) . view #bhealth) . view (bdDeck s))

isFaint :: BattlePet -> Bool
isFaint = (<=0) . view #bhealth

removeFainters :: Battle -> Battle
removeFainters b =
  over (bdDeck SideRight) (V.filter (not . isFaint)) $
  over (bdDeck SideLeft) (V.filter (not . isFaint)) b

-- FIXME:
-- level1Effects hard coded
--
bdfEffects_ :: Trigger -> (Int -> BattlePet -> Bool) -> BattleDeck -> State SapState [(Effect, Int)]
bdfEffects_ t f bd = do
  s <- get
  pure $ (\l -> [(e,i) | (i,Just e) <- l]) $ (second (fmap (view #effect) . view #level1Ability . (Map.!) (pets s) . bpet)) <$> (List.filter ((Just True ==) . (fmap (\x -> view #trigger x == t)) . view #level1Ability . (Map.!) (pets s) . bpet . snd) $ List.filter (uncurry f) $ zip [(0::Int)..] . V.toList . view #bdeckV $ bd)

bdfEffectsBy_ :: Trigger -> (Target -> Bool) -> (Int -> BattlePet -> Bool) -> BattleDeck -> State SapState [(Effect, Int)]
bdfEffectsBy_ t target f bd = do
  s <- get
  pure $ (\l -> [(e,i) | (i,Just e) <- l]) $ (second (fmap (view #effect) . view #level1Ability . (Map.!) (pets s) . bpet)) <$> (List.filter ((\a -> ((Just True ==) $ fmap (target . view #triggeredBy) a) && ((Just True ==) . (fmap (\x -> view #trigger x == t)) $ a)) . view #level1Ability . (Map.!) (pets s) . bpet . snd) $ List.filter (uncurry f) $ zip [(0::Int)..] . V.toList . view #bdeckV $ bd)

bfEffects :: Trigger -> (Int -> BattlePet -> Bool) -> Battle -> State SapState [(Effect, Pos)]
bfEffects t f (Battle (l, r)) =
   (<>) <$>
   (fmap (second (Pos SideLeft)) <$> (bdfEffects_ t f l)) <*>
   (fmap (second (Pos SideRight)) <$> (bdfEffects_ t f r))

bfEffectsBy :: Trigger -> (Target -> Bool) -> (Int -> BattlePet -> Bool) -> Battle -> State SapState [(Effect, Pos)]
bfEffectsBy t target f (Battle (l, r)) =
   (<>) <$>
   (fmap (second (Pos SideLeft)) <$> (bdfEffectsBy_ t target f l)) <*>
   (fmap (second (Pos SideRight)) <$> (bdfEffectsBy_ t target f r))

-- | apply an effect to the Battle
applyEffect :: Effect -> Pos -> Battle -> State SapState Battle
-- mosquito
applyEffect (DealDamage (Amount x) (Target RandomEnemy (Just 1) Nothing)) (Pos side _) b = do
  let n = view (bdSize (other side)) b
  r <- zoom #gen (rvi n)
  pure $ over (bdPet (Pos (other side) r) % #bhealth) (\h -> h - x) b
-- cricket
applyEffect (SummonPet p Friendly (Just (Attack a)) (Just (Health h))) (Pos side _) b = do
  let b' = over (bdDeck side) (V.cons (BattlePet (Key (A.fromText p)) a h Nothing)) b
  -- FIXME: assumes summoned pets come out one at a time
  applyFEffectsBy Summoned ((==EachFriend) . view #targetType) (\_ _ -> True) b'
applyEffect (ModifyStats False (Just (Amount a)) (Just (Amount h)) (Target RandomFriend (Just 1) Nothing)) (Pos side _) b = do
  let n = view (bdSize side) b - 1
  r <- zoom #gen (rvi n)
  pure $
    over (bdPet (Pos side (r+1)) % #bhealth) (h +) $
    over (bdPet (Pos side (r+1)) % #battack) (a +)
    b
applyEffect (ModifyStats True (Just (Amount a)) Nothing (Target TriggeringEntity Nothing Nothing)) (Pos side _) b = do
  pure $
    over (bdPet (Pos side 0) % #battack) (a +)
    b
applyEffect eff _ _ = error ("NYI: " <> show eff)

-- A filtered version
applyFEffects :: Trigger -> (Int -> BattlePet -> Bool) -> Battle -> State SapState Battle
applyFEffects t f b = do
  es <- bfEffects t f b
  foldr (>=>) pure (uncurry applyEffect <$> es) b

applyFEffectsBy :: Trigger -> (Target -> Bool) -> (Int -> BattlePet -> Bool) -> Battle -> State SapState Battle
applyFEffectsBy t target f b = do
  es <- bfEffectsBy t target f b
  foldr (>=>) pure (uncurry applyEffect <$> es) b

startP :: Battle -> State SapState Battle
startP = fmap removeFainters . applyFEffects StartOfBattle (\_ _ -> True)

data Outcome = WinLeft | WinRight | Draw | ContB deriving (Eq, Ord, Show, Generic)

result :: Battle -> Outcome
result b =
  case (view (bdSize SideLeft) b, view (bdSize SideRight) b) of
    (0, 0) -> Draw
    (0, _) -> WinRight
    (_, 0) -> WinLeft
    (_, _) -> ContB

-- Also:
-- FIXME:
-- WhenAttacking
-- WhenDamaged
-- Summoned
fight :: Battle -> State SapState Battle
fight b = do
  b0 <- applyFEffects BeforeAttack (\k _ -> bool False True (k==0)) b
  let b1 = ah SideLeft (ah SideRight b0)
  -- FIXME:
  -- Assumes for sure Hurt by the attack.
  b2 <- applyFEffects Hurt (\k p -> bool False True (k==0 && view #bhealth p > 0)) b1
  b3 <- applyFEffects Faint (\k p -> bool False True (k==0 && view #bhealth p <= 0)) b2
  -- FIXME:
  -- KnockOut requires knowledge of the whole Board. applyFEffects needs refactoring.
  -- FIXME:
  -- Does AfterAttack also apply if the pet is already fainted.
  b4 <- applyFEffects AfterAttack (\k _ -> bool False True (k==0)) b3
  pure b4

ah :: Side -> Battle -> Battle
ah s b = over (bdFront s % #bhealth) (\x -> x - view (bdFront (other s) % #battack) b) b

fightP :: Battle -> State SapState Battle
fightP = fmap removeFainters . fight

loop :: Battle -> State SapState (Outcome, Battle)
loop b = case result b of
  ContB -> do
    b' <- fight b
    let b'' = removeFainters b'
    loop b''
  x -> pure (x,b)

loopDebug :: Int -> Battle -> State SapState (Outcome, [Battle])
loopDebug x b = go x b []
  where
    go x' b' bs = case (x', result b') of
      (0,r) -> pure (r,reverse (b:bs))
      (_, ContB) -> do
        b'' <- fight b'
        let b''' = removeFainters b''
        go (x'-1) b''' (b''':b'':bs)
      (_, r) -> pure (r,reverse (b:bs))


