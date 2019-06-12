{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Registry.Feat where

import           Control.Enumerable
import           Control.Monad.Morph
import           Data.HashMap.Strict             as HashMap (HashMap, fromList)
import           Data.List.NonEmpty
import           Data.Map                        as Map (fromList)
import           Data.Maybe                      as Maybe
import           Data.Registry
import           Data.Registry.Internal.Hedgehog (GenIO, liftGen)
import           Data.Registry.Internal.Types
import           Data.Set                        as Set (fromList)
import           Hedgehog
import           Hedgehog.Gen                    as Gen (discard, integral)
import           Hedgehog.Internal.Gen           as Gen (GenT (..), runGenT)
import           Hedgehog.Internal.Property      (forAllT)
import           Hedgehog.Range                  as Range (Size (..), linear)
import           Protolude                       as P
import           Test.Feat.Access                (indexWith)
import           Test.Feat.Enumerate

-- * CREATION / TWEAKING OF REGISTRY ENUMERATES

-- | Get a value generated from an enumerate in the registry
--   using a state monad
enumAllS :: forall a m out . (Typeable a, Show a, MonadIO m) => PropertyT (StateT (Registry _ out) m) a
enumAllS = do
  r <- P.lift $ get
  withFrozenCallStack $ hoist liftIO $ forAllT (enumToGen $ enumWith @a r)

-- | Create an Enumerate a for a given constructor of type a
enumFun :: forall a b . (ApplyVariadic Enumerate a b, Typeable a, Typeable b) => a -> Typed b
enumFun = funTo @Enumerate

-- | Lift a value into Enumerate a to be added to a registry
enumVal :: forall a . (Typeable a) => a -> Typed (Enumerate a)
enumVal a = fun (pure a :: Enumerate a)

-- | Extract an enumerate from a registry
--   We use makeUnsafe assuming that the registry has been checked before
enumWith :: forall a ins out . (Typeable a) => Registry ins out -> Enumerate a
enumWith = makeUnsafe @(Enumerate a)

-- | Modify the value of an enumerate in a given registry
tweakEnum :: forall a ins out . (Typeable a) => (a -> a) -> Registry ins out -> Registry ins out
tweakEnum f = tweakUnsafe @(Enumerate a) (\enumA -> f <$> enumA)

-- | Modify the registry for a given enumerator in a State monad
tweakEnumS :: forall a m ins out . (Typeable a, MonadState (Registry ins out) m) => (a -> a) -> m ()
tweakEnumS f = modify (tweakEnum f)

setEnum :: forall a ins out . (Typeable a) => Enumerate a -> Registry ins out -> Registry ins out
setEnum enumA = tweakUnsafe @(Enumerate a) (const enumA)

-- | Set a specific enumerator on the registry the value of an enumerate in a given registry in a State monad
setEnumS :: forall a m ins out . (Typeable a, MonadState (Registry ins out) m) => Enumerate a -> m ()
setEnumS enumA = modify (setEnum enumA)

-- | Specialize an enumerate in a given context
specializeEnum :: forall a b ins out . (Typeable a, Typeable b, Contains (Enumerate a) out) => Enumerate b -> Registry ins out -> Registry ins out
specializeEnum = specialize @(Enumerate a)

-- | Specialize an enumerate in a given context
specializeEnumS :: forall a b m ins out . (Typeable a, Typeable b, Contains (Enumerate a) out, MonadState (Registry ins out) m) => Enumerate b -> m ()
specializeEnumS g = modify (specializeEnum @a @b g)

-- | Modify an enumerate
modifyEnumS :: forall a ins out . (Typeable a) => (Enumerate a -> Enumerate a) -> PropertyT (StateT (Registry ins out) IO) ()
modifyEnumS f = modify (tweakUnsafe @(Enumerate a) f)

-- | Make sure there is always one element of a given type in a list of elements
makeNonEmptyEnum :: forall a ins out . (Typeable a) => Registry ins out -> Registry ins out
makeNonEmptyEnum r =
  -- extract an enumerate for one element only
  let enumA = enumWith @a r
  -- add that element in front of a list of enumerated elements
  in  tweakUnsafe @(Enumerate [a]) (\enumAs -> (:) <$> enumA <*> enumAs) r

-- | Make sure there is always one element of a given type in a list of elements in a State monad
makeNonEmptyEnumS :: forall a m ins out . (Typeable a, MonadState (Registry ins out) m) => m ()
makeNonEmptyEnumS = modify (makeNonEmptyEnum @a)

-- * CONTAINERS COMBINATORS

-- | Create an enumerate for a pair
enumPairOf :: forall a b . Enumerate a -> Enumerate b -> Enumerate (a, b)
enumPairOf ga gb = (,) <$> ga <*> gb

-- | Create an enumerate for a triple
enumTripleOf :: forall a b c . Enumerate a -> Enumerate b -> Enumerate c -> Enumerate (a, b, c)
enumTripleOf ga gb gc = (,,) <$> ga <*> gb <*> gc

-- | Create an enumerate for a list of elements
enumListOf :: forall a . Enumerate a -> Enumerate [a]
enumListOf a = pay (pure [] <|> ((:) <$> a <*> enumListOf a))

-- | Create an enumerator for a list of elements of min elements and max elements
enumListOfMinMax :: forall a . Int -> Int -> Enumerate a -> Enumerate [a]
enumListOfMinMax min' max' a
  | min' < max'  = pay (enumSizedListOf min' a <|> enumListOfMinMax (min' + 1) max' a)
  | min' == max' = enumSizedListOf min' a
  | otherwise    = panic $ "cannot enumerate a list of elements with min > max. min=" <> show min' <> ", max=" <> show max'

-- | Create an enumerator for a list of elements of a fixed size
enumSizedListOf :: forall a . Int -> Enumerate a -> Enumerate [a]
enumSizedListOf size a
  | size == 0 = pure []
  | size > 0 = (:) <$> a <*> enumSizedListOf (size - 1) a
  | otherwise = panic $ "cannot enumerate a list of size < 0. size=" <> show size

-- | Create an enumerator for a non-empty list of elements
enumNonEmptyOf :: Enumerate a -> Enumerate (NonEmpty a)
enumNonEmptyOf a = pay ((:|) <$> a <*> enumListOf a)

-- | Create an enumerator for a Maybe
enumMaybeOf :: forall a . Enumerate a -> Enumerate (Maybe a)
enumMaybeOf enumA = pure Nothing <|> (Just <$> enumA)

-- | Create a default enumerator for a Either, choosing evenly between Left and Right
enumEitherOf :: forall a b . Enumerate a -> Enumerate b -> Enumerate (Either a b)
enumEitherOf enumA enumB = (Left <$> enumA) <|> (Right <$> enumB)

-- | Create a default enumerator for a small set of elements
enumSsetOf :: forall a . (Ord a) => Enumerate a -> Enumerate (Set a)
enumSsetOf = fmap Set.fromList . enumListOf

-- | Create a default enumerator for map of key/values
enumMapOf :: forall k v . (Ord k) => Enumerate k -> Enumerate v -> Enumerate (Map k v)
enumMapOf gk gv = Map.fromList <$> enumListOf (enumPairOf gk gv)

-- | Create a default enumerator for HashMap of key/values
enumHashMapOf :: forall k v . (Ord k, Hashable k) => Enumerate k -> Enumerate v -> Enumerate (HashMap k v)
enumHashMapOf gk gv = HashMap.fromList <$> enumListOf (enumPairOf gk gv)

-- | Create a default enumerator for a small non-empty map of elements
enumNonEmptyMapOf :: forall k v . (Ord k) => Enumerate k -> Enumerate v -> Enumerate (Map k v)
enumNonEmptyMapOf gk gv =
  (\h t -> Map.fromList (h : t)) <$> enumPairOf gk gv <*> enumListOf (enumPairOf gk gv)

-- * GENERATORS

enumToGen :: Enumerate a -> GenIO a
enumToGen a = liftGen $
  GenT $ \(Size size) seed ->
    if size <= 50 then
      pure (indexWith a (fromIntegral size))
    else
      runGenT (Size size) seed (uniformWith a size)


-- | Draw a value uniformly from an enumerate where the size is bounded
uniformWith :: Enumerate a -> Int -> Gen a
uniformWith = uni . parts where
  uni :: [Finite a] -> Int -> Gen a
  uni  []  _     =  Gen.discard
  uni  ps  maxp  =  let  (incl, rest)  = P.splitAt maxp ps
                         finite        = mconcat incl
    in  case fCard finite of
          0  -> uni rest 1
          _  -> do  i <- Gen.integral (Range.linear 0 (fCard finite - 1))
                    pure (fIndex finite i)

-- increasing :: Range
-- increasing = Range 0 (\r -> (r + 1, r + 1))
-- integral_' :: (MonadGen m, Integral a) => Range a -> m a
-- integral_' range =
--   generate $ \size seed ->
--     let
--       (x, y) =
--         Range.bounds size range
--     in
--       fromInteger . fst $
--         Seed.nextInteger (toInteger x) (toInteger y) seed
