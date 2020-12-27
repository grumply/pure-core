{-# LANGUAGE CPP, ExistentialQuantification, TypeFamilies, PatternSynonyms, ViewPatterns, ScopedTypeVariables, RankNTypes, DefaultSignatures, FlexibleContexts, FlexibleInstances, UndecidableInstances, RecordWildCards, BangPatterns, GADTs, MagicHash #-}
module Pure.Data.View where

-- from base
import Control.Concurrent (MVar)
import Control.Monad (void,join)
import Control.Monad.ST (ST)
import Data.Coerce (Coercible(),coerce)
import Data.IORef (IORef,readIORef)
import Data.Monoid (Monoid(..),(<>))
import Data.Proxy (Proxy(..))
import Data.STRef (STRef)
import Data.String (IsString(..))
import Data.Traversable (for)
import Data.Typeable (Typeable,tyConName,typeRepTyCon,typeOf,typeRepFingerprint,cast)
import Data.Unique (Unique,newUnique)
import GHC.Exts (reallyUnsafePtrEquality#,isTrue#)
import GHC.Fingerprint.Type (Fingerprint())
import GHC.Generics (Generic(..))
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

-- from pure-default
import Pure.Data.Default (Default(..))

-- from pure-json
import Pure.Data.JSON (ToJSON,FromJSON)

-- from pure-txt
import Pure.Data.Txt (FromTxt(..),ToTxt(..),Txt)

-- from pure-queue
import Pure.Data.Queue (Queue,arrive)

-- from pure-lifted
import Pure.Data.Lifted (Evt,Element,Node,IsNode(..),Text,Options)

-- from unordered-containers
import Data.Set (Set)
import Data.Map.Lazy (Map)

data Target = ElementTarget | WindowTarget | DocumentTarget deriving Eq

data Listener =
  On
    { eventName     :: Txt
    , eventTarget   :: Target
    , eventOptions  :: Options
    , eventAction   :: Evt -> IO ()
    , eventStopper  :: IO ()
    }

data Lifecycle =
  HostRef
    { withHost :: Node -> IO ()
    }

data Comp props state = Comp
  { initialize   :: state -> IO state
  , initialized  :: IO ()
  , construct    :: IO state
  , mount        :: state -> IO state
  , executing    :: state -> IO state
  , mounted      :: IO ()
  , receive      :: props -> state -> IO state
  , force        :: props -> state -> IO Bool
  , update       :: props -> state -> IO ()
  , render       :: props -> state -> View
  , updated      :: props -> state -> IO ()
  , unmounted    :: IO ()
  }

instance Default (Comp props state) where
  {-# INLINE def #-}
  def = Comp
    { construct   = return (error "Comp.construct: no initial state supplied.")
    , initialize  = return
    , initialized = return ()
    , mount       = return
    , executing   = return
    , mounted     = return ()
    , receive     = \_ -> return
    , force       = \_ _ -> return True
    , update      = \_ _ -> return ()
    , render      = \_ _ -> NullView Nothing
    , updated     = \_ _ -> return ()
    , unmounted   = return ()
    }

data ComponentPatch props state
  = Unmount (Maybe View) (IO ())
  | UpdateProperties props
  | UpdateState (props -> state -> IO (state,IO ()))

data Ref props state
  = Ref
      { crView       :: IORef View
      , crProps      :: IORef props
      , crState      :: IORef state
      , crComponent  :: IORef (Comp props state)
      , crPatchQueue :: IORef (Maybe (Queue (ComponentPatch props state)))
      }

data Features =
  Features_
       { classes    :: Set Txt
       , styles     :: Map Txt Txt
       , attributes :: Map Txt Txt
       , properties :: Map Txt Txt
       , listeners  :: [Listener]
       , lifecycles :: [Lifecycle]
       }

instance Monoid Features where
  {-# INLINE mempty #-}
  mempty = Features_ mempty mempty mempty mempty mempty mempty
#if !MIN_VERSION_base(4,11,0)
  {-# INLINE mappend #-}
  mappend (Features_ c1 s1 a1 p1 ls1 lc1) (Features_ c2 s2 a2 p2 ls2 lc2) =
    -- NOTE: mappending prefers the styles, attributes, and properties on the right
    Features_ (c1 <> c2) (s2 <> s1) (a2 <> a1) (p2 <> p1) (ls1 <> ls2) (lc1 <> lc2)
#else
instance Semigroup Features where
  {-# INLINE (<>) #-}
  (<>) (Features_ c1 s1 a1 p1 ls1 lc1) (Features_ c2 s2 a2 p2 ls2 lc2) =
    Features_ (c1 <> c2) (s2 <> s1) (a2 <> a1) (p2 <> p1) (ls1 <> ls2) (lc1 <> lc2)
#endif

instance Default Features where
  {-# INLINE def #-}
  def = mempty

data TypeWitness a = TypeWitness { unCompWitness :: Fingerprint }

witness :: forall a. Typeable a => TypeWitness a
witness = TypeWitness (typeRepFingerprint $ typeOf (undefined :: a))

sameTypeWitness :: TypeWitness a -> TypeWitness b -> Bool
sameTypeWitness (TypeWitness fp1) (TypeWitness fp2) =
  case reallyUnsafePtrEquality# fp1 fp2 of
    1# -> True
    _  -> fp1 == fp2

data View where
  HTMLView ::
       { elementHost :: Maybe Element
       , tag         :: Txt
       , features    :: {-# UNPACK #-}!Features
       , children    :: [View]
       } -> View

  TextView ::
        { textHost :: Maybe Text
        , content  :: Txt
        } -> View

  NullView ::
        { elementHost :: Maybe Element
        } -> View

  RawView ::
       { elementHost:: Maybe Element
       , tag        :: Txt
       , features   :: {-# UNPACK #-}!Features
       , content    :: Txt
       } -> View

  SVGView ::
       { elementHost :: Maybe Element
       , tag         :: Txt
       , features    :: {-# UNPACK #-}!Features
       , xlinks      :: !(Map Txt Txt)
       , children    :: [View]
       } -> View

  KHTMLView ::
       { elementHost   :: Maybe Element
       , tag           :: Txt
       , features      :: {-# UNPACK #-}!Features
       , keyedChildren :: [(Int,View)]
       } -> View

  KSVGView ::
       { elementHost   :: Maybe Element
       , tag           :: Txt
       , features      :: {-# UNPACK #-}!Features
       , xlinks        :: !(Map Txt Txt)
       , keyedChildren :: [(Int,View)]
       } -> View

  SomeView :: Pure a =>
       { renderable :: a
       } -> View

  LazyView ::
      { lazyFun :: a -> View
      , lazyArg :: a
      } -> View

  PortalView ::
      { portalProxy :: Maybe Element
      , portalDestination :: Element
      , portalView :: View
      } -> View

  ComponentView ::
       { __comp_witness :: TypeWitness (props,state)
       , record :: Maybe (Ref props state)
       , comp   :: Ref props state -> Comp props state
       , props  :: props
       } -> View

  TaggedView :: forall tag.
       { __tag :: TypeWitness tag 
       , taggedView :: View 
       } -> View
       
  Prebuilt :: 
      { prebuilt :: View 
      } -> View

instance Default View where
  {-# INLINE def #-}
  def = NullView Nothing

instance IsString View where
  {-# INLINE fromString #-}
  fromString = TextView Nothing . toTxt

instance FromTxt View where
  {-# INLINE fromTxt #-}
  fromTxt = TextView Nothing

asProxyOf :: a -> Proxy a
asProxyOf _ = Proxy

class Typeable a => Pure a where
  {-# NOINLINE __pure_witness #-}
  __pure_witness :: Proxy a -> TypeWitness a
  __pure_witness _ = witness
  view :: a -> View

instance Pure View where
  {-# INLINE view #-}
  view (SomeView a) = view a
  view a = a

{-# INLINE tyCon #-}
tyCon :: Typeable t => t -> String
tyCon = tyConName . typeRepTyCon . typeOf

class ToView a where
  toView :: a -> View

instance {-# OVERLAPPABLE #-} (Typeable a, Pure a) => ToView a where
  {-# INLINE toView #-}
  toView = View

instance {-# OVERLAPS #-} ToView View where
  {-# INLINE toView #-}
  toView = id

pattern View :: forall a. (Pure a, Typeable a) => a -> View
pattern View a <- (SomeView (cast -> Just a)) where
  View a = SomeView a

{-# INLINE get #-}
get :: Ref props state -> IO state
get = readIORef . crState

{-# INLINE ask #-}
ask :: Ref props state -> IO props
ask = readIORef . crProps

{-# INLINE look #-}
look :: Ref props state -> IO View
look = readIORef . crView

{-# INLINE modify #-}
modify :: Ref props state -> (props -> state -> state) -> IO Bool
modify r f = modifyM r (\p s -> return (f p s,return ()))

{-# INLINE modify_ #-}
modify_ :: Ref props state -> (props -> state -> state) -> IO ()
modify_ r f = void (modify r f)

{-# INLINE modifyM #-}
modifyM :: Ref props state -> (props -> state -> IO (state,IO ())) -> IO Bool
modifyM cr f = queueComponentUpdate cr (UpdateState f)

{-# INLINE modifyM_ #-}
modifyM_ :: Ref props state -> (props -> state -> IO (state,IO ())) -> IO ()
modifyM_ r f = void (modifyM r f)

{-# INLINE setProps #-}
setProps :: Ref props state -> props -> IO Bool
setProps cr = queueComponentUpdate cr . UpdateProperties

{-# INLINE queueComponentUpdate #-}
queueComponentUpdate :: Ref props state -> ComponentPatch props state -> IO Bool
queueComponentUpdate crec cp = do
  mq <- readIORef (crPatchQueue crec)
  case mq of
    Nothing -> return False
    Just q  -> do
      arrive q cp
      return True

{-# INLINE getHost #-}
getHost :: View -> Maybe Node
getHost ComponentView {..} = join $ for record (getHost . unsafePerformIO . readIORef . crView)
getHost TextView      {..} = fmap toNode textHost
getHost SomeView      {}   = Nothing
getHost LazyView      {}   = Nothing
getHost PortalView    {..} = fmap toNode portalProxy
getHost TaggedView    {..} = getHost taggedView
getHost Prebuilt      {..} = getHost prebuilt
getHost x                  = fmap toNode (elementHost x)
