{-# LANGUAGE CPP, ExistentialQuantification, TypeFamilies, PatternSynonyms, ViewPatterns, ScopedTypeVariables, RankNTypes, DefaultSignatures, FlexibleContexts, GADTs #-}
module Pure.Data.View where

-- base
import Control.Concurrent (MVar)
import Control.Monad.ST (ST)
import Data.Coerce (Coercible(),coerce)
import Data.IORef (IORef)
import Data.Proxy (Proxy(..))
import Data.STRef (STRef)
import Data.String (IsString(..))
import Data.Typeable (Typeable,tyConName,typeRepTyCon,typeOf)
import GHC.Generics (Generic(..))
import Unsafe.Coerce (unsafeCoerce)

-- containers
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Set (Set)

-- pure-default
import Pure.Data.Default (Default(..))

-- pure-json
import Pure.Data.JSON (ToJSON,FromJSON)

-- pure-txt
import Pure.Data.Txt (FromTxt(..),ToTxt(..),Txt)

-- pure-queue
import Pure.Data.Queue (Queue)

-- from ghcjs-base
#ifdef __GHCJS__
import GHCJS.Marshal.Pure
import GHCJS.Types

type JSV = JSVal
#else
type JSV = ()
#endif

newtype Win     = Win JSV
newtype Doc     = Doc JSV
newtype Head    = Head JSV
newtype Body    = Body JSV
newtype Element = Element JSV
newtype Text    = Text JSV
newtype Node    = Node JSV
newtype Frag    = Frag JSV

newtype History = History JSV
newtype Loc     = Loc JSV

toJSV :: Coercible a JSV => a -> JSV
toJSV = coerce

class IsNode e where
  toNode :: e -> Node
  default toNode :: Coercible e Node => e -> Node
  toNode = coerce
instance IsNode Node where
  toNode = id
instance IsNode Body
instance IsNode Head
instance IsNode Element
instance IsNode Text
instance IsNode Frag
instance IsNode Doc

data Options = Options
  { preventDef :: Bool
  , stopProp   :: Bool
  , passive    :: Bool
  } deriving (Eq,Show)
instance Default Options where
  def = Options False False True

data Evt = Evt
  { evtObj            :: JSV
  , evtTarget         :: JSV
  , evtRemoveListener :: IO ()
  }

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

data Comp (m :: * -> *) props state = Monad m =>
    Comp
      { performIO    :: forall a. IO a ->  m a
      , execute      :: forall a.  m a -> IO a
      , initialize   :: state -> IO state
      , initialized  :: IO ()
      , construct    :: IO state
      , mount        :: state -> IO state
      , mounted      :: IO ()
      , receive      :: props -> state -> m state
      , force        :: props -> state -> m Bool
      , update       :: props -> state -> m ()
      , render       :: props -> state -> View
      , updated      :: props -> state -> View -> m ()
      , unmount      :: m ()
      , unmounted    :: m ()
      }

instance Monad m => Default (Comp m props state) where
  def =
    Comp
      { performIO   = error "Component.performIO: no lifter specified"
      , execute     = error "Component.execute: no executor specified"
      , construct   = return (error "Component.construct: no initial state.")
      , initialize  = return
      , initialized = return ()
      , mount       = return
      , mounted     = return ()
      , receive     = \_ -> return
      , force       = \_ _ -> return True
      , update      = \_ _ -> return ()
      , render      = \_ _ -> NullView Nothing
      , updated     = \_ _ _ -> return ()
      , unmount     = return ()
      , unmounted   = return ()
      }

data ComponentPatch m props state
  = forall s. Unmount (STRef s [IO ()] -> View -> ST s ()) (MVar (IO ()))
  | UpdateProperties props
  | UpdateState (props -> state -> m (state,m ()))

data Ref m props state
  = Ref
      { crType       :: String
      , crView       :: (IORef View)
      , crProps      :: (IORef props)
      , crState      :: (IORef state)
      , crComponent  :: Comp m props state
      , crPatchQueue :: (IORef (Maybe (Queue (ComponentPatch m props state))))
      }

data Features =
  Features
       { classes    :: Set Txt
       , styles     :: Map Txt Txt
       , attributes :: Map Txt Txt
       , properties :: Map Txt Txt
       , listeners  :: [Listener]
       , lifecycles :: [Lifecycle]
       }

data View where
  -- NullView must have a presence on the page for proper diffing
  NullView ::
        { elementHost :: Maybe Element
        } -> View

  TextView ::
        { textHost :: Maybe Text
        , content  :: Txt
        } -> View

  RawView ::
       { elementHost:: Maybe Element
       , tag        :: Txt
       , features   :: Features
       , content    :: Txt
       } -> View

  HTMLView ::
       { elementHost :: Maybe Element
       , tag         :: Txt
       , features    :: Features
       , children    :: [View]
       } -> View

  KHTMLView ::
       { elementHost   :: Maybe Element
       , tag           :: Txt
       , features      :: Features
       , keyedChildren :: [(Int,View)]
       , childMap      :: IntMap View
       } -> View

  ComponentView ::
       { name   :: String
       , props  :: props
       , record :: Maybe (Ref m props state)
       , comp   :: Ref m props state -> Comp m props state
       } -> View

  SVGView ::
       { elementHost :: Maybe Element
       , tag         :: Txt
       , features    :: Features
       , xlinks      :: Map Txt Txt
       , children    :: [View]
       } -> View

  KSVGView ::
       { elementHost   :: Maybe Element
       , tag           :: Txt
       , features      :: Features
       , xlinks        :: Map Txt Txt
       , keyedChildren :: [(Int,View)]
       , childMap      :: IntMap View
       } -> View

  SomeView :: Pure a =>
       { name       :: String -- Typeable is too expensive here because of the weight of m
       , renderable :: a
       } -> View

  -- StaticView
  --   :: { staticView :: View ms } -> View ms

instance Default View where
  def = NullView Nothing

instance IsString View where
  fromString = TextView Nothing . fromString

instance FromTxt View where
  fromTxt = TextView Nothing

class Pure a where
  view :: a -> View

instance Pure View where
  view (SomeView _ a) = view a
  view a = a

tyCon :: Typeable t => t -> String
tyCon = tyConName . typeRepTyCon . typeOf

pattern View :: forall a. (Pure a, Typeable a) => a -> View
pattern View a <- (SomeView ((==) (tyCon (undefined :: a)) -> True) (unsafeCoerce -> a)) where
  View a = SomeView (tyCon (undefined :: a)) a

type Plan s = STRef s [IO ()]
type DiffIO   a = a -> a -> a -> IO   a
type DiffST s a = a -> a -> a -> ST s a

