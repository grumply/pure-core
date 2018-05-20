{-# LANGUAGE CPP, ExistentialQuantification, TypeFamilies, PatternSynonyms, ViewPatterns, ScopedTypeVariables, RankNTypes, DefaultSignatures, FlexibleContexts, FlexibleInstances, UndecidableInstances, RecordWildCards, BangPatterns #-}
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
import Data.Typeable (Typeable,tyConName,typeRepTyCon,typeOf)
import GHC.Generics (Generic(..))
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

-- from containers
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Set (Set)

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
  = forall s. Unmount (STRef s [IO ()] -> STRef s [IO ()] -> View -> ST s ()) (MVar (IO ()))
  | UpdateProperties props
  | UpdateState (props -> state -> m (state,m ()))

data Ref m props state
  = Ref
      { crType       :: String
      , crView       :: (IORef View)
      , crProps      :: (IORef props)
      , crState      :: (IORef state)
      , crComponent  :: (Comp m props state)
      , crPatchQueue :: (IORef (Maybe (Queue (ComponentPatch m props state))))
      }

data Features =
  Features_
       { classes    :: (Set Txt)
       , styles     :: (Map Txt Txt)
       , attributes :: (Map Txt Txt)
       , properties :: (Map Txt Txt)
       , listeners  :: [Listener]
       , lifecycles :: [Lifecycle]
       }

instance Monoid Features where
  mempty = Features_ mempty mempty mempty mempty mempty mempty
  mappend (Features_ c1 s1 a1 p1 ls1 lc1) (Features_ c2 s2 a2 p2 ls2 lc2) =
    -- NOTE: mappending prefers the styles, attributes, and properties on the right
    Features_ (c1 <> c2) (s2 <> s1) (a2 <> a1) (p2 <> p1) (ls1 <> ls2) (lc1 <> lc2)

instance Default Features where
  def = mempty

data View
  = NullView
        { elementHost :: (Maybe Element)
        }

  | TextView
        { textHost :: (Maybe Text)
        , content  :: Txt
        }

  | RawView
       { elementHost:: (Maybe Element)
       , tag        :: Txt
       , features   :: Features
       , content    :: Txt
       }

  | HTMLView
       { elementHost :: (Maybe Element)
       , tag         :: Txt
       , features    :: Features
       , children    :: [View]
       }

  | KHTMLView
       { elementHost   :: (Maybe Element)
       , tag           :: Txt
       , features      :: Features
       , keyedChildren :: [(Int,View)]
       , childMap      :: (IntMap View)
       }

  | forall m props state. ComponentView
       { name   :: String
       , props  :: props
       , record :: (Maybe (Ref m props state))
       , comp   :: (Ref m props state -> Comp m props state)
       }

  | SVGView
       { elementHost :: (Maybe Element)
       , tag         :: Txt
       , features    :: Features
       , xlinks      :: (Map Txt Txt)
       , children    :: [View]
       }

  | KSVGView
       { elementHost   :: (Maybe Element)
       , tag           :: Txt
       , features      :: Features
       , xlinks        :: (Map Txt Txt)
       , keyedChildren :: [(Int,View)]
       , childMap      :: (IntMap View)
       }

  | forall a. Pure a => SomeView
       { name       :: String
       , renderable :: a
       }

  | PortalView
      { portalDestination :: Element
      , portalView :: View
      }

instance Default View where
  def = NullView Nothing

instance IsString View where
  fromString s = fromTxt $ fromString s

instance IsString [View] where
  fromString s = [ fromString s ]

instance FromTxt View where
  fromTxt t = TextView Nothing t

instance FromTxt [View] where
  fromTxt t = [ fromTxt t ]

class Pure a where
  view :: a -> View

instance Pure View where
  view (SomeView _ a) = view a
  view a = a

{-# INLINE tyCon #-}
tyCon :: Typeable t => t -> String
tyCon = tyConName . typeRepTyCon . typeOf

class ToView a where
  toView :: a -> View

instance {-# OVERLAPPABLE #-} (Typeable a, Pure a) => ToView a where
  toView = View

instance {-# OVERLAPS #-}ToView View where
  toView = id

pattern View :: forall a. (Pure a, Typeable a) => a -> View
pattern View a <- (SomeView ((==) (tyCon (undefined :: a)) -> True) (unsafeCoerce -> a)) where
  View a = SomeView (tyCon (undefined :: a)) a

{-# INLINE getState #-}
getState :: Ref m props state -> IO state
getState = readIORef . crState

{-# INLINE getProps #-}
getProps :: Ref m props state -> IO props
getProps = readIORef . crProps

{-# INLINE getView #-}
getView :: Ref m props state -> IO View
getView = readIORef . crView

{-# INLINE setStatePure #-}-- TODO: consider optimizing for this case?
setStatePure :: Monad m => Ref m props state -> (props -> state -> state) -> IO Bool
setStatePure r f = setState r (\p s -> return (f p s,return ()))

{-# INLINE setStatePure_ #-}
setStatePure_ :: Monad m => Ref m props state -> (props -> state -> state) -> IO ()
setStatePure_ r f = void (setStatePure r f)

{-# INLINE setState #-}
setState :: Ref m props state -> (props -> state -> m (state,m ())) -> IO Bool
setState cr = queueComponentUpdate cr . UpdateState

{-# INLINE setState_ #-}
setState_ :: Ref m props state -> (props -> state -> m (state,m ())) -> IO ()
setState_ r f = void (setState r f)

{-# INLINE setProps #-}
setProps :: Ref m props state -> props -> IO Bool
setProps cr = queueComponentUpdate cr . UpdateProperties

{-# INLINE queueComponentUpdate #-}
queueComponentUpdate :: Ref m props state -> ComponentPatch m props state -> IO Bool
queueComponentUpdate crec cp = do
  mq <- readIORef (crPatchQueue crec)
  case mq of
    Nothing -> return False
    Just q  -> do
      arrive q cp
      return True

{-# INLINE getHost #-}
getHost :: View -> Maybe Node
-- EEK
getHost ComponentView {..} = join $ for record (getHost . unsafePerformIO . readIORef . crView)
getHost TextView  {..} = fmap toNode textHost
getHost SomeView {}    = Nothing
getHost PortalView {..} = getHost portalView
getHost x              = fmap toNode $ elementHost x
