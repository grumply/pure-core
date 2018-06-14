{-# LANGUAGE CPP, ExistentialQuantification, TypeFamilies, PatternSynonyms, ViewPatterns, ScopedTypeVariables, RankNTypes, DefaultSignatures, FlexibleContexts, FlexibleInstances, UndecidableInstances, RecordWildCards, BangPatterns, GADTs #-}
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
    { eventName     :: {-# UNPACK #-}!Txt
    , eventTarget   :: !Target
    , eventOptions  :: {-# UNPACK #-}!Options
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
      , executing    :: m ()
      , mounted      :: IO ()
      , receive      :: props -> state -> m state
      , force        :: props -> state -> m Bool
      , update       :: props -> state -> m ()
      , render       :: props -> state -> View
      , updated      :: props -> state -> m ()
      , unmounted    :: m ()
      }

instance Monad m => Default (Comp m props state) where
  {-# INLINE def #-}
  def =
    Comp
      { performIO   = unsafeCoerce id
      , execute     = unsafeCoerce id
      , construct   = return (error "Comp.construct: no initial state supplied.")
      , initialize  = return
      , initialized = return ()
      , mount       = return
      , executing   = return ()
      , mounted     = return ()
      , receive     = \_ -> return
      , force       = \_ _ -> return True
      , update      = \_ _ -> return ()
      , render      = \_ _ -> NullView Nothing
      , updated     = \_ _ -> return ()
      , unmounted   = return ()
      }

data ComponentPatch m props state
  = Unmount (Maybe View) (IO ())
  | UpdateProperties props
  | UpdateState (props -> state -> m (state,m ()))

data Ref m props state
  = Ref
      { crType       :: String
      , crView       :: {-# UNPACK #-}!(IORef View)
      , crProps      :: {-# UNPACK #-}!(IORef props)
      , crState      :: {-# UNPACK #-}!(IORef state)
      , crComponent  :: Comp m props state
      , crPatchQueue :: {-# UNPACK #-}!(IORef (Maybe (Queue (ComponentPatch m props state))))
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
  {-# INLINE mappend #-}
  mappend (Features_ c1 s1 a1 p1 ls1 lc1) (Features_ c2 s2 a2 p2 ls2 lc2) =
    -- NOTE: mappending prefers the styles, attributes, and properties on the right
    Features_ (c1 <> c2) (s2 <> s1) (a2 <> a1) (p2 <> p1) (ls1 <> ls2) (lc1 <> lc2)

instance Default Features where
  {-# INLINE def #-}
  def = mempty

data View where
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
       } -> View

  SomeView :: Pure a =>
       { name       :: String
       , renderable :: a
       } -> View

  PortalView ::
      { portalProxy :: Maybe Element
      , portalDestination :: Element
      , portalView :: View
      } -> View

  LazyView :: Pure b =>
      { lazyFun :: a -> b
      , lazyArg :: a
      } -> View

instance Default View where
  {-# INLINE def #-}
  def = NullView Nothing

instance IsString View where
  {-# INLINE fromString #-}
  fromString = TextView Nothing . toTxt

class Pure a where
  view :: a -> View

instance Pure View where
  {-# INLINE view #-}
  view (SomeView _ a) = view a
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

instance {-# OVERLAPS #-} Default (Features -> Features) where
  {-# INLINE def #-}
  def = id

instance {-# OVERLAPS #-} Default (View -> View) where
  {-# INLINE def #-}
  def = id

pattern View :: forall a. (Pure a, Typeable a) => a -> View
pattern View a <- (SomeView ((==) (tyCon (undefined :: a)) -> True) (unsafeCoerce -> a)) where
  View a = SomeView (tyCon (undefined :: a)) a

{-# INLINE get #-}
get :: Ref m props state -> IO state
get = readIORef . crState

{-# INLINE ask #-}
ask :: Ref m props state -> IO props
ask = readIORef . crProps

{-# INLINE look #-}
look :: Ref m props state -> IO View
look = readIORef . crView

{-# INLINE modify #-}
modify :: Monad m => Ref m props state -> (props -> state -> state) -> IO Bool
modify r f = modifyM r (\p s -> return (f p s,return ()))

{-# INLINE modify_ #-}
modify_ :: Monad m => Ref m props state -> (props -> state -> state) -> IO ()
modify_ r f = void (modify r f)

{-# INLINE modifyM #-}
modifyM :: Monad m => Ref m props state -> (props -> state -> m (state,m ())) -> IO Bool
modifyM cr f = queueComponentUpdate cr (UpdateState f)

{-# INLINE modifyM_ #-}
modifyM_ :: Monad m => Ref m props state -> (props -> state -> m (state,m ())) -> IO ()
modifyM_ r f = void (modifyM r f)

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
getHost ComponentView {..} = join $ for record (getHost . unsafePerformIO . readIORef . crView)
getHost TextView      {..} = fmap toNode textHost
getHost SomeView      {}   = Nothing
getHost PortalView    {..} = fmap toNode portalProxy
getHost x                  = fmap toNode (elementHost x)
