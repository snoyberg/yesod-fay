{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Utility functions for using Fay from a Yesod application.
--
-- This module is intended to be used from your Yesod application, not from
-- your Fay programs.
--
-- We assume a specific file structure, namely that there is a @fay@ folder
-- containing client-side code, and @fay-shared@ containing code to be used by
-- both the client and server.
--
-- The @Language.Fay.Yesod@ module (part of this package) is
-- required by both client and server code. However, since Fay does not
-- currently have package management support, we use a bit of a hack: the TH
-- calls in this package will automatically create the necessary
-- @fay\/Language\/Fay\/Yesod.hs@ file.  Ultimately, we will use a more elegant
-- solution.
--
-- In the future, if this package proves popular enough, Fay support will
-- likely be built into the scaffolding. In the meantime, you must manually
-- integrate it. In order to take advantage of this module, you should modify
-- your Yesod application as follows:
--
-- * Modify your @cabal@ file to include the @fay-shared@ folder when
--   compiling. This can be done by adding a @hs-source-dirs: ., fay-shared@ line
--   to your library section.
--
-- * Create the module @SharedTypes@ in @fay-shared@ and create a @Command@
--   datatype. For an example of what this file should look like, see
--   <https://github.com/snoyberg/yesod-fay/blob/master/sample/fay-shared/SharedTypes.hs>.
--
-- * Add the function @fayFile@ to your @Import@ module. See
--   <https://github.com/snoyberg/yesod-fay/blob/master/sample/Import.hs> for an
--   example.
--
-- * Add a new route at for the Fay subsite. Generally, it will look like
--   @/fay-command FaySiteR FaySite getFaySite@.
--
-- * Import the @SharedTypes@ and @Yesod.Fay@ modules into your @Foundation.hs@
--   module. Add an instance of @YesodFay@ for your application. You should set
--   the @YesodFayCommand@ associated type to the @Command@ datatype you created.
--   (You may also need to add a @YesodJquery@ instance.) Note that this instance
--   must appear after your @parseRoutes@. Set the method @fayRoute@ to
--   @FaySiteR@ (or whatever you called the subsite route), and implement
--   @yesodFayCommand@. It will generally look something like @yesodFayCommand
--   render command = case command of { ... }@.
--
-- * In order to use Fay, add @$(fayFile \"MyModule\")@ to a widget, and then
--   write the corresponding @fay/MyModule.hs@ file. For an example, see
--   <https://github.com/snoyberg/yesod-fay/blob/master/sample/fay/Home.hs>.
module Yesod.Fay
    ( -- * Typeclass
      YesodFay (..)
    , YesodFaySettings (..)
    , yesodFaySettings
      -- * Include Fay programs
    , fayFileProd
    , fayFileReload
    , FayFile
      -- * Commands
    , CommandHandler
    , Returns
      -- * Subsite
    , FaySite
    , getFaySite
      -- * Reexports
    , YesodJquery (..)
    ) where

import           Control.Applicative        ((<$>))
import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (decode, toJSON, Value)
import           Data.Aeson.Encode          (fromValue)
import qualified Data.ByteString.Lazy       as L
import           Data.Data                  (Data)
import           Data.Default               (def)
import           Data.Maybe                 (isNothing)
import           Data.Monoid                ((<>), mempty)
import           Data.Text                  (pack, unpack)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Text.Lazy.Builder     (fromText, toLazyText, Builder)
import           Filesystem                 (createTree, isFile, readTextFile)
import           Filesystem.Path.CurrentOS  (directory, encodeString, (</>), decodeString)
import           Fay                        (compileFile, getRuntime)
import           Fay.Convert                (readFromFay, showToFay)
import           Fay.Types                  (CompileConfig,
                                             configDirectoryIncludes,
                                             configTypecheck,
                                             configExportRuntime,
                                             configNaked)
import           Language.Fay.Yesod         (Returns (Returns))
import           Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL),
                                             Pred (ClassP), Q, Type (VarT),
                                             mkName, qAddDependentFile, qRunIO)
import           System.Directory           (copyFile)
import           System.Exit                (ExitCode (ExitSuccess))
import           System.Process             (rawSystem)
import           Text.Julius                (Javascript (Javascript), julius)
import           Yesod.Core                 (GHandler, GWidget,
                                             RenderRoute (..), RepJson, Yesod,
                                             YesodDispatch (..), addScript,
                                             addScriptEither, getUrlRender,
                                             getYesod, lift, lookupPostParam,
                                             mkYesodSub, parseRoutes,
                                             toMasterHandler, toWidget, toWidgetHead)
import           Yesod.Form.Jquery          (YesodJquery (..))
import           Yesod.Handler              (invalidArgs)
import           Yesod.Json                 (jsonToRepJson)
import           Yesod.Static

jsMainCall :: Bool -> String -> Builder
jsMainCall False _ = mempty
jsMainCall True mn' =
    "Fay$$_(" <> mn <> "$main);"
  where
    mn = fromText $ T.pack mn'

-- | Type class for applications using Fay.
--
-- We depend on @YesodJquery@ since the generated client-side code uses jQuery
-- for making Ajax calls. We have an associated type stating the command
-- datatype. Since this datatype must be used by both the client and server,
-- you should place its definition in the @fay-shared@ folder.
class YesodJquery master => YesodFay master where
    -- | User-defined function specifying how to respond to commands. Using the
    -- above datatype, this might look like:
    --
    -- > yesodFayCommand render command =
    -- >     case command of
    -- >         GetFib index r = render r $ fibs !! index
    yesodFayCommand :: CommandHandler master master

    -- | Where in the routing tree our Fay subsite is located. This is
    -- generally named @FaySiteR@, e.g.:
    --
    -- > mkYesod "YourSite" [parseRoutes
    -- > ...
    -- > /fay-command FaySiteR FaySite getFaySite
    -- > |]
    -- >
    -- > instance YesodFay YourSite where
    -- >     fayRoute = FaySiteR
    fayRoute :: Route FaySite -> Route master

-- | A function provided by the developer describing how to answer individual
-- commands from client-side code.
--
-- Due to restrictions of the type system in Fay, we use a relatively simple
-- approach for encoding the return type. In order to specify this, an extra
-- parameter- @Returns@- is passed around, with a phantom type variable stating
-- the expected return type.
--
-- The first argument to your function is the \"respond\" function: it takes
-- the extra @Returns@ parameter as well as the actual value to be returned,
-- and produces the expected result.
type CommandHandler sub master
    = forall s.
      (forall a. Show a => Returns a -> a -> GHandler sub master s)
   -> Value
   -> GHandler sub master s

-- | A setttings data type for indicating whether the generated Javascript
-- should contain a copy of the Fay runtime or not.
data YesodFaySettings = YesodFaySettings
    { yfsModuleName      :: String
    , yfsSeparateRuntime :: Maybe (FilePath, Exp)
    , yfsPostProcess     :: String -> IO String
    , yfsExternal        :: Maybe (FilePath, Exp)
    , yfsRequireJQuery   :: Bool
    }

yesodFaySettings :: String -> YesodFaySettings
yesodFaySettings moduleName = YesodFaySettings moduleName Nothing return Nothing True

updateRuntime :: FilePath -> IO ()
updateRuntime fp = getRuntime >>= \js -> createTree (directory $ decodeString fp) >> copyFile js fp

-- | The Fay subsite.
data FaySite = FaySite

mkYesodSub "FaySite"
    [ ClassP ''YesodFay [VarT $ mkName "master"]
    ] [parseRoutes|
/ FayCommandR POST
|]

-- | To be used from your routing declarations.
getFaySite :: a -> FaySite
getFaySite _ = FaySite

postFayCommandR :: YesodFay master => GHandler FaySite master RepJson
postFayCommandR =
    toSub $ runCommandHandler yesodFayCommand
  where
    toSub :: YesodFay master => GHandler master master a -> GHandler FaySite master a
    toSub mhandler = do
        m <- getYesod
        toMasterHandler id (const m) (fayRoute FayCommandR) mhandler

    -- | Run a command handler. This provides server-side responses to Fay queries.
    runCommandHandler :: YesodFay master
                      => CommandHandler sub master
                      -> GHandler sub master RepJson
    runCommandHandler f = do
        mtxt <- lookupPostParam "json"
        case mtxt of
            Nothing -> invalidArgs ["No JSON provided"]
            Just txt ->
                case decode (L.fromChunks [encodeUtf8 txt]) of
                    Nothing -> error $ "Unable to parse input: " ++ show txt
                    Just cmd -> f go cmd
      where
        go Returns = jsonToRepJson . showToFay

langYesodFay :: String
langYesodFay = $(qRunIO $ fmap (LitE . StringL . unpack) $ readTextFile "Language/Fay/Yesod.hs")

writeYesodFay :: IO ()
writeYesodFay = do
    let fp = "fay/Language/Fay/Yesod.hs"
        content = "-- NOTE: This file is auto-generated.\n" ++ langYesodFay
    exists <- isFile fp
    mcurrent <-
        if exists
            then fmap (Just . unpack) $ readTextFile fp
            else return Nothing
    unless (mcurrent == Just content) $ do
        createTree $ directory fp
        writeFile (encodeString fp) content

maybeRequireJQuery :: YesodFay master => Bool -> GWidget sub master ()
maybeRequireJQuery needJQuery = when needJQuery requireJQuery

requireJQuery :: YesodFay master => GWidget sub master ()
requireJQuery = do
    master <- lift getYesod
    addScriptEither $ urlJqueryJs master
    render <- lift getUrlRender
    -- FIXME get rid of toLazyText call below
    toWidgetHead [julius|window.yesodFayCommandPath = #{toJSON $ render $ fayRoute FayCommandR};|]

mkfp :: String -> FilePath
mkfp name = "fay/" ++ name ++ ".hs"

requireFayRuntime :: YesodFaySettings -> Q Exp
requireFayRuntime settings = do
    maybe (return ())
        (\(path,_) -> qRunIO $ updateRuntime (path ++ "/fay-runtime.js"))
        (yfsSeparateRuntime settings)
    case yfsSeparateRuntime settings of
        Nothing -> [| return () |]
        Just (_,exp) -> do
            hash <- qRunIO $ getRuntime >>= fmap base64md5 . L.readFile
            [| addScript ($(return exp) (StaticRoute ["fay-runtime.js"] [(T.pack hash, "")])) |]

-- | A function that takes a String giving the Fay module name, and returns an
-- TH splice that generates a @Widget@.
type FayFile = String -> Q Exp

-- | Does a full compile of the Fay code via GHC for type checking, and then
-- embeds the Fay-generated Javascript as a static string. File changes during
-- runtime will not be reflected.
fayFileProd :: YesodFaySettings -> Q Exp
fayFileProd settings = do
    let needJQuery = yfsRequireJQuery settings
    qAddDependentFile fp
    qRunIO writeYesodFay
    eres <- qRunIO $ compileFile config
        { configExportRuntime = exportRuntime
        , configNaked = not exportRuntime
        } fp
    case eres of
        Left e -> error $ "Unable to compile Fay module \"" ++ name ++ "\": " ++ show e
        Right s -> do
            s' <- qRunIO $ yfsPostProcess settings s
            let contents = fromText (pack s') <> jsMainCall (not exportRuntime) name
            external <-
                case yfsExternal settings of
                    Nothing -> return [|Nothing|]
                    Just (fp, exp) -> do
                        let name = concat ["faygen-", hash, ".js"]
                            hash = base64md5 contents'
                            contents' = TLE.encodeUtf8 $ toLazyText contents
                        qRunIO $ L.writeFile (concat [fp, "/", name]) contents'
                        return [| Just ($(return exp), name) |]

            [| do
                maybeRequireJQuery needJQuery
                $(requireFayRuntime settings)
                case $external of
                    Nothing -> toWidget $ const $ Javascript $ fromText $ pack
                               $(return $ LitE $ StringL $ TL.unpack $ toLazyText contents)
                    Just (constructor, name) -> addScript $ constructor $ StaticRoute [name] []
                |]
  where
    name = yfsModuleName settings
    exportRuntime = isNothing (yfsSeparateRuntime settings)
    fp = mkfp name

config :: CompileConfig
config = def
    { configDirectoryIncludes
        = (Nothing, "fay")
        : (Nothing, "fay-shared")
        : configDirectoryIncludes def
    }

-- | Performs no type checking on the Fay code. Each time the widget is
-- requested, the Fay code will be compiled from scratch to Javascript.
fayFileReload :: YesodFaySettings -> Q Exp
fayFileReload settings = do
    let needJQuery = yfsRequireJQuery settings
    qRunIO writeYesodFay
    [|
        liftIO (compileFile config
                { configTypecheck = False
                , configExportRuntime = exportRuntime
                , configNaked = not exportRuntime
                }
                $ mkfp name) >>= \eres -> do
        (case eres of
              Left e -> error $ "Unable to compile Fay module \"" ++ name ++ "\": " ++ show e
              Right s -> do
                maybeRequireJQuery needJQuery
                $(requireFayRuntime settings)
                toWidget (const $ Javascript $ fromText (pack s) <> jsMainCall (not exportRuntime) name))|]
  where
    name = yfsModuleName settings
    exportRuntime = isNothing (yfsSeparateRuntime settings)
