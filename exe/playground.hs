import Control.Monad
import Control.Monad.State
import Data.Constraint
import Data.Function
import Data.Functor
import Data.List
import GHC hiding (NotQualified, runGhc)
import GHC.Base hiding (Type, withDict)
import GHC.Core.TyCo.Tidy (tidyOpenType)
import GHC.Data.Bag (bagToList)
import GHC.Driver.Env (hsc_units)
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Driver.Ppr (showSDocForUser)
import GHC.IO.FD ()
import GHC.Tc.Errors.Types (
  SolverReportWithCtxt (..),
  TcRnMessage (..),
  TcRnMessageDetailed (..),
  TcSolverReportMsg (..),
 )
import GHC.Tc.Types.Constraint (Hole (..))
import GHC.Types.Error (errMsgDiagnostic, getMessages)
import GHC.Types.SourceError (srcErrorMessages)
import GHC.Types.Var.Env (emptyTidyEnv)
import GHC.Utils.Outputable (defaultSDocContext, ppr, renderWithContext)
import Language.Haskell.Interpreter hiding (get)
import Language.Haskell.Interpreter.Unsafe (unsafeInterpret)
import System.IO

main :: IO ()
main =
  print =<< runInterpreter do
    set [languageExtensions := [UnknownExtension "GHC2024"]]
    setImportsF
      [ ModuleImport "Prelude" NotQualified NoImportList
      , ModuleImport "Data.Functor" NotQualified NoImportList
      , ModuleImport "Control.Monad.Identity" NotQualified NoImportList
      , ModuleImport "Control.Monad.State" NotQualified NoImportList
      , ModuleImport "Control.Monad.Reader" NotQualified NoImportList
      , ModuleImport "Language.Haskell.Interpreter" NotQualified (ImportList ["InterpreterT", "MonadInterpreter (..)"])
      , ModuleImport "Data.Constraint" NotQualified NoImportList
      ]

    void $
      runGhc $
        runDecls $
          unlines
            [ "forceVoid :: m () -> m (); forceVoid  = id"
            , "instance (MonadInterpreter m) => MonadInterpreter (StateT s m) where"
            , "  runGhc x = lift $ runGhc x"
            , "  fromSession x = lift $ fromSession x"
            , "  modifySessionRef x y = lift $ modifySessionRef x y"
            , "instance (MonadInterpreter m) => MonadInterpreter (ReaderT r m) where"
            , "  runGhc x = lift $ runGhc x"
            , "  fromSession x = lift $ fromSession x"
            , "  modifySessionRef x y = lift $ modifySessionRef x y"
            ]

    liftIO $ hSetBuffering stdout NoBuffering
    let session :: forall m. (MonadInterpreter m) => String -> m ()
        session monad_str = fix $ \loop -> do
          liftIO $ putStr "> "
          line <- liftIO getLine

          case line of
            ":q" -> pure ()
            ":print-monad" -> do
              liftIO $ putStrLn monad_str
              loop
            line
              | ":e " `isPrefixOf` line -> do
                  let line' = "(" <> drop 3 line <> ")"
                      doExp = ("(" <> line' <> " (forceVoid _a)) :: " <> monad_str <> " _b")
                  holes <- runGhc $ inferHoles doExp

                  let (_, b_type) : (_, a_type) : _ = holes

                  let insideActionType = monadOf $ renderWithContext defaultSDocContext (ppr a_type)

                  run :: (n () -> m ()) <-
                    unsafeInterpret
                      ("\\m -> void (" <> line' <> " m)")
                      (actionOf insideActionType <> " -> " <> actionOf monad_str)

                  dict :: Dict (MonadInterpreter n) <- unsafeInterpret ("Dict") ("Dict (MonadInterpreter (" <> insideActionType <> "))")

                  run (withDict dict $ (session $ insideActionType))
                  loop
              | otherwise -> do
                  void . join $ unsafeInterpret (line <> " >>= liftIO . print") (actionOf monad_str)
                  loop
    session "InterpreterT IO"

showTypeQualified :: (GhcMonad m) => Type -> m String
showTypeQualified ty = do
  df <- getSessionDynFlags
  units <- hsc_units <$> getSession
  pure $ showSDocForUser df units alwaysQualify (ppr ty)

inferHoles :: (GhcMonad m) => String -> m [(RdrName, Type)]
inferHoles expr =
  handleSourceError (pure . holesOf) $ exprType TM_Inst expr $> []
 where
  holesOf err =
    [ (hole_occ h, snd (tidyOpenType emptyTidyEnv (hole_ty h)))
    | GhcTcRnMessage msg <- errMsgDiagnostic <$> bagToList (getMessages (srcErrorMessages err))
    , ReportHoleError h _ <- solverReports msg
    ]
  solverReports = \case
    TcRnMessageWithInfo _ (TcRnMessageDetailed _ msg) -> solverReports msg
    TcRnWithHsDocContext _ msg -> solverReports msg
    TcRnSolverReport rpt _ -> [reportContent rpt]
    _ -> []

dropEnd n xs = take (length xs - n) xs

monadOf action_str =
  assert (drop (length action_str - 3) action_str == " ()") $
    dropEnd 3 action_str
actionOf monad_str = monad_str <> " ()"
