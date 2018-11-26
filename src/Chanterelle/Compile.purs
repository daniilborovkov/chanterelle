module Chanterelle.Compile (compileProject) where

import Prelude
import Chanterelle.Project (loadProject)
import Chanterelle.Internal.Codegen (generatePS) as Chanterelle
import Chanterelle.Internal.Compile (compile) as Chanterelle
import Chanterelle.Internal.Logging (logCompileError)
import Chanterelle.Internal.Types.Compile (CompileError(..), runCompileM)
import Effect.Aff (launchAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Node.Process as P

compileProject
  :: Effect Unit
compileProject = do
    root <- liftEffect P.cwd
    void $ launchAff $ do
      proj <- try $ loadProject root
      case proj of
        Left err -> logCompileError $ MalformedProjectError (message err)
        Right project -> do
          eres <- flip runCompileM project $ do
            _ <- Chanterelle.compile
            Chanterelle.generatePS
          case eres of
            Right _ -> pure unit
            Left err -> logCompileError err
