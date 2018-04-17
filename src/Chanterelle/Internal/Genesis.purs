module Chanterelle.Internal.Genesis where

import Prelude (class Functor, class Show, Unit, bind, discard, flip, otherwise, pure, show, void, ($), (&&), (<$>), (<<<), (<=), (<>), (==), (>>=))
import Chanterelle.Project (loadProject)
import Chanterelle.Internal.Compile (OutputContract(..), compileModuleWithoutWriting, decodeContract, makeSolcInput, resolveContractMainModule)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (ChanterelleModule(..), ChanterelleProject(..), ChanterelleProjectSpec(..), CompileError(..), GenesisAlloc(..), GenesisBlock(..), GenesisGenerationError(..), InjectableLibraryCode(..), Libraries(..), Library(..), insertGenesisAllocs, isFixedLibrary, logGenesisGenerationError, lookupGenesisAllocs, runCompileMExceptT)
import Chanterelle.Internal.Utils (jsonStringifyWithSpaces)
import Control.Error.Util (note)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM, randomRange)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Error.Class (try, throwError)
import Control.Monad.Except.Trans (ExceptT(..), except, runExceptT, withExceptT)
import Control.Monad.State.Trans (execStateT, get, put)
import Data.Argonaut as A
import Data.Array ((!!), replicate, null, all)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, elem)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.Traversable (for, sequence)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process (PROCESS)
import Node.Process as P
import Network.Ethereum.Web3 (Address, HexString, embed, mkAddress, mkHexString, unAddress, unHex)

substituteLibraryAddress :: HexString -> Address -> Either String HexString
substituteLibraryAddress hsBytecode target = ret
    where -- length of the asm PUSH20 <ownaddr>; ADDRESS; EQ
          -- which is part of the library's nonpayability guard
          -- so 27 bytes of opcodes *2 cause it's hex
          minBytecodeLength = 46

          -- evm opcodes
          op_push20'     = "73"
          op_push20 addr = op_push20' <> (unHex $ unAddress addr)
          op_addr        = "30"
          op_eq          = "14"

          -- work against the raw hex string
          bytecode = unHex hsBytecode

          -- split at minBytecodeLength since we have to edit before that
          bcsplit = case S.splitAt minBytecodeLength bytecode of
                      Nothing -> { preamble: "", code: "" }
                      Just s  -> { preamble: s.before, code: s.after }

          -- is the first instruction a PUSH20?
          firstByteIsPush20 = S.take 2 bcsplit.preamble == op_push20'

          -- are the last instructions in preamble ADDRESS; EQ?
          preambleEndsInAddrEq = S.takeRight 4 bcsplit.preamble == (op_addr <> op_eq)

          -- if yes to both then it's a library
          firstBytesAreLibPreamble = firstByteIsPush20 && preambleEndsInAddrEq

          newPreamble = (op_push20 target) <> op_addr <> op_eq

          ret | S.length bytecode <= minBytecodeLength = Left "Bytecode too short to be a library"
              | firstBytesAreLibPreamble == false      = Left "Bytecode does not look like a library"
              | otherwise                              = note "Couldn't make a valid HexString" $ mkHexString (newPreamble <> bcsplit.code)

-- this might actually be useless in retrospect
generateAddress :: forall eff m f
                 . MonadEff (random :: RANDOM | eff) m
                => Foldable f
                => f Address
                -> m Address
generateAddress blacklist = do
  addr' <- randomAddress
  case mkHexAddress addr' of
    Nothing -> do
      log Debug $ "Generated address " <> show addr' <> " which isn't actually a valid address"
      generateAddress blacklist
    Just addr -> if addr `elem` blacklist
                   then do
                     log Debug $ "Generated address " <> show addr <> " which is blacklisted, retrying"
                     generateAddress blacklist
                   else do
                     log Debug $ "Successfully generated address " <> show addr
                     pure addr
  where randomHexDigit = toHex <<< floor <$> liftEff (randomRange (toNumber 0) (toNumber 16)) -- 0 inclusive, 16 exclusive, aka 0-F
        hexDigits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']
        randomAddress = S.fromCharArray <$> (sequence $ replicate 40 randomHexDigit)
        toHex n = fromMaybe '0' $ hexDigits !! n
        mkHexAddress s = mkHexString s >>= mkAddress

generateGenesis :: forall eff m
                 . MonadAff (fs :: FS, console :: CONSOLE, now :: NOW, process :: PROCESS | eff) m
                => ChanterelleProject
                -> FilePath
                -> m (Either GenesisGenerationError GenesisBlock)
generateGenesis cp@(ChanterelleProject project) genesisIn = liftAff <<< runExceptT $
  if nothingToDo
    then throwError $ NothingToDo ntdReason
    else do
      genesis <- loadGenesisIn
      injects <- for libs $ case _ of
        FixedLibrary { name } -> throwError $ CouldntInjectLibrary name "is a fixed library without any source code" -- we should eventually try fetching this from network!
        InjectableLibrary { name, address, code } -> case code of
            InjectableWithBytecode bc -> do
                injectedBytecode <- withExceptT (CouldntInjectLibraryAddress name) <<< except $ substituteLibraryAddress bc address
                pure { name, address, injectedBytecode }
            InjectableWithSourceCode r f -> do
                let libProject = cp
                rawBytecode <- withExceptT (CouldntCompileLibrary name) <<< flip runCompileMExceptT libProject $ do
                    let mfi@(ChanterelleModule mfi') = moduleForInput { name, root: r, filePath: f }
                    input <-  makeSolcInput name f
                    output <- compileModuleWithoutWriting mfi input
                    decoded <- decodeContract name output
                    OutputContract { deployedBytecode } <- resolveContractMainModule f decoded mfi'.solContractName
                    pure deployedBytecode
                hexBytecode <- withExceptT (CouldntCompileLibrary name <<< UnexpectedSolcOutput) <<< except $ note "Solc somehow gave us invalid hex" (mkHexString rawBytecode)
                injectedBytecode <- withExceptT (CouldntInjectLibraryAddress name) <<< except $ substituteLibraryAddress hexBytecode address
                pure { name, address, injectedBytecode }
      injectedGenesis <- flip execStateT genesis <<< for injects $ \{ name, address, injectedBytecode } -> do
        (GenesisBlock currGen) <- get
        newAllocs <- case lookupGenesisAllocs address currGen.allocs of
            Nothing -> pure $ insertGenesisAllocs address (GenesisAlloc { code: Just injectedBytecode, storage: Nothing, balance: embed 0 }) currGen.allocs
            Just (GenesisAlloc existingAlloc) -> if existingAlloc.code == Just injectedBytecode
                                   then pure currGen.allocs
                                   else throwError $ CouldntInjectLibraryAddress name $ "Genesis block already contains an entry for address " <> show address <> " with different bytecode"
        put $ GenesisBlock currGen { allocs = newAllocs }
      pure injectedGenesis

  where (ChanterelleProjectSpec spec) = project.spec
        (Libraries libs)              = spec.libraries

        moduleForInput {name, root, filePath } = 
            let { root, dir, base, ext, name } = Path.parse filePath
             in ChanterelleModule { moduleName: name, solContractName: name, solPath: filePath, jsonPath: "", pursPath: "" }

        { nothingToDo, ntdReason } = if null libs
                                          then { nothingToDo: true, ntdReason: "No libraries specified in project" }
                                          else if all isFixedLibrary libs
                                                 then { nothingToDo: true, ntdReason: "All libraries are fixed libraries" }
                                                 else { nothingToDo: false, ntdReason: "There's stuff to do!" }
        
        wrapLoadFailure :: forall e m' a. Show e => Functor m' => m' (Either e a) -> ExceptT GenesisGenerationError m' a
        wrapLoadFailure = withExceptT (CouldntLoadGenesisBlock genesisIn <<< show) <<< ExceptT
        
        loadGenesisIn = do
            genTxt <- wrapLoadFailure (try $ FS.readTextFile UTF8 genesisIn)
            wrapLoadFailure (pure $ A.jsonParser genTxt >>= A.decodeJson)

runGenesisGenerator :: forall e. FilePath -> FilePath -> Eff (console :: CONSOLE, fs :: FS, now :: NOW, process :: PROCESS | e) Unit 
runGenesisGenerator genesisIn genesisOut = do
    root <- liftEff P.cwd
    void <<< launchAff $
      (try $ loadProject root) >>= case _ of
        Left err -> liftAff <<< logGenesisGenerationError $ MalformedProjectErrorG (message err)
        Right project -> (liftAff $ generateGenesis project genesisIn) >>= case _ of
            Right gb -> do
                let strungGb = jsonStringifyWithSpaces 4 (A.encodeJson gb)
                try (FS.writeTextFile UTF8 genesisOut strungGb) >>= case _ of
                    Left err -> log Error $ "Couldn't write genesis block to " <> show genesisOut <> ": " <> show err
                    Right _  -> log Info $ "Successfully wrote generated genesis block to " <> show genesisOut
            Left err -> liftAff $ logGenesisGenerationError err