{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Evolution.SamplerTable (buildTree, buildTable, SamplerTable, TermsAndOps (..), typesToArgMap, instantiateArgs) where

import Control.Monad (guard)
import qualified Control.Monad.State.Strict as State
import Data.Bifunctor
import Data.Bitraversable (bisequence)
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe, maybeToList)
import qualified Data.Set as S
import Evolution.Core (Depth, St)
import Evolution.Helpers (sample)
import Grammar
import qualified System.Random as R

data TermsAndOps = MkTermsAndOps
  { _arguments :: !(M.Map OutputType [Int]),
    _literals :: !(M.Map OutputType [St Lit]),
    _operations :: !(M.Map OutputType [Operation])
  }

data UsesInput = UsesInput | DoesNotUseInput

instance Semigroup UsesInput where
  (<>) :: UsesInput -> UsesInput -> UsesInput
  DoesNotUseInput <> DoesNotUseInput = DoesNotUseInput
  _ <> _ = UsesInput

instance Monoid UsesInput where
  mempty :: UsesInput
  mempty = DoesNotUseInput

type SamplerTableRow = (UsesInput, [Either (St Terminal) (St (Operation, FunctionType))])

type SamplerTable = M.Map (Depth, OutputType) SamplerTableRow

maxLambdaDepthAtEachLevel :: [Depth]
maxLambdaDepthAtEachLevel = [3, 0]

maxLambdaLevel :: Int
maxLambdaLevel = length maxLambdaDepthAtEachLevel

buildTree :: SamplerTable -> Depth -> OutputType -> St Tree
buildTree _ depth _ | depth < 0 = error "Negative depth!"
buildTree table depth outputType = do
  node <- sample $ snd $ table M.! (depth, outputType)
  case node of
    Left stTerm -> mkLeaf <$> stTerm
    Right st -> do
      (op, fType) <- st
      args <- sequence $ buildTree table (depth - 1) <$> _argTypes fType
      return $ op <| args

buildTable :: TermsAndOps -> Depth -> Bool -> SamplerTable
buildTable = buildTableForLevel 0

buildTableForLevel :: Int -> TermsAndOps -> Depth -> Bool -> SamplerTable
buildTableForLevel currentLambdaLevel (MkTermsAndOps args literals ops) maxDepth forceFullTree = mapping
  where
    maxLambdaDepth = maxLambdaDepthAtEachLevel !! currentLambdaLevel
    terms, argMap, litMap :: M.Map OutputType (UsesInput, [St Terminal])
    argMap = M.map ((\x -> (if null x then DoesNotUseInput else UsesInput, x)) . fmap (return . Arg)) args
    litMap = M.map ((DoesNotUseInput,) . fmap (fmap Literal)) literals
    terms = M.unionWith (<>) argMap litMap
    relevantTypes = S.toList $ S.union (M.keysSet terms) (M.keysSet ops)
    mapping :: SamplerTable
    mapping =
      M.fromList $ do
        depth <- [0 .. maxDepth]
        outputType <- relevantTypes
        return ((depth, outputType), buildTableRow (depth, outputType))

    lambdaMappings :: M.Map FunctionType SamplerTable
    lambdaMappings = M.fromList $ do
      typeCandidate <- relevantTypes
      case typeCandidate of
        GLambda ft ->
          -- if the level is zero, only id is an option, so it must be a -> a
          if maxLambdaDepth == 0 && head (_argTypes ft) /= _outType ft
            then []
            else
              let args = typesToArgMap (_argTypes ft)
                  table = buildTableForLevel (currentLambdaLevel + 1) (MkTermsAndOps args literals ops) maxLambdaDepth False
               in case table M.!? (maxLambdaDepth, _outType ft) of
                    Nothing -> []
                    Just (DoesNotUseInput, _) -> []
                    Just (_, []) -> []
                    Just _ -> [(ft, table)]
        _ -> []

    buildTableRow :: (Depth, OutputType) -> (UsesInput, [Either (St Terminal) (St (Operation, FunctionType))])
    -- a special case is made for generating lambdas
    buildTableRow (0, GLambda ft)
      | currentLambdaLevel == maxLambdaLevel = mempty
      | otherwise = (mempty, maybeToList $ Left . generateLambda maxLambdaDepth ft <$> lambdaMappings M.!? ft)
    buildTableRow (0, outType) = maybe mempty (fmap (fmap Left)) (terms M.!? outType)
    buildTableRow (depth, outType) = terminals <> operations
      where
        getMapping (depth, t) = fromMaybe mempty (mapping M.!? (depth, t))
        terminals, operations :: (UsesInput, [Either (St Terminal) (St (Operation, FunctionType))])
        terminals
          | forceFullTree = mempty
          | otherwise = getMapping (0, outType)
        operations =
          bimap mconcat (Right <$>) $
            unzip $ do
              op <- fromMaybe [] $ ops M.!? outType
              let possibleArgs = instantiateArgs relevantTypes outType op
                  rowsToCheck :: Depth -> GType -> [SamplerTableRow]
                  rowsToCheck mDepth t
                    | forceFullTree = [getMapping (mDepth, t)] -- if we're forcing a full, the child tree must have mDepth
                    | otherwise = [getMapping (d, t) | d <- [0 .. mDepth]] -- if it's not full, check every depth from here below
                  isMappingEmpty :: Depth -> GType -> Bool
                  isMappingEmpty mDepth t = all (null . snd) $ rowsToCheck mDepth t

                  areArgsValid :: ArgTypes -> Bool
                  areArgsValid argTypes = not (any (isMappingEmpty (depth - 1)) argTypes)

                  mappingUsesInput :: Depth -> GType -> UsesInput
                  mappingUsesInput mDepth t = foldMap fst $ rowsToCheck mDepth t

                  doArgsUseInput :: ArgTypes -> UsesInput
                  doArgsUseInput argTypes = foldMap (mappingUsesInput (depth - 1)) argTypes

                  validArgs = filter areArgsValid possibleArgs
                  build :: St ArgTypes -> St (Operation, FunctionType)
                  build = fmap (\arg -> (op, arg ->> outType))
              guard $ (not . null) validArgs
              return (foldMap doArgsUseInput validArgs, build $ sample validArgs)

generateLambda :: Depth -> FunctionType -> SamplerTable -> St Terminal
generateLambda maxDepth ft table = do
  tree <- buildTree table maxDepth $ _outType ft
  if hasInput tree -- always generate lambdas that use their inputs
    then return $ Literal $ LambdaLit $ MkTypedTree tree ft
    else generateLambda maxDepth ft table
  where
    hasInput :: Tree -> Bool
    hasInput (LeafArg _) = True
    hasInput (LeafLit _) = False
    hasInput Node {_args = trs} = any hasInput trs

typesToArgMap :: [GType] -> M.Map GType [Int]
typesToArgMap types = M.fromListWith (<>) $ zipWith (\i t -> (t, [i])) [0 ..] types

-- | Returns a list of possible arg types that make the operation satisfy the output type
instantiateArgs ::
  -- | what types are allowed
  [GType] ->
  -- | the desired output type for the operation
  OutputType ->
  -- | the operation
  Operation ->
  [ArgTypes]
instantiateArgs relevantTypes desiredOutputType op = case unifyTypes desiredOutputType (opOutput op) of
  Nothing -> []
  Just gt ->
    getPossibilities
      (opArgTypes op)
      relevantTypes
      (learnTypes (opOutput op, gt))
  where
    getPossibilities :: ArgTypes -> [GType] -> [(GType, GType)] -> [[GType]]
    getPossibilities [] relevantTypes constraints = return []
    getPossibilities (a : as) relevantTypes constraints = case checkConstraints constraints a of
      Just xs -> do
        x <- mapMaybe (unifyTypes xs) relevantTypes
        ps <- getPossibilities as relevantTypes (constraints <> learnTypes (a, x))
        return (x : ps)
      Nothing -> do
        (t, cs) <- zip possibleTypes newConstraints
        ps <- getPossibilities as relevantTypes (constraints <> cs)
        return (t : ps)
      where
        possibleTypes = mapMaybe (unifyTypes a) relevantTypes
        newConstraints = (\t -> learnTypes (a, t)) <$> possibleTypes

    checkConstraints :: [(GType, GType)] -> GType -> Maybe GType
    checkConstraints constraints (GPoly n) = snd <$> find ((== GPoly n) . fst) constraints
    checkConstraints constraints (GList x) = GList <$> checkConstraints constraints x
    checkConstraints constraints (GPair a b) = case (checkConstraints constraints a, checkConstraints constraints b) of
      (Just ra, Just rb) -> Just (GPair ra rb)
      (Just ra, Nothing) -> Just (GPair ra b)
      (Nothing, Just rb) -> Just (GPair a rb)
      (Nothing, Nothing) -> Nothing
    checkConstraints constraints (GLambda (MkFunctionType args out)) =
      if isNothing o && as == args
        then Nothing
        else Just $ GLambda (MkFunctionType as (fromMaybe out o))
      where
        as = zipWith fromMaybe args (checkConstraints constraints <$> args)
        o = checkConstraints constraints out
    checkConstraints _ x = Just x
