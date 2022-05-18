{-# LANGUAGE OverloadedStrings #-} -- Enables the use of string literals as `Text` values.

module DockerfileBuildSecrets (jsonFromDockerfile) where

import Control.Monad (join)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text, dropWhileEnd, takeWhileEnd, unpack)
import Language.Docker.Syntax
    ( Dockerfile
    , InstructionPos (instruction)
    , Instruction (Run)
    , RunArgs (RunArgs)
    , RunFlags (mount)
    , RunMount (SecretMount)
    , SecretOpts (sCacheId, sSource, sTarget, sIsRequired)
    , SourcePath (unSourcePath)
    , TargetPath (unTargetPath)
    )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.JSON as JSON


jsonFromDockerfile :: Dockerfile -> String
jsonFromDockerfile = JSON.encode . JSON.toJSObject . withDesiredStructure . secretsFromDockerfile


withDesiredStructure :: MapFromIdToRequired -> [(String, JSON.JSValue)]
withDesiredStructure = Map.toList . Map.mapKeys unpack . Map.map makeInnerObject -- `Text` is not JSON-encodable, so we must unpack the keys to `String` values.
    where
        makeInnerObject :: Bool -> JSON.JSValue
        makeInnerObject isRequired = JSON.encJSDict [("required" :: String, isRequired)] -- We want `{"foo":{"required":true}}` instead of `{"foo":true}`, because it's more extensible and easier to understand.


data Secret = Secret Bool Text -- Hadolint's parser uses `Text`.

type MapFromIdToRequired = Map.Map Text Bool


secretsFromDockerfile :: Dockerfile -> MapFromIdToRequired
secretsFromDockerfile = mapFromListOfMaps . map secretsFromInstruction
    where
        mapFromListOfMaps :: [MapFromIdToRequired] -> MapFromIdToRequired
        mapFromListOfMaps = Map.unionsWith (||) -- Using `(||)` as the combining function means each secret ID is required if and only if it is required in _at least one_ of the instructions.


secretsFromInstruction :: InstructionPos Text -> MapFromIdToRequired
secretsFromInstruction i = case instruction i of
    Run (RunArgs _ runFlags) -> secretsFromRunFlags runFlags
    _ -> Map.empty


secretsFromRunFlags :: RunFlags -> MapFromIdToRequired
secretsFromRunFlags runFlags =
    -- For each secret ID referenced in this instruction, we need to find out whether there is _at least one_ `--mount` flag that requires it.
    -- For example, in `RUN --mount=type=secret,id=foo,required=true --mount=type=secret,id=foo echo`, the secret ID `foo` is required, even though it is optional in the second `--mount` flag.
    Map.fromListWith (||) secretsInThisRunInstruction -- Using `(||)` as the combining function means each secret ID is required if and only if there is _at least one_ `--mount` flag in this instruction that requires it.
    where
        secretsInThisRunInstruction :: [(Text, Bool)]
        secretsInThisRunInstruction = map toKeyValuePair . mapMaybe secretFromRunMount $ Set.toList $ mount runFlags


toKeyValuePair :: Secret -> (Text, Bool)
toKeyValuePair (Secret required secretId) = (secretId, required)


secretFromRunMount :: RunMount -> Maybe Secret
secretFromRunMount (SecretMount opts) = Secret isRequired <$> firstJust possibleIDs
    where
        isRequired :: Bool
        isRequired = fromMaybe False (sIsRequired opts) -- If `required` is not specified, then the secret is not required (in this `--mount` flag; it may of course be required somewhere else in the Dockerfile).

        possibleIDs :: [Maybe Text]
        possibleIDs =
            -- Precedence in BuildKit is source > cache ID > target. 👉 https://github.com/moby/buildkit/blob/55f0ecfdafaa5b06d8fe34d748600748f53c6829/frontend/dockerfile/dockerfile2llb/convert_secrets.go#L12:L22
            [ unSourcePath <$> sSource opts              -- https://github.com/moby/buildkit/blob/55f0ecfdafaa5b06d8fe34d748600748f53c6829/frontend/dockerfile/dockerfile2llb/convert_secrets.go#L14
            , sCacheId opts                              -- https://github.com/moby/buildkit/blob/55f0ecfdafaa5b06d8fe34d748600748f53c6829/frontend/dockerfile/dockerfile2llb/convert_secrets.go#L12
            , pathBase . unTargetPath <$> sTarget opts   -- https://github.com/moby/buildkit/blob/55f0ecfdafaa5b06d8fe34d748600748f53c6829/frontend/dockerfile/dockerfile2llb/convert_secrets.go#L21
            ]
secretFromRunMount _ = Nothing


-- | Returns the first element in the given list that is not `Nothing`. If all the elements are `Nothing`, then `Nothing` is returned.
firstJust :: [Maybe a] -> Maybe a
firstJust = join . find isJust -- `join` "flattens" a `Maybe (Maybe a)` to a `Maybe a`.


-- | Returns the last element of the given path. Ported from https://pkg.go.dev/path@go1.18.2#Base.
pathBase :: Text -> Text
pathBase "" = "."                                     -- 'If the path is empty, Base returns ".".' — Go docs
pathBase path = case dropWhileEnd isSlash path of     -- 'Trailing slashes are removed before extracting the last element.' — Go docs
    "" -> "/"                                         -- 'If the path consists entirely of slashes, Base returns "/".' — Go docs
    stripped -> takeWhileEnd (not . isSlash) stripped -- 'Base returns the last element of path.' — Go docs
    where
        isSlash = (== '/')
