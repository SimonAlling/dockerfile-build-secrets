import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Text (pack)
import DockerfileBuildSecrets (jsonFromDockerfile)
import Language.Docker (errorBundlePretty, parseText)
import System.Directory (listDirectory)
import Test.Hspec (Spec, hspec, shouldBe, specify)


testFilePrefix :: String
testFilePrefix = "Dockerfile."


expectedOutputSuffix :: String
expectedOutputSuffix = ".out"


testDataDir :: String
testDataDir = "test/testdata/"


main :: IO ()
main = do
    fileNames <- getTestDataFileNames
    fileNamesAndContents <- mapM getTestData fileNames
    hspec $ combineSpecs $ map spec fileNamesAndContents


combineSpecs :: [Spec] -> Spec
combineSpecs = foldr (>>) (pure ()) -- Because `Spec` is defined as `SpecM () ()` and `SpecM ()` is a writer monad, two specs can be combined with `(>>)`. The choice of `foldr` or `foldl'` doesn't affect the order of the test cases, as everything is collected in the "log" of the writer monad, not by building up a result throughout the fold.


spec :: (FilePath, String, String) -> Spec
spec (dockerfileName, dockerfileContent, expectedOutput) = case parseText (pack dockerfileContent) of
    Right dockerfile -> specify dockerfileName $ jsonFromDockerfile dockerfile `shouldBe` expectedOutput
    Left parseErrorBundle -> error (errorBundlePretty parseErrorBundle) -- We assume that all test Dockerfiles are valid.


getTestDataFileNames :: IO [String]
getTestDataFileNames = sort . filter isTestInputFile <$> listDirectory testDataDir
    where
        isTestInputFile :: String -> Bool
        isTestInputFile fileName = testFilePrefix `isPrefixOf` fileName && not (expectedOutputSuffix `isSuffixOf` fileName)


getTestData :: FilePath -> IO (FilePath, String, String)
getTestData fileName = do
    fileContent <- readFile (testDataDir ++ fileName)
    expectedOutput <- removeTrailingNewline <$> readFile (testDataDir ++ fileName ++ expectedOutputSuffix)
    return (fileName, fileContent, expectedOutput)
    where
        removeTrailingNewline = init -- We assume all expected-output files are non-empty and end with a newline.
