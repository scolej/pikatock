import Test.HUnit
import qualified Pikatok.Util.Test
import Test.Framework
import Test.Framework.Providers.HUnit

main = defaultMain $ hUnitTestToTests Pikatok.Util.Test.tests
