import Test.HUnit
import qualified Pikatock.Util.Test
import Test.Framework
import Test.Framework.Providers.HUnit

main = defaultMain $ hUnitTestToTests Pikatock.Util.Test.tests
