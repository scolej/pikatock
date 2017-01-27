import qualified Pikatock.Util.Test
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main = defaultMain $ hUnitTestToTests Pikatock.Util.Test.tests
