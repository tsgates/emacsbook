import Text.Pandoc
import Data.Char(toLower)
import System.Process (readProcess)
import System.IO.Unsafe

main = toJsonFilter highlight

highlight (CodeBlock (_, options , _ ) code) = RawBlock "html" (pygments code options)
highlight x = x

pygments:: String -> [String] -> String
pygments code options
         | (length options) == 1 = doPygmentize []
         | (length options) == 2 = doPygmentize ["-O linenos"]
         | otherwise = "<div class =\"highlight\"><pre>" ++ code ++ "</pre></div>"
  where doPygmentize opts = unsafePerformIO $
          readProcess "pygmentize" (["-l", (map toLower $ head options), "-f", "html"] ++ opts) code
