-- A Pandoc filter to use Pygments for Pandoc
-- Code blocks in HTML output 
-- Matti Pastell 2011 <matti.pastell@helsinki.fi> 
-- More filters in https://bitbucket.org/mpastell/pandoc-filters/
-- Requires Pandoc 1.8

import Text.Pandoc
import Text.Pandoc.Shared
import Data.Char(toLower)
import System.Process (readProcess)
import System.IO.Unsafe


main = interact $ jsonFilter $ bottomUp highlight

highlight :: Block -> Block
highlight (CodeBlock (_, options , _ ) code) = RawBlock "html" (pygments code options)
highlight x = x

pygments:: String -> [String] -> String
pygments code options 
         | (length options) == 1 = unsafePerformIO $ readProcess "pygmentize" ["-l", (map toLower (head options)),  "-f", "html"] code
         | (length options) == 2 = unsafePerformIO $ readProcess "pygmentize" ["-l", (map toLower (head options)), "-O linenos",  "-f", "html"] code
         | otherwise = "<div class =\"highlight\"><pre>" ++ code ++ "</pre></div>"
