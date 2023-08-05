module Parse where

import Data.Int (Int32)
import Data.Vector.Unboxed (Vector (..))
import qualified Data.Vector.Unboxed as V

-- This is a point-free "|>"
-- For some reason, Haskell uses "&" for "|>" and "$" for "<|".
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

readProgram :: String -> IO (Vector Int32)
readProgram fname = readFile fname >>=
                    lines .>
                    map (read . ("0x" ++)) .>
                    V.fromList .>
                    return
