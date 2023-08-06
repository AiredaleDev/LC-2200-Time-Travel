{-# LANGUAGE OverloadedStrings #-}

module Parse (readProgram) where

import Data.Int (Int32)
import Data.Vector.Unboxed (Vector (..))
import qualified Data.Vector.Unboxed as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (hexadecimal)

-- This is a point-free "|>"
-- For some reason, Haskell uses "&" for "|>" and "$" for "<|"
-- ...and you have to import "&" in Data.Function, but not "$".
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

readProgram :: FilePath -> IO (Vector Int32)
readProgram fname = readFile fname >>=
                    lines .>
                    -- If I find a nice way to use Text instead I don't need to append "0x"
                    map (read . ("0x" ++)) .>
                    V.fromList .>
                    return

{-
-- I don't know if I'm allowed to distribute the assembler I've been using soooo
-- we rollin our own!
assembleFile :: FilePath -> IO ()
assembleFile = TIO.readFile >>=
               T.lines .>
               map textToMachineCode .> -- maybe we should actually fold this into one big text buffer.
               undefined
-}

-- Don't forget to add 1 to PC in memory/jump transactions, as the assembler produces output that expects this!

type Label = (Text, Int)
type Code = Int
assemble :: [Text] -> (Text, Text)
assemble = go "" ""
  where
    go prog labels = undefined
