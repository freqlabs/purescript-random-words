module RandomWords where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array (length, unsafeIndex)
import Data.String (Pattern(..), split)
import Partial.Unsafe (unsafePartial)

import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)


systemDictionary :: FilePath
systemDictionary = "/usr/share/dict/words"

readWords :: forall eff. FilePath -> Aff (fs :: FS | eff) (Array String)
readWords dictionary = do
  wordsText <- readTextFile UTF8 dictionary
  pure $ split (Pattern "\n") wordsText

readWords' :: forall eff. Aff (fs :: FS | eff) (Array String)
readWords' = readWords systemDictionary

randomWord :: forall e. Array String -> Eff (random :: RANDOM | e) String
randomWord words = do
  index <- randomInt 0 $ length words
  pure $ unsafePartial $ unsafeIndex words index
