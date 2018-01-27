module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Node.FS (FS)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldContain, shouldNotEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import RandomWords (randomWord, readWords')


main :: Eff (RunnerEffects (fs :: FS, random :: RANDOM)) Unit
main = run [consoleReporter] do
  describe "loadWords" do
    it "Reads a dictionary file into an array of words" do
      words <- readWords'
      words `shouldNotEqual` []
  describe "randomWord" do
    it "Chooses a word from an array of words" do
      words <- readWords'
      word <- liftEff $ randomWord words
      words `shouldContain` word
    it "Doesn't always choose the same word" do
      words <- readWords'
      word1 <- liftEff $ randomWord words
      word2 <- liftEff $ randomWord words
      word1 `shouldNotEqual` word2
