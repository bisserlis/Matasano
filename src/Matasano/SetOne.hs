module Matasano.SetOne (test) where

import qualified Matasano.SetOne.ChallengeOne as ChallengeOne
import qualified Matasano.SetOne.ChallengeTwo as ChallengeTwo

test :: IO ()
test = do
        putStrLn "Set One"
        
        putStrLn ""
        
        ChallengeOne.test
        ChallengeTwo.test
