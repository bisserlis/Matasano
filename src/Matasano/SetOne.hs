module Matasano.SetOne (test) where

import qualified Matasano.SetOne.ChallengeOne as ChallengeOne
import qualified Matasano.SetOne.ChallengeTwo as ChallengeTwo
import qualified Matasano.SetOne.ChallengeThree as ChallengeThree
import qualified Matasano.SetOne.ChallengeFour as ChallengeFour

test :: IO ()
test = do
        putStrLn "Set One"
        
        putStrLn ""
        
        ChallengeOne.test
        ChallengeTwo.test
        ChallengeThree.test
        ChallengeFour.test
