module Matasano.SetOne (test) where

import qualified Matasano.SetOne.ChallengeOne as ChallengeOne

test :: IO ()
test = do
        putStrLn "Set One"
        
        putStrLn ""
        
        ChallengeOne.test
