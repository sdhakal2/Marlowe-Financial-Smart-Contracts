{-# LANGUAGE OverloadedStrings #-}
module PAM where

import           Language.Marlowe

main :: IO ()
main = print . pretty $ contract 10

{-
        Author: Sujan Dhakal
        Contract: 10 year $100 Treasury notes
        Description: Buyer gets 2% interest per year on face value for 10 years. 
                     At maturity, buyer gets face value back. 
-}


contract maxTerm = 
        let year k = When [Case (Deposit "bondSeller" "bondSeller" ada interest) 
                                (Pay "bondSeller" (Party "bondBuyer") ada interest 
                                        (if k >= maxTerm then Close else year (k+1)))] 
                     (currentSlot + ((Slot k) * ((Slot 365) * day)))
                     Close
        in 
                When [Case (Deposit "bondBuyer" "bondBuyer" ada faceValue)
                        (Pay "bondBuyer" (Party "bondSeller") ada faceValue 
                                (year 1))]
                (currentSlot + day)
                Close

interest :: (Value Observation)
interest = Constant 2

faceValue :: (Value Observation)
faceValue = Constant 100

currentSlot :: Slot
currentSlot = Slot 0

day :: Slot
day = Slot 4320