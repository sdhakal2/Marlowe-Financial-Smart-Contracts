{-# LANGUAGE OverloadedStrings #-}
module Annuity where

import           Language.Marlowe

main :: IO ()
main = print . pretty $ contract 120

{-
        Author: Sujan Dhakal
        Contract: Annuity
        Problem: annuity buyer pays lump sum and get monthly payment of fix amount. 
-}

contract maxTerm = 
        let month k = When [Case (Deposit "annuitySeller" "annuitySeller" ada fixPay) 
                                (Pay "annuitySeller" (Party "annuityBuyer") ada fixPay 
                                        (if k >= maxTerm then Close else month (k+1)))] 
                     (currentSlot + ((Slot k) * ((Slot 30) * day)))
                     Close
        in 
                When [Case (Deposit "annuityBuyer" "annuityBuyer" ada lumpSum)
                        (Pay "annuityBuyer" (Party "annuitySeller") ada lumpSum 
                                (month 1))]
                (currentSlot + day)
                Close

fixPay = Constant (fixPayCalc 500000 120 0.005)

fixPayCalc :: Float -> iteration -> Float -> Integer
fixPayCalc lumpSum iteration rate = round (lumpSum * (rate / (1-(1+rate)^(-iteration))))

lumpSum :: (Value Observation)
lumpSum = Constant 500000

currentSlot :: Slot
currentSlot = Slot 0

day :: Slot
day = Slot 4320