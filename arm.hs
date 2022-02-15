{-# LANGUAGE OverloadedStrings #-}
module ARM where

import           Language.Marlowe

main :: IO ()
main = print . pretty $ contract 7

{-
        Author: Sujan Dhakal
        Contract: Adjustable-Rate Mortgage
        Description:    A house seller sells a house to a house buyer.
                        A bank finances the purchase with 2% for first year 
                        and variable interest rate for remaining 29 years.  
-}


contract maxTerm = 
    let year k remainingBalance = When [Case varInterestChoice 
            (When [Case (Deposit "houseBuyer" "houseBuyer" ada (calcMortgage remainingBalance (ChoiceValue (ChoiceId "adausdt" "kraken")) (31 - k)) )
                (Pay "houseBuyer" (Party "bank") ada (calcMortgage remainingBalance (ChoiceValue (ChoiceId "adausdt" "kraken")) (31 - k)) 
                    (if k == maxTerm 
                        then (When [Case (Deposit "bank" "bank" house234 houseQ)
                                        (Pay "bank" (Party "houseBuyer") house234 houseQ
                                            Close)] 
                                 (currentSlot + (((Slot k) * ((Slot 365) * day)) + (2 * day)))
                                 Close)
                        else 
                            Let "previousRemainingBalance" remainingBalance 
                            (year (k+1) (SubValue (UseValue "previousRemainingBalance") (calcMortgage (UseValue "previousRemainingBalance") (ChoiceValue (ChoiceId "adausdt" "kraken")) (31 - k))))))] 
                    (currentSlot + (((Slot k) * ((Slot 365) * day)) + day))
                    Close)] 
            (currentSlot + ((Slot k) * ((Slot 365) * day)))
            Close
    in
        When [Case (Deposit "houseSeller" "houseSeller" house234 houseQ)
            (When [Case (Deposit "bank" "bank" ada housePrice)
                     (Pay "bank" (Party "houseSeller") ada housePrice 
                        (Pay "houseSeller" (Party "bank") house234 houseQ 
                            (When [Case (Deposit "houseBuyer" "houseBuyer" ada (calcMortgage housePrice (Constant 2) 30) )
                                     (Pay "houseBuyer" (Party "Bank") ada (calcMortgage housePrice (Constant 2) 30)
                                        (year 2 (SubValue housePrice (calcMortgage housePrice (Constant 2) 30) ) ) )] 
                            (currentSlot + (day * (Slot 365))) 
                            Close)))]
            (currentSlot + (day * (Slot 2)))
            Close)]
        (currentSlot + day)
        Close


initialInterest :: (Value Observation)
initialInterest = Constant 2

housePrice :: (Value Observation)
housePrice = Constant 210000

house234 :: Token
house234 = Token "" "house234"

houseQ :: (Value Observation)
houseQ = Constant 1

currentSlot :: Slot
currentSlot = Slot 0

day :: Slot
day = Slot 4320

choiceName :: ChoiceName
choiceName = "adausdt"

choice :: Party -> [Bound] -> Action
choice party = Choice (ChoiceId choiceName party)

varInterestChoice :: Action
varInterestChoice = choice "kraken" varInterest

varInterest :: [Bound]
varInterest = [Bound 2000 2000]

calcMortgage :: (Value Observation) -> (Value Observation) -> Integer -> (Value Observation)
calcMortgage price rate termRem = 
        Scale (1 % termRem) (AddValue price (Scale (1%100) (MulValue price rate)))
