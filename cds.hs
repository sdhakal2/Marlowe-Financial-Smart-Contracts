{-# LANGUAGE OverloadedStrings #-}
module CDS where

import           Language.Marlowe

main :: IO ()
main = print . pretty $ cds 10

{-
        Author: Sujan Dhakal
        Contract: Credit default Swap
        Problem: cdsBuyer buys cds for 1000 ada for 10 years. 
                 buyer pays 30 ada premium.
                 seller pays 1000 + remaining interest to buyer in credit event.
                 Note: seller gets 100 ada as interest per year from bond seller. 
-}




cds maxTerm = 
    let year k = When [Case bondSellerChoice 
                   (Pay "cdsSeller" (Party "cdsBuyer") ada (swapValue `SubValue` ((Constant (k - 1)) `MulValue` accessFund))
                        Close)]
             ((currentSlot + ((Slot k) * ((Slot 365) * day))) - day)
             (When [Case (Deposit "cdsBuyer" "cdsBuyer" ada premium)
                         (Pay "cdsBuyer" (Party "cdsSeller") ada premium
                                (Pay "cdsSeller" (Party "cdsSeller") ada accessFund
                                        (if k >= maxTerm then Close else year (k + 1))))]
                   (currentSlot + ((Slot k) * ((Slot 365) * day)))
                   Close)
    in
        When [Case (Deposit "cdsSeller" "cdsSeller" ada swapValue)
                (When [Case (Deposit "cdsBuyer" "cdsBuyer" ada premium) 
                                (Pay "cdsBuyer" (Party "cdsSeller") ada premium (year 1))]
                (currentSlot + (2 * day))
                Close)]
        (currentSlot + (1 * day))
        Close

swapValue :: (Value Observation)
swapValue = Constant 2000

premium :: (Value Observation)
premium = Constant 30

accessFund :: (Value Observation)
accessFund = Constant 100

choiceName :: ChoiceName
choiceName = "choice"

choice :: Party -> [Bound] -> Action
choice party = Choice (ChoiceId choiceName party)

bondSellerChoice :: Action
bondSellerChoice = choice "bondSeller" defaulted

defaulted :: [Bound]
defaulted = [Bound 0 0]

currentSlot :: Slot
currentSlot = Slot 0

day :: Slot
day = Slot 4320
