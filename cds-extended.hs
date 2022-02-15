{-# LANGUAGE OverloadedStrings #-}
module CDSExtended where

import           Language.Marlowe

{-
        Author: Sujan Dhakal
        Contract: Credit default Swap extended.
        Problem: Investor buys 1000 ada worth of bond for 10 year where they get
                 10% (100 ada) interest per year and sum (1000 ada) after 10 years.
                 Investor buys cds for 1000 ada for 10 years from cdsSeller to secure their investment 
                 where the investor pays 30 ada as premium.
                 cdsSeller pays 1000 + remaining interest to investor in credit event.
-}

main :: IO ()
main = print . pretty $ cds 10

cds maxTerm = 
    let year k = 
          When [Case bondSellerDefault 
                     (Pay "cdsSeller" (Party "investor") ada 
                          (swapValue `SubValue` ((Constant (k - 1)) `MulValue` interest))
                          Close)]
               ((daysInAYear k) - day)
               (When  [Case bondSellerPaysInterest
                            (if k == maxTerm then 
                                 (Pay "bondSeller" (Party "investor") ada interest Close)
                             else
                                 (Pay "bondSeller" (Party "investor") ada (interest `SubValue` premium) 
                                    (Pay "bondSeller" (Party "cdsSeller") ada premium
                                        (Pay "cdsSeller" (Party "cdsSeller") ada interest
                                            (if k > maxTerm then Close else year (k + 1))))))]
                        (daysInAYear k)
                        Close)

    in
        When [Case investorAllocatesFundsForBondPurchase
                   (When [Case (Deposit "cdsSeller" "cdsSeller" ada swapValue)
                               (Pay "investor" (Party "bondSeller") ada bondValue
                               (When [Case (Deposit "investor" "investor" ada premium)
                                           (Pay "investor" (Party "cdsSeller") ada premium
                                            (year 1))]
                                     (currentSlot + (3 * day))
                                     Close))]
                         (currentSlot + (2 * day))
                         Close)]
             (currentSlot + (1 * day))
             Close

--daysInAYear :: Timeout
daysInAYear k = currentSlot + ((Slot k) * ((Slot 365) * day))

bondValue :: (Value Observation)
bondValue = Constant 1000

swapValue :: (Value Observation)
swapValue = Constant 2000

premium :: (Value Observation)
premium = Constant 30

interest :: (Value Observation)
interest = Constant 100

choiceName :: ChoiceName
choiceName = "choice"

choice :: Party -> [Bound] -> Action
choice party = Choice (ChoiceId choiceName party)

bondSellerPaysInterest :: Action
bondSellerPaysInterest = Deposit "bondSeller" "bondSeller" ada interest

bondSellerDefault :: Action
bondSellerDefault = choice "bondSeller" defaulted

investorAllocatesFundsForBondPurchase :: Action
investorAllocatesFundsForBondPurchase = Deposit "investor" "investor" ada bondValue


defaulted :: [Bound]
defaulted = [Bound 0 0]

currentSlot :: Slot
currentSlot = Slot 0

day :: Slot
day = Slot 4320
