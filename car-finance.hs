{-# LANGUAGE OverloadedStrings #-}
module Example where

import           Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract

{-
    Contract: Car buy/sell via bank finance
    Author: Sujan Dhakal
    Description: A car seller and a car buyer agree on car selling/buying price. 
    A bank deposits agreed currency to their own account. 
    The car seller deposits the car token. 
    The bank pays the car price value to the car seller.
    The car seller pays the car token to the bank.
    The seller pays all monthly premiums. 
    The bank pays the car token to the car buyer. 

-}

contract :: Contract
contract = carFinance

carFinance :: Contract
carFinance = When [Case buyerChoice $ 
                        When [Case sellerChoice bothAgreed] 20 Close 
                    , Case sellerChoice $ 
                        When [Case buyerChoice bothAgreed] 20 Close
                    ] 10 Close

bothAgreed :: Contract
bothAgreed = If (ValueEQ  (ChoiceValue buyerChoiceId) (ChoiceValue sellerChoiceId))
                deposits
                Close

deposits :: Contract
deposits = When [Case bankDeposits $ 
                    When [Case sellerDeposits bothDeposited] 40 Close
                , Case sellerDeposits $ 
                    When [Case bankDeposits bothDeposited] 40 Close
            ] 30 Close

bothDeposited :: Contract 
bothDeposited = Pay "bank" (Party "seller") ada (ChoiceValue buyerChoiceId) $
                    Pay "seller" (Party "bank") nsnAlt12 nsnAlt12Q
                        (buyerPaysPremium 1)

buyerPaysPremium :: Integer -> Contract
buyerPaysPremium i = When [Case buyerDeposits $
                            Pay "buyer" (Party "bank") ada monthlyPayment 
                            (if (i > numOfMonth)
                            then carTransferToBuyer
                            else (buyerPaysPremium (i+1)))
                    ]((Slot i)*(Slot 100)) Close

carTransferToBuyer :: Contract
carTransferToBuyer = When [Case banksDepositsCar $ 
                            Pay "bank" (Party "seller") nsnAlt12 nsnAlt12Q Close
                        ](((Slot numOfMonth)+(Slot 1))*(Slot 100)) Close

buyerChoiceId, sellerChoiceId :: ChoiceId
buyerChoiceId = ChoiceId "buyerPrice" "buyer"
sellerChoiceId = ChoiceId "sellerPrice" "seller"

buyerChoice, sellerChoice :: Action
buyerChoice = Choice buyerChoiceId priceOption
sellerChoice = Choice sellerChoiceId priceOption

priceOption :: [Bound]
priceOption = [Bound 5000 6000]

bankDeposits, sellerDeposits, buyerDeposits, banksDepositsCar :: Action
bankDeposits = Deposit "bank" "bank" ada (ChoiceValue buyerChoiceId)
sellerDeposits = Deposit "seller" "seller" nsnAlt12 nsnAlt12Q
buyerDeposits = Deposit "buyer" "buyer" ada monthlyPayment
banksDepositsCar = Deposit "bank" "bank" nsnAlt12 nsnAlt12Q

nsnAlt12 :: Token
nsnAlt12 = Token "" "nsnalt12"

nsnAlt12Q :: Value 
nsnAlt12Q = Constant 1

calcMonthlyPaymentScale monthlyRate numOfTerm =
    let x = (monthlyRate + 1)^numOfTerm
    in (monthlyRate * x)/(x - 1)

monthlyPayment = Scale (calcMonthlyPaymentScale monthlyRate numOfMonth) (ChoiceValue buyerChoiceId)

monthlyRate = (0.045/12)

numOfMonth = 54