{-# LANGUAGE OverloadedStrings #-}
module CallMoney where

import           Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract


{- Define a contract, Close is the simplest contract which just ends the contract straight away
-}

contract :: Contract
contract = callMoney

callMoney::Contract
callMoney = When [Case firstBankChoice $ 
                            When [ Case secondBankChoice bothMadeChoice] 40 Close
                        , Case secondBankChoice $ 
                            When [Case firstBankChoice bothMadeChoice] 40 Close
                        ] 20 Close 

bothMadeChoice :: Contract
bothMadeChoice = If (ValueEQ  (ChoiceValue firstBankChoiceId) (ChoiceValue secondBankChoiceId))
                    fundTransfer
                    Close

fundTransfer :: Contract
fundTransfer = When [Case firstBankDeposits $ 
                        payToSecondBank (waitForCall 1 maxDays)
                    ] 60 Close

waitForCall :: Integer -> Value -> Contract
waitForCall k mt = When [Case callMade $ 
                        If (OrObs (ValueEQ (Constant k) mt) (ValueEQ (ChoiceValue callMadeChoiceId) (Constant 1)))
                            (returnMoney k)
                            (continueToHoldMoney k maxDays)
                   ] ((Slot k) * day) Close

returnMoney::Integer -> Contract
returnMoney k = When [Case secondBankDepositsPrNIn $ 
                        payPrNInToFirstBank Close
                   ] (((Slot k) * day) + (Slot 60)) Close

continueToHoldMoney :: Integer -> Value -> Contract
continueToHoldMoney k mt = When [Case secondBankDepositsIn $ 
                                payInToFirstBank (waitForCall (k+1) mt)
                             ] (((Slot k) * day) + (Slot 60)) Close

choice :: ChoiceName -> Party -> [Bound] -> Action
choice choiceName party = Choice (ChoiceId choiceName party)

deposit :: Party -> Party -> Token -> Value -> Action
deposit party account token value = Deposit party account token value

pay :: Party -> Payee -> Token -> Value -> Contract -> Contract
pay party payee token value ct = Pay party payee token value ct

payToSecondBank, payPrNInToFirstBank, payInToFirstBank :: Contract -> Contract
payToSecondBank ct = pay "firstBank" (Party "secondBank") ada callMoneyValue ct
payPrNInToFirstBank ct = pay "secondBank" (Party "secondBank") ada (callMoneyValue `AddValue` (calcInterest callMoneyValue rate)) ct
payInToFirstBank ct = pay "secondBank" (Party "secondBank") ada (calcInterest callMoneyValue rate) ct

option :: [Bound]
option = [Bound 0 1]

callMade, firstBankChoice, secondBankChoice, firstBankDeposits, secondBankDepositsPrNIn, secondBankDepositsIn :: Action
callMade = choice "firstBankCall" "firstBank" option
firstBankChoice = choice "firstBankChoice" "firstBank" option
secondBankChoice = choice "secondBankChoice" "secondBank" option
firstBankDeposits = deposit "firstBank" "firstBank" ada callMoneyValue
secondBankDepositsPrNIn = deposit "secondBank" "secondBank" ada (callMoneyValue `AddValue` (calcInterest callMoneyValue rate))
secondBankDepositsIn = deposit "secondBank" "secondBank" ada (calcInterest callMoneyValue rate)

firstBankChoiceId, secondBankChoiceId, callMadeChoiceId :: ChoiceId
firstBankChoiceId = ChoiceId "firstBankChoice" "firstBank" 
secondBankChoiceId = ChoiceId "secondBankChoice" "secondBank" 
callMadeChoiceId = ChoiceId "firstBankCall" "firstBank"

callMoneyValue :: Value
callMoneyValue = Constant 140000000

rate :: Value
rate = Constant 234

maxDays :: Value
maxDays = Constant 14

day :: Timeout
day = Slot 4320

calcInterest :: Value -> Value -> Value  --Observation
calcInterest p r = 
        Scale (1 % 365) (Scale (1 % 10000) (MulValue p r))