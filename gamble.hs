{-# LANGUAGE OverloadedStrings #-}
module Gamble where

import           Language.Marlowe
import           System.Random

main :: IO ()
main = print . pretty $ (playerDeposit players players 1)

{-
    1.  Anyone can deposit exactlu 1 ada.
    2.  All ada goes to contract owner address.
    3.  Once ada is deposited, depositer address is stored 
        members and participators address list.
    4.  Once owner account balance reach to 10 ada, winer 
        is selected randomly and pay 8 ada.
    5.  Repeat again. 
-}

playerDeposit::[Party]->[Party]->Slot->Contract
playerDeposit [] winnerList k = Close
playerDeposit playerList winnerList k = 
    When [Case (Deposit "owner" (head playerList) ada (Constant 1)) 
                    (If (ValueLT (AvailableMoney "owner" ada) (Constant 10)) 
                        (playerDeposit (tail playerList) winnerList (k + 1))
                        (drawWinner winnerList))]
        (10 * k)
        Close

drawWinner ::[Party]->Contract
drawWinner [] = Close
drawWinner playerList = 
    let winner = 3 --Replace this with a random number between 0-list length. 
    in Pay "owner" (Party (playerList !! winner)) ada (Constant 8) Close

players :: [Party]
players = [(Role "player1"),(Role "player2"),(Role "player3"),(Role "player4"),(Role "player5"),(Role "player6"),(Role "player7"),(Role "player8"),(Role "player9"),(Role "player10")]

