module BlackJack where

import Control.Monad ( when ) 
import Control.Monad.Trans.State ( evalStateT ) 
import Control.Monad.Trans.Class ( MonadTrans(lift) )
--import Control.Monad.Trans.Except
import System.Random.Shuffle (shuffleM)
import Card 
import Field




----------
-- Game --
----------

initialField :: Field
initialField = Field [][][] False


runGame :: IO ()
runGame = evalStateT (initGame >> playerTurn) initialField


initGame :: Game ()
initGame = putFieldDeck fullDeck >> shuffle >> initHand

shuffle :: Game ()
shuffle = do
    deck <- getFieldDeck 
    shuffled <- lift $ shuffleM deck
    putFieldDeck shuffled

initHand :: Game ()
initHand = do
    putPlayerHand [] >> putDealerHand []
    playerDraw >> dealerDraw
    playerDraw >> dealerDraw
    getFieldDeck >>=  putFieldDeck . tail

----------
-- Draw --    
----------

maybeDraw :: Game (Maybe Card) 
maybeDraw = do
    d <- getFieldDeck 
    case d of
        [] -> return Nothing 
        (c:cs) -> putFieldDeck cs >> return (Just c)

playerDraw :: Game ()
playerDraw = do
    c <- maybeDraw
    case c of
        Nothing -> runOut
        Just c -> do
            hand_ <- getPlayerHand 
            putPlayerHand (hand_ ++ [c])

dealerDraw :: Game ()
dealerDraw = do    
    c <- maybeDraw
    case c of
        Nothing -> runOut
        Just c -> do
            hand_ <- getDealerHand 
            putDealerHand (hand_ ++ [c])


---------------
-- Main Loop --    
---------------            


playerTurn :: Game ()
playerTurn = do
    hold <- isHold
    let next = if hold then playerTurn else dealerTurn
    showPlayerHand
    hit <- hitOrHold Player
    if hit
        then do
            playerDraw
            hand <- getPlayerHand
            let busted = getPoint hand == 0
            if busted then bust Player else next
        else if hold 
            then showResult
            else putStatus True >> next


dealerTurn :: Game ()
dealerTurn = do
    hold <- isHold
    let next = if hold then dealerTurn else playerTurn  
    hit <- hitOrHold Dealer
    if hit
        then do
            dealerDraw
            hand <- getDealerHand
            let busted = getPoint hand == 0
            if busted then bust Dealer else next
        else if hold
            then showResult
            else putStatus True >> next


-------------
-- 分岐条件 --    
-------------               

printLn :: String -> Game()
printLn = lift . putStrLn

yesno :: String -> Game Bool
yesno msg = do    
    printLn $ msg ++ " yes/no"
    res <- lift getLine
    case res of
        "yes" -> return True
        "no" -> return False
        _     -> printLn "yesかnoで答えて下さい" >> yesno msg

hitOrHold :: Player -> Game Bool
hitOrHold Player  =  yesno "カードを引きますか？"  
hitOrHold Dealer = do
    point <- getPoint <$> getDealerHand
    return (point < 17) 



--------------
-- 終了/継続 -- 
-------------- 


bust :: Player -> Game ()
bust p = do
    printLn $ show p ++ "がバーストしました"
    showResult

continue :: Game ()
continue = do
    yes <- yesno "ゲームを続けますか？"    
    when yes (putStatus False >> initHand >> playerTurn) 

newGame :: Game ()
newGame = do
    yes <- yesno "山札を配り直しますか？"
    when yes initGame

runOut :: Game ()
runOut = printLn "山札がなくなりました" >> newGame


----------
-- Show --    
----------  


getWinner :: Game (Maybe Player)
getWinner = do
    playerPoint <- getPoint <$> getPlayerHand 
    dealerPoint <- getPoint <$> getDealerHand
    if playerPoint == dealerPoint
        then return Nothing
        else if playerPoint > dealerPoint
            then return $ Just Player 
            else return $ Just Dealer


showResult :: Game ()
showResult = do
    winner <- getWinner
    case winner of
        Nothing -> printLn "引き分けです"
        Just Player -> printLn "あなたの勝ちです"
        Just Dealer -> printLn "あなたの負けです"
    showHands
    continue

showPlayerHand :: Game ()
showPlayerHand = do         
    playerHand <- getPlayerHand
    let point = getPoint playerHand
    printLn $ "あなたの手札：" ++ show playerHand ++ " " ++ show point ++ "点"

showDealerHand :: Game ()
showDealerHand = do         
    dealerHand <- getDealerHand
    let point = getPoint dealerHand 
    printLn $ "親の手札    ：" ++ show dealerHand ++ " " ++ show point ++ "点"

showHands :: Game ()    
showHands = showPlayerHand >> showDealerHand


            
























