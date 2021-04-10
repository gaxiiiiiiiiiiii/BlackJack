module Field (Player(..), Field(..), Game,
              getPlayerHand, getDealerHand, getFieldDeck, isHold,
              putPlayerHand, putDealerHand, putFieldDeck, putStatus) where
import Card
import Control.Monad.Trans.State 

data Player
    = Player
    | Dealer 

instance Show Player where
    show Player = "あなた"
    show Dealer = "親"


data Field  = Field
    { playerHand :: Hand
    , dealerHand :: Hand
    , fieldDeck :: Deck
    , hold :: Bool    
    } deriving Show
 
type Game a = StateT Field IO a


baseGet :: (Field -> a) -> Game a
baseGet f     = get >>= return . f
getPlayerHand = baseGet playerHand
getDealerHand = baseGet dealerHand
getFieldDeck  = baseGet fieldDeck
isHold        = baseGet hold


updatePlayerHnad h f = f{playerHand = h}
updateDealerhand h f = f{dealerHand = h}
updateFieldDeck d f  = f{fieldDeck  = d}
updateStatus s f     = f{hold       = s}


basePut :: (a -> Field -> Field) -> a -> Game ()
basePut f a = get >>= put . f a
putPlayerHand = basePut updatePlayerHnad
putDealerHand = basePut updateDealerhand
putFieldDeck  = basePut updateFieldDeck
putStatus     = basePut updateStatus 