module Card (Card, Hand, Deck, getPoint, fullDeck)where

data Card = Card {getSuit :: Suit, getNumber :: Int}
data Suit = Hart | Diamond | Spade | Club deriving Enum
newtype Number = Number Int
type Hand = [Card]
type Deck = [Card]
fullDeck :: Deck
fullDeck = [Card s n | s <- [Hart ..], n <- [1 .. 13]]

instance Show Suit where
    show Hart    = "❤︎"
    show Diamond = "♦︎"
    show Spade   = "♠︎"
    show Club    = "☘"


showNum :: Int -> String
showNum n = 
    case n of
        1  -> "A"
        11 -> "J"
        12 -> "Q"
        13 -> "K"
        _  -> show n   

instance Show Card where
    show (Card {getSuit = s, getNumber = n}) = show s ++ showNum n

to10 :: Card -> Int
to10 Card{getNumber = n} =
    case n of
        11 -> 10
        12 -> 10
        13 -> 10
        _  -> n

getHandPoint :: Hand -> [Int]
getHandPoint h = 
    let s = sum $ map to10 h
    in if 1 `elem` (map getNumber h)
        then [s,s+10]
        else [s]

getValidPoint :: Hand -> [Int]
getValidPoint = filter (<= 21) . getHandPoint

isBust :: Hand -> Bool
isBust h =
    case getValidPoint h of
        [] -> True
        _  -> False

getPoint :: Hand -> Int
getPoint h = 
    case getValidPoint h of
        [] -> 0
        ps -> maximum ps






