module Board where
import Data.List.Lens
import Control.Lens hiding (Empty)
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Extra
import Control.Monad
import System.Random.Shuffle
import System.IO.Unsafe

data BoardPiece = Occupied | Wall | Traversed | Empty deriving Eq


newtype Board = Board { getPieces :: [[BoardPiece]] }

emptyBoard :: Int -- width
         -> Int -- height
         -> BoardPiece -- default boardpiece
         -> Board 
emptyBoard w h p = Board $ replicate h ( replicate w p )

instance Show Board where
    show (Board pieces) = unlines $ map (\row -> concat $ map show row) pieces

instance Show BoardPiece where
    show x = case x of 
        Occupied -> "* "
        Wall -> "X "
        Traversed -> ". "
        Empty -> "  "



changeTile :: Int -> Int -> BoardPiece -> State Board ()
changeTile r c p = state $ \(Board pieces) -> 
    ((), Board (pieces & ix r . ix c .~ p))

getTile :: Int -> Int -> MaybeT (Reader Board) BoardPiece
getTile r c  = MaybeT $ reader $ \(Board pieces) ->  pieces ^? ix r . ix c


    
maze :: Int -> Int -> Board -- width , height 
maze w h = snd $ runState (generateMaze (1,1)) (emptyBoard w h Wall)

isWall :: BoardPiece -> Bool
isWall p = p == Wall

generateMaze :: (Int,Int)  -- starting point
                -> State Board [(Int, Int)]
generateMaze (r,c) = do
        let siblings =  [(r+1, c), (r-1, c), (r, c+1), (r, c-1)]
        -- check the current state
        flag <- (readerToState . uncurry isWallRemovable) (r,c) 
        if flag == False then return [] else do 
            changeTile r c Empty -- mark a tile as occupied
            eligibleOnes <- filterM (readerToState . uncurry isWallRemovable) siblings :: State Board [(Int, Int)]
            -- TODO Fix unsafe perform IO IMPORTANT
            let suffled = unsafePerformIO $ shuffleM eligibleOnes
            rest <- concatMapM generateMaze suffled :: State Board [(Int, Int)]
            return $ (r,c) : rest
                           


readerToState :: Reader env a -> State env a 
readerToState r = state $ \env -> 
    (runReader r env, env)

isWallRemovable :: Int -> Int -> Reader Board Bool
isWallRemovable r c = do 
    let mbool = (\(r1,c1) -> fmap isWall $ getTile r1 c1) :: (Int, Int) -> MaybeT (Reader Board) Bool
        testpoints =  (,) <$> ([(+1),id, (\x -> x-1)] <*> [r] )
                      <*> ([(+1), id, (\x -> x-1)] <*> [c]) :: [(Int, Int)]
    mbools <- runMaybeT $ (sequence $ fmap mbool testpoints :: MaybeT (Reader Board) [Bool])
    case mbools of Nothing -> return False
                   Just bs -> return $ length (filter id bs) > 6
    



