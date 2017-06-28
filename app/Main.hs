module Main where
import Graphics.UI.Fungen
import Paths_runner (getDataFileName)

data GameAttribute = Score Int
data GameState = Level Int
type RunnerObject = GameObject ()
type RunnerAction a = IOGame GameAttribute () GameState TileAttribute a

data TileAttribute = NoTileAttribute
type RunnerTile = Tile TileAttribute
type RunnerMap = TileMatrix TileAttribute

screenWidth, screenHeight :: Int
screenWidth = 1000;
screenHeight = 1000;

speed :: Double
speed = 30.0

up, down, right, left :: Int
up = 7
left = 8
down = 9
right = 4

magenta :: InvList
magenta = Just [(255,0,255)]

bmpList :: FilePictureList
bmpList = [("cherry.bmp", magenta),
           ("chefinho.bmp", magenta),
           ("boo.bmp", magenta),
           ("crash.bmp", magenta),
           ("playerright.bmp", magenta),
           ("wall.bmp", Nothing),
           ("border.bmp", Nothing),
           ("playerup.bmp", magenta),
           ("playerleft.bmp", magenta),
           ("playerdown.bmp", magenta)]

tileSize :: Double
tileSize = 50.0

free,border::Int
free  = 5
border = 6

b,f::RunnerTile
b = (border, True,  0.0, NoTileAttribute)
f = (free, False, 0.0, NoTileAttribute)

map1::RunnerMap
map1 = [[f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f]]

map2::RunnerMap
map2 = [[b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
        [b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b]]

map3::RunnerMap
map3 = [[f,f,f,f,f,f,f,f,f,f,b,b,b,b,b,b,b,b,b,b],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [f,f,f,f,f,f,f,f,f,f,b,f,f,f,f,f,f,f,f,f],
        [b,b,b,b,b,b,b,b,b,b,b,f,f,f,f,f,f,f,f,f]]

main :: IO ()
main = do
  let winConfig = ((0,0),(screenWidth,screenHeight),"Runner")
      gameMap = multiMap [(tileMap map1 tileSize tileSize),
                          (tileMap map2 tileSize tileSize),
                          (tileMap map3 tileSize tileSize)] 0
      player    = objectGroup "playerGroup" [createPlayer]
      ghost     = objectGroup "ghostGroup" [createChefinho, createCrash, createBoo]
      fruit     = objectGroup "fruitGroup" [createFruit]
      groups    = [player, ghost, fruit]
      initScore = Score 0
      input = [
        (SpecialKey KeyRight, StillDown, movePlayerToRight)
        ,(SpecialKey KeyLeft,  StillDown, movePlayerToLeft)
        ,(SpecialKey KeyUp,  StillDown, movePlayerToUp)
        ,(SpecialKey KeyDown,  StillDown, movePlayerToDown)
        ,(Char 'q',            Press,     \_ _ -> funExit)
        ,(Char 'r',            Press, restartGame)
        ]
  bmpList' <- mapM (\(a,f) -> do { a' <- getDataFileName ("app/"++a); return (a', f)}) bmpList
  funInit winConfig gameMap groups (Level 1) initScore input gameCycle (Timer 50) bmpList'

restartGame :: Modifiers -> Position -> RunnerAction()
restartGame _ _ = do
  setInitGame

setInitGame :: RunnerAction()
setInitGame = do
  setGameState (Level 1)
  setGameAttribute (Score 0)
  player <- findObject "player" "playerGroup"
  chefinho <- findObject "chefinho" "ghostGroup"
  fruit <- findObject "fruit" "fruitGroup"
  setObjectAsleep False player
  setObjectAsleep False chefinho
  setObjectAsleep False fruit
  setObjectPosition (125,30) player
  setObjectPosition (500,500) chefinho
  setObjectPosition (900,900) fruit

createPlayer :: RunnerObject
createPlayer = let pacTex = Tex (tileSize, tileSize) 4
               in object "player" pacTex False (125,30) (0,0) ()

createChefinho :: RunnerObject
createChefinho = let chefinhoTex = Tex (tileSize, tileSize) 1
               in object "chefinho" chefinhoTex False (500,500) (0,0) ()

createCrash :: RunnerObject
createCrash = let crashTex = Tex (tileSize, tileSize) 3
               in object "crash" crashTex True (300,700) (0,0) ()

createBoo :: RunnerObject
createBoo = let booTex = Tex (tileSize, tileSize) 2
               in object "boo" booTex True (500,500) (0,0) ()

createFruit :: RunnerObject
createFruit = let tex = Tex (tileSize, tileSize) 0
              in object "fruit" tex False (900,900) (0,0) ()


movePlayerToRight :: Modifiers -> Position -> RunnerAction ()
movePlayerToRight _ _= do
       obj <- findObject "player" "playerGroup"
       setObjectSpeed (speed,0) obj
       setObjectCurrentPicture right obj

movePlayerToLeft :: Modifiers -> Position -> RunnerAction ()
movePlayerToLeft _ _ = do
        obj <- findObject "player" "playerGroup"
        setObjectSpeed (-speed,0) obj
        setObjectCurrentPicture left obj

movePlayerToUp :: Modifiers -> Position -> RunnerAction ()
movePlayerToUp _ _ = do
        obj <- findObject "player" "playerGroup"
        setObjectSpeed (0,speed) obj
        setObjectCurrentPicture up obj

movePlayerToDown :: Modifiers -> Position -> RunnerAction ()
movePlayerToDown _ _ = do
        obj <- findObject "player" "playerGroup"
        setObjectSpeed (0,-speed) obj
        setObjectCurrentPicture down obj

gameCycle :: RunnerAction ()
gameCycle = do
        (Score n) <- getGameAttribute
        (Level level) <- getGameState
        printOnScreen (show n) TimesRoman24 (0,0) 1.0 1.0 1.0
        if (n == 5 && level /= 0)
          then setGameState (Level 2)
          else return()
        if (n == 10 && level /= 0)
          then setGameState (Level 3)
          else return()
        case level of
          0 -> do
            printOnScreen (show("Score "++ show(n))) TimesRoman24 (450,500) 1.0 1.0 1.0
            printOnScreen (show("Aperte Q para sair")) TimesRoman10 (450,300) 1.0 1.0 1.0
            printOnScreen (show("Aperte r para recomecar")) TimesRoman10 (440,200) 1.0 1.0 1.0
            chefinho <- findObject "chefinho" "ghostGroup"
            crash <- findObject "crash" "ghostGroup"
            boo <- findObject "boo" "ghostGroup"
            player <- findObject "player" "playerGroup"
            fruit <- findObject "fruit" "fruitGroup"
            setObjectAsleep True chefinho
            setObjectAsleep True crash
            setObjectAsleep True boo
            setObjectAsleep True player
            setObjectAsleep True fruit
          1 -> levelOne (Score n)
          2 -> levelTwo (Score n)
          3 -> levelThree (Score n)

setNewMap :: Int -> RunnerAction ()
setNewMap 1 = setCurrentMapIndex 0
setNewMap 2 = setCurrentMapIndex 1
setNewMap 3 = setCurrentMapIndex 2
setNewMap _ = return ()

-- checkWallCollision :: RunnerObject -> RunnerAction()
-- checkWallCollision player = do
--   playerPos <- getObjectPosition player
--   tile <- getTileFromWindowPosition playerPos
--   if (getBlockedTile tile) then
--     do


levelOne :: GameAttribute ->  RunnerAction()
levelOne (Score n) = do
  setNewMap 1
  chefinho <- findObject "chefinho" "ghostGroup"
  player <- findObject "player" "playerGroup"
  fruit <- findObject "fruit" "fruitGroup"
  setObjectAsleep False chefinho
  movingGhost 2.0 chefinho player
  movingPlayer
  colChefinhoPlayer <- objectsCollision chefinho player
  colFruitPlayer <- objectsCollision fruit player
  if colChefinhoPlayer
    then do
      setGameState (Level 0)
    else do
      if colFruitPlayer
        then do
          x <- randomDouble (100,900)
          y <- randomDouble (100,900)
          setObjectPosition (x,y) fruit
          setGameAttribute (Score (n+1))
        else return()

levelTwo :: GameAttribute ->  RunnerAction()
levelTwo (Score n) = do
  setNewMap 2
  chefinho <- findObject "chefinho" "ghostGroup"
  crash <- findObject "crash" "ghostGroup"
  player <- findObject "player" "playerGroup"
  fruit <- findObject "fruit" "fruitGroup"
  setObjectAsleep False chefinho
  setObjectAsleep False crash
  movingGhost 2.0 chefinho player
  movingGhost 3.7 crash player
  movingPlayer
  colChefinhoPlayer <- objectsCollision chefinho player
  colCrashPlayer <- objectsCollision crash player
  colFruitPlayer <- objectsCollision fruit player
  if (or [colChefinhoPlayer, colCrashPlayer])
    then do
      setGameState (Level 0)
    else do
      if colFruitPlayer
        then do
          x <- randomDouble (100,900)
          y <- randomDouble (100,900)
          setObjectPosition (x,y) fruit
          setGameAttribute (Score (n+1))
        else return()

levelThree :: GameAttribute ->  RunnerAction()
levelThree (Score n) = do
  setNewMap 3
  chefinho <- findObject "chefinho" "ghostGroup"
  crash <- findObject "crash" "ghostGroup"
  boo <- findObject "boo" "ghostGroup"
  player <- findObject "player" "playerGroup"
  fruit <- findObject "fruit" "fruitGroup"
  setObjectAsleep False chefinho
  setObjectAsleep False crash
  setObjectAsleep False boo
  movingGhost 2.5 chefinho player
  movingGhost 3.0 crash player
  movingGhost 5.5 boo player
  movingPlayer
  colChefinhoPlayer <- objectsCollision chefinho player
  colCrashPlayer <- objectsCollision crash player
  colBooPlayer <- objectsCollision boo player
  colFruitPlayer <- objectsCollision fruit player
  if (or [colChefinhoPlayer, colCrashPlayer, colBooPlayer])
    then do
      setGameState (Level 0)
    else do
      if colFruitPlayer
        then do
          x <- randomDouble (100,900)
          y <- randomDouble (100,900)
          setObjectPosition (x,y) fruit
          setGameAttribute (Score (n+1))
        else return()

movingGhost :: Double -> RunnerObject -> RunnerObject -> RunnerAction()
movingGhost speed ghost player = do
  (playerPosX, playerPosY) <- getObjectPosition player
  (ghostPosX, ghostPosY) <- getObjectPosition ghost
  if (ghostPosX > playerPosX)
    then do setObjectPosition (ghostPosX-speed,ghostPosY) ghost
    else setObjectPosition (ghostPosX+speed,ghostPosY) ghost
  (ghostPosX, ghostPosY) <- getObjectPosition ghost
  if (ghostPosY > playerPosY)
    then setObjectPosition (ghostPosX, ghostPosY-speed) ghost
    else setObjectPosition (ghostPosX, ghostPosY+speed) ghost

movingPlayer :: RunnerAction()
movingPlayer = do
  obj <- findObject "player" "playerGroup"
  (pX, pY) <- getObjectPosition obj
  (sX, sY) <- getObjectSize obj
  boundPlayerRight pX pY sX sY obj
  boundPlayerLeft pX pY sX sY obj
  boundPlayerUp pX pY sX sY obj
  boundPlayerDown pX pY sX sY obj
  collisionWithBlock obj (pX,pY)

collisionWithBlock :: RunnerObject -> (Double, Double) -> RunnerAction()
collisionWithBlock obj (pX,pY) = do
  tile <- getTileFromWindowPosition (pX,pY)
  (vX, vY) <- getObjectSpeed obj
  if (getTileBlocked tile)
    then do
      setObjectPosition (pX-vX,pY-vY) obj
      setObjectSpeed (0,0) obj
    else return ()


boundPlayerRight :: Double -> Double -> Double -> Double -> RunnerObject -> RunnerAction()
boundPlayerRight pX pY sX _ obj = do
  if pX + (sX/2) >= 1000
    then setObjectPosition (0, pY) obj
    else return ()

boundPlayerLeft :: Double -> Double -> Double -> Double -> RunnerObject -> RunnerAction()
boundPlayerLeft pX pY sX _ obj = do
  if pX < 0
    then setObjectPosition (1000 - sX, pY) obj
    else return ()

boundPlayerUp :: Double -> Double -> Double -> Double -> RunnerObject -> RunnerAction()
boundPlayerUp pX pY _ sY obj = do
  if pY + (sY/2) > 1000
    then setObjectPosition (pX, 0) obj
    else return ()

boundPlayerDown :: Double -> Double -> Double -> Double -> RunnerObject -> RunnerAction()
boundPlayerDown pX pY _ sY obj = do
  if pY < 0
    then setObjectPosition (pX, 1000-sY) obj
    else return ()
