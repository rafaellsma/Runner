module Main where
import Graphics.UI.Fungen
import Paths_runner (getDataFileName)

data GameAttribute = Score Int Int
data GameState = Level Int | GameInit Int
type RunnerObject = GameObject ()
type RunnerAction a = IOGame GameAttribute () GameState TileAttribute a

data TileAttribute = NoTileAttribute
type RunnerTile = Tile TileAttribute
type RunnerMap = TileMatrix TileAttribute

screenWidth, screenHeight :: Int
screenWidth = 1000;
screenHeight = 1000;

speed :: Double
speed = 50.0

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
           ("background.bmp", Nothing),
           ("block.bmp", Nothing),
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
map3 = [[b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b],
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
        [b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b]]

main :: IO ()
main = do
  let winConfig = ((0,0),(screenWidth,screenHeight),"Runner")
      gameMap = multiMap [(tileMap map1 tileSize tileSize),
                          (tileMap map2 tileSize tileSize),
                          (tileMap map3 tileSize tileSize),
                          (colorMap 1.0 1.0 1.0 1000 1000)] 3
      player    = objectGroup "playerGroup" [createPlayer]
      ghost     = objectGroup "ghostGroup" [createChefinho, createCrash, createBoo]
      fruit     = objectGroup "fruitGroup" [createFruit]
      groups    = [player, ghost, fruit]
      initScore = Score 0 15
      input = [
        (SpecialKey KeyRight, StillDown, movePlayerToRight)
        ,(SpecialKey KeyLeft,  StillDown, movePlayerToLeft)
        ,(SpecialKey KeyUp,  StillDown, movePlayerToUp)
        ,(SpecialKey KeyDown,  StillDown, movePlayerToDown)
        ,(Char 'q',            Press,     \_ _ -> funExit)
        ,(Char 'r',            Press, restartGame)
        ]
  bmpList' <- mapM (\(a,f) -> do { a' <- getDataFileName ("app/"++a); return (a', f)}) bmpList
  funInit winConfig gameMap groups (GameInit 1) initScore input gameCycle (Timer 50) bmpList'

restartGame :: Modifiers -> Position -> RunnerAction()
restartGame _ _ = do
  setInitGame

setInitGame :: RunnerAction()
setInitGame = do
  setGameState (GameInit 1)
  setGameAttribute (Score 0 15)
  setInitPositions

createPlayer :: RunnerObject
createPlayer = let pacTex = Tex (tileSize, tileSize) 4
               in object "player" pacTex True (125,80) (0,0) ()

createChefinho :: RunnerObject
createChefinho = let chefinhoTex = Tex (tileSize, tileSize) 1
               in object "chefinho" chefinhoTex True (500,500) (0,0) ()

createCrash :: RunnerObject
createCrash = let crashTex = Tex (tileSize, tileSize) 3
               in object "crash" crashTex True (300,700) (0,0) ()

createBoo :: RunnerObject
createBoo = let booTex = Tex (tileSize, tileSize) 2
               in object "boo" booTex True (500,500) (0,0) ()

createFruit :: RunnerObject
createFruit = let tex = Tex (tileSize, tileSize) 0
              in object "fruit" tex True (900,900) (0,0) ()


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
        (Score n timer) <- getGameAttribute
        state <- getGameState
        case state of
          (GameInit levelInit) -> do
            setInitPositions
            setNewMap 4
            printOnScreen (show("Nivel") ++ show levelInit) TimesRoman24 (450,500) 0.0 0.0 0.0
            setGameAttribute (Score n (timer-1))
            if (timer-1) == 0
              then setGameState (Level levelInit)
              else return ()
          (Level level) -> do
            printOnScreen (show n) TimesRoman24 (0,0) 1.0 1.0 1.0
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
              1 -> levelOne (Score n 0)
              2 -> levelTwo (Score n 0)
              3 -> levelThree (Score n 0)


setNewMap :: Int -> RunnerAction ()
setNewMap 1 = setCurrentMapIndex 0
setNewMap 2 = setCurrentMapIndex 1
setNewMap 3 = setCurrentMapIndex 2
setNewMap 4 = setCurrentMapIndex 3
setNewMap _ = return ()

-- checkWallCollision :: RunnerObject -> RunnerAction()
-- checkWallCollision player = do
--   playerPos <- getObjectPosition player
--   tile <- getTileFromWindowPosition playerPos
--   if (getBlockedTile tile) then
--     do


levelOne :: GameAttribute ->  RunnerAction()
levelOne (Score n _) = do
  setNewMap 1
  chefinho <- findObject "chefinho" "ghostGroup"
  player <- findObject "player" "playerGroup"
  fruit <- findObject "fruit" "fruitGroup"
  setObjectAsleep False chefinho
  setObjectAsleep False player
  setObjectAsleep False fruit
  movingGhost (speed * 0.10) chefinho player
  movingPlayer
  colChefinhoPlayer <- objectsCollision chefinho player
  colFruitPlayer <- objectsCollision fruit player
  if colChefinhoPlayer
    then do
      setGameState (Level 0)
    else do
      if colFruitPlayer
        then do
          setGameAttribute (Score (n+1) 0)
          spawnFood fruit
          changeLevel (n+1)
        else return()

levelTwo :: GameAttribute ->  RunnerAction()
levelTwo (Score n _) = do
  setNewMap 2
  chefinho <- findObject "chefinho" "ghostGroup"
  crash <- findObject "crash" "ghostGroup"
  player <- findObject "player" "playerGroup"
  fruit <- findObject "fruit" "fruitGroup"
  setObjectAsleep False chefinho
  setObjectAsleep False crash
  setObjectAsleep False player
  setObjectAsleep False fruit
  movingGhost (speed * 0.10) chefinho player
  movingGhost (speed * 0.10) crash player
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
          setGameAttribute (Score (n+1) 0)
          spawnFood fruit
          changeLevel (n+1)
        else return()

levelThree :: GameAttribute ->  RunnerAction()
levelThree (Score n _) = do
  setNewMap 3
  chefinho <- findObject "chefinho" "ghostGroup"
  crash <- findObject "crash" "ghostGroup"
  boo <- findObject "boo" "ghostGroup"
  player <- findObject "player" "playerGroup"
  fruit <- findObject "fruit" "fruitGroup"
  setObjectAsleep False chefinho
  setObjectAsleep False crash
  setObjectAsleep False boo
  setObjectAsleep False player
  setObjectAsleep False fruit
  movingGhost (speed * 0.10) chefinho player
  movingGhost (speed * 0.10) crash player
  movingGhost (speed * 0.10) boo player
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
          setGameAttribute (Score (n+1) 0)
          spawnFood fruit
        else return()

spawnFood :: RunnerObject -> RunnerAction()
spawnFood fruit = do
  x <- randomDouble (50,950)
  y <- randomDouble (50,950)
  tile <- getTileFromWindowPosition (x,y)
  if getTileBlocked tile
    then spawnFood fruit
    else setObjectPosition (x,y) fruit

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
  boundPlayerRight pX pY obj
  boundPlayerLeft pX pY obj
  boundPlayerUp pX pY obj
  boundPlayerDown pX pY obj
  collisionWithBlock obj (pX,pY)

collisionWithBlock :: RunnerObject -> (Double, Double) -> RunnerAction()
collisionWithBlock obj (pX,pY) = do
  if(pX < 1000 && pX >= 0 && pY < 1000 && pY >= 0)
    then do
      tile <- getTileFromWindowPosition (pX,pY)
      (vX, vY) <- getObjectSpeed obj
      if (getTileBlocked tile)
        then do
          setObjectSpeed (0,0) obj
          setObjectPosition (pX - vX,pY - vY) obj
        else return ()
    else return ()


boundPlayerRight :: Double -> Double -> RunnerObject -> RunnerAction()
boundPlayerRight pX pY obj = do
  if pX > 1000
    then setObjectPosition (0+(speed/2), pY) obj
    else return ()

boundPlayerLeft :: Double -> Double -> RunnerObject -> RunnerAction()
boundPlayerLeft pX pY obj = do
  if pX < 0
    then setObjectPosition (1000-(speed/2), pY) obj
    else return ()

boundPlayerUp :: Double -> Double -> RunnerObject -> RunnerAction()
boundPlayerUp pX pY obj = do
  if pY > 1000
    then setObjectPosition (pX, 0+(speed/2)) obj
    else return ()

boundPlayerDown :: Double -> Double -> RunnerObject -> RunnerAction()
boundPlayerDown pX pY obj = do
  if pY < 0
    then setObjectPosition (pX, 1000-(speed/2)) obj
    else return ()

setInitPositions :: RunnerAction()
setInitPositions = do
  player <- findObject "player" "playerGroup"
  chefinho <- findObject "chefinho" "ghostGroup"
  crash <- findObject "crash" "ghostGroup"
  boo <- findObject "boo" "ghostGroup"
  fruit <- findObject "fruit" "fruitGroup"
  setObjectAsleep True player
  setObjectAsleep True chefinho
  setObjectAsleep True crash
  setObjectAsleep True boo
  setObjectAsleep True fruit
  setObjectPosition (125,125) player
  setObjectSpeed (0,0) player
  setObjectPosition (500,500) chefinho
  setObjectPosition (100,800) boo
  setObjectPosition (300,700) crash
  setObjectPosition (900,900) fruit

changeLevel :: Int -> RunnerAction()
changeLevel n = do
  if (n == 5)
    then do
      setGameState (GameInit 2)
      setGameAttribute (Score n 15)
    else return()
  if (n == 10)
    then do
      setGameState (GameInit 3)
      setGameAttribute (Score n 15)
    else return()
