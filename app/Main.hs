module Main where
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Paths_runner (getDataFileName)

data GameAttribute = Score Int
data GameState = Level Int
type RunnerObject = GameObject ()
type RunnerAction a = IOGame GameAttribute () GameState () a

screenWidth, screenHeight :: Int
screenWidth = 1000;
screenHeight = 1000;

speed :: Double
speed = 30.0

magenta :: InvList
magenta = Just [(255,0,255)]

bmpList :: FilePictureList
bmpList = [("fruit.bmp", magenta),
           ("chefinho.bmp", Nothing),
           ("boo.bmp", Nothing),
           ("crash.bmp", Nothing),
           ("player.bmp", magenta)]

main :: IO ()
main = do
  let winConfig = ((0,0),(screenWidth,screenHeight),"Runner")
      gameMap = (colorMap 0.0 0.0 0.0 1000 1000)
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
  bmpList' <- mapM (\(a,b) -> do { a' <- getDataFileName ("app/"++a); return (a', b)}) bmpList
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
createPlayer = let pacTex = Tex (30, 30) 4
               in object "player" pacTex False (125,30) (0,0) ()

createChefinho :: RunnerObject
createChefinho = let chefinhoTex = Tex (30, 30) 1
               in object "chefinho" chefinhoTex False (500,500) (0,0) ()

createCrash :: RunnerObject
createCrash = let crashTex = Tex (30, 30) 3
               in object "crash" crashTex True (300,700) (0,0) ()

createBoo :: RunnerObject
createBoo = let booTex = Tex (30, 30) 2
               in object "boo" booTex True (500,500) (0,0) ()

createFruit :: RunnerObject
createFruit = let tex = Tex (32.0, 32.0) 0
              in object "fruit" tex False (900,900) (0,0) ()


movePlayerToRight :: Modifiers -> Position -> RunnerAction ()
movePlayerToRight _ _= do
       obj <- findObject "player" "playerGroup"
       (pX,pY) <- getObjectPosition obj
       (sX,_)  <- getObjectSize obj
       if (pX + (sX/2) + speed <= 1000.0)
       	then (setObjectPosition ((pX + speed),pY) obj)
       	else (setObjectPosition ((0 + (sX/2)),pY) obj)

movePlayerToLeft :: Modifiers -> Position -> RunnerAction ()
movePlayerToLeft _ _ = do
        obj <- findObject "player" "playerGroup"
        (pX,pY) <- getObjectPosition obj
        (sX,_)  <- getObjectSize obj
        if (pX - (sX/2) - speed >= 0)
        	then (setObjectPosition ((pX - speed),pY) obj)
        	else (setObjectPosition (1000 - sX/2,pY) obj)

movePlayerToUp :: Modifiers -> Position -> RunnerAction ()
movePlayerToUp _ _ = do
        obj <- findObject "player" "playerGroup"
        (pX,pY) <- getObjectPosition obj
        (_,sY)  <- getObjectSize obj
        if (pY + (sY/2) + speed <= 1000)
        	then (setObjectPosition (pX,(pY + speed)) obj)
        	else (setObjectPosition (pX,1000 - sY/2) obj)

movePlayerToDown :: Modifiers -> Position -> RunnerAction ()
movePlayerToDown _ _ = do
        obj <- findObject "player" "playerGroup"
        (pX,pY) <- getObjectPosition obj
        (_,sY)  <- getObjectSize obj
        if (pY - (sY/2) - speed >= 0)
        	then (setObjectPosition (pX,(pY - speed)) obj)
        	else (setObjectPosition (pX,0 + sY/2) obj)

gameCycle :: RunnerAction ()
gameCycle = do
        (Score n) <- getGameAttribute
        (Level level) <- getGameState
        printOnScreen (show n) TimesRoman24 (0,0) 1.0 1.0 1.0
        if n == 5
          then setGameState (Level 2)
          else return()
        if n == 10
          then setGameState (Level 3)
          else return()
        case level of
          0 -> do
            printOnScreen (show("Perdeu!")) TimesRoman24 (500,500) 1.0 1.0 1.0
            printOnScreen (show("Aperte Q para sair")) TimesRoman10 (500,300) 1.0 1.0 1.0
            printOnScreen (show("Aperte r para recomecar")) TimesRoman10 (480,200) 1.0 1.0 1.0
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


levelOne :: GameAttribute ->  RunnerAction()
levelOne (Score n) = do
  chefinho <- findObject "chefinho" "ghostGroup"
  player <- findObject "player" "playerGroup"
  fruit <- findObject "fruit" "fruitGroup"
  setObjectAsleep False chefinho
  movingGhost 22.0 chefinho player
  colChefinhoPlayer <- objectsCollision chefinho player
  colFruitPlayer <- objectsCollision fruit player
  if colChefinhoPlayer
    then do
      setGameAttribute (Score 0)
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
  chefinho <- findObject "chefinho" "ghostGroup"
  crash <- findObject "crash" "ghostGroup"
  player <- findObject "player" "playerGroup"
  fruit <- findObject "fruit" "fruitGroup"
  setObjectAsleep False chefinho
  setObjectAsleep False crash
  movingGhost 22.0 chefinho player
  movingGhost 23.7 crash player
  colChefinhoPlayer <- objectsCollision chefinho player
  colCrashPlayer <- objectsCollision crash player
  colFruitPlayer <- objectsCollision fruit player
  if (or [colChefinhoPlayer, colCrashPlayer])
    then do
      setGameAttribute (Score 0)
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
  chefinho <- findObject "chefinho" "ghostGroup"
  crash <- findObject "crash" "ghostGroup"
  boo <- findObject "boo" "ghostGroup"
  player <- findObject "player" "playerGroup"
  fruit <- findObject "fruit" "fruitGroup"
  setObjectAsleep False chefinho
  setObjectAsleep False crash
  setObjectAsleep False boo
  movingGhost 22.5 chefinho player
  movingGhost 23.0 crash player
  movingGhost 25.5 boo player
  colChefinhoPlayer <- objectsCollision chefinho player
  colCrashPlayer <- objectsCollision crash player
  colBooPlayer <- objectsCollision boo player
  colFruitPlayer <- objectsCollision fruit player
  if (or [colChefinhoPlayer, colCrashPlayer, colBooPlayer])
    then do
      setGameAttribute (Score 0)
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
