{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module EscapeTheRoom
  ( run,
  )
where

import CodeWorld
import EscapeTheRoom.Levels

-- ###############

-- | Main function
-- ###############
run :: IO ()
run = activityOf (takeNext allLevels) handleWorld drawWorld

-- ############################################

-- | Data declarations and basic pieces of game
-- ############################################

-- | State of the game
data State = State Coords (Coords -> Tile) [DoorColor] Player [Level] Author | Simple Picture

-- | Player emotions
data Player = Basic | Nice | Surprised | Love

-- | Draw wall tile
wallTile :: Picture
wallTile = colored black (solidRectangle 0.95 0.95)

-- | Draw floor tile
floorTile :: Picture
floorTile = colored yellow (solidRectangle 0.95 0.95)

-- | Draw trap tile
trapTile :: Picture
trapTile = floorTile

-- | Draw exit tile
exitTile :: Picture
exitTile =
  colored black (solidRectangle 0.20 0.20)
    <> colored red (solidRectangle 0.40 0.40)
    <> colored black (solidRectangle 0.60 0.60)
    <> colored red (solidRectangle 0.80 0.80)
    <> floorTile

-- | Draw button tile
buttonTile :: DoorColor -> Picture
buttonTile color =
  colored (doorColorToColor color) (solidCircle 0.25)
    <> floorTile

-- | Draw door tile
doorTile :: DoorColor -> Picture
doorTile color =
  colored (doorColorToColor color) (solidCircle 0.25)
    <> wallTile

-- | Render a single tile.
drawTile :: Tile -> Picture
drawTile Void = floorTile
drawTile Floor = floorTile
drawTile (Trap _) = trapTile
drawTile Wall = wallTile
drawTile Exit = exitTile
drawTile (Button color) = buttonTile color
drawTile (Door color) = doorTile color

-- ###################################

-- | Functions to draw the game levels
-- ###################################

-- | Draw tile at the current positon of given level
drawTileAt ::
  (Coords -> Tile) ->
  Coords ->
  Picture
drawTileAt level (Coords i j) = translated x y (drawTile (level (Coords i j)))
  where
    x = fromIntegral i
    y = fromIntegral j

-- | Draw level of a particular size
drawLevelMap ::
  (Coords -> Tile) ->
  Picture
drawLevelMap level =
  myLevelMapPictureBounded level (-30, 30) (-30, 30)

-- | Draw tiles in given range
drawFromTo ::
  (Integer -> Picture) ->
  (Integer, Integer) ->
  Picture
drawFromTo something (from, to)
  | from > to = blank
  | otherwise = something from <> drawFromTo something (from + 1, to)

-- | Draw one column of the level
myLevelMapColumnPicture ::
  (Coords -> Tile) ->
  (Integer, Integer) ->
  Integer ->
  Picture
myLevelMapColumnPicture level (b, t) i = drawFromTo (\j -> drawTileAt level (Coords i j)) (b, t)

-- | Draw few number of colums according to given range
myLevelMapPictureBounded ::
  (Coords -> Tile) ->
  (Integer, Integer) ->
  (Integer, Integer) ->
  Picture
myLevelMapPictureBounded level (b, t) (l, r) =
  drawFromTo (\i -> myLevelMapColumnPicture level (b, t) i) (l, r)

-- #########################################################

-- | Functions to draw player and control player's movements
-- #########################################################

-- | Draw player at the given position
drawPlayerAt ::
  Coords ->
  Player ->
  Picture
drawPlayerAt (Coords i j) player = translated x y (playerToPicture player)
  where
    x = fromIntegral i
    y = fromIntegral j

-- | Get pricture according to player
playerToPicture ::
  Player ->
  Picture
playerToPicture Basic = (lettering "\x1F63C")
playerToPicture Nice = (lettering "\x1F638")
playerToPicture Surprised = (lettering "\x1F640")
playerToPicture Love = (lettering "\x1F63B")

-- | Draw the world with the player
drawWorld :: State -> Picture
drawWorld (State (Coords x y) level doors player _levels author) = translated (-i) (-j) ((drawAuthor author) <> drawPlayerAt (Coords x y) player 
  <> (drawLevelMap (openDoors doors level)))
  where 
      i = fromIntegral x
      j = fromIntegral y
drawWorld (Simple picture) = picture

drawAuthor :: Author -> Picture
drawAuthor author = translated 16 (-5) ((lettering author) <> colored (light pink) (solidRectangle 10 2))

-- | Handle player's movements
handleWorld ::
  Event ->
  State ->
  State
handleWorld (KeyPress "Up") state = tryMove Up state
handleWorld (KeyPress "Down") state = tryMove Down state
handleWorld (KeyPress "Left") state = tryMove Left_ state
handleWorld (KeyPress "Right") state = tryMove Right_ state
handleWorld _ state = state

-- | Movement of player according to tile type
tryMove ::
  Direction ->
  State ->
  State
tryMove direction (State coords level doors player levels author)
  | canMove newCoordsTile doors =
    changeState newCoordsTile (State newCoords level doors player levels author)
  | otherwise = (State coords level doors player levels author)
  where
    newCoords = changeCoords direction coords
    newCoordsTile = level newCoords
tryMove _ state = state

-- | Change coordinates according to direction
changeCoords ::
  Direction ->
  Coords ->
  Coords
changeCoords Up (Coords i j) = Coords i (j + 1)
changeCoords Down (Coords i j) = Coords i (j - 1)
changeCoords Left_ (Coords i j) = Coords (i - 1) j
changeCoords Right_ (Coords i j) = Coords (i + 1) j

-- | Definition of tiles where player can move
canMove ::
  Tile ->
  [DoorColor] ->
  Bool
canMove Void _ = True
canMove Floor _ = True
canMove Wall _ = False
canMove Exit _ = True
canMove (Button _) _ = True
canMove (Trap _) _ = True
canMove (Door dc) colors
  | oneOf dc colors = True
  | otherwise = False

-- ################################################

-- | Functions to handle doors behaviour and states
-- ################################################

-- | Return the level with open doors according to list of open doors
openDoors ::
  [DoorColor] -> -- ˆ Doors to open.
  (Coords -> Tile) -> -- ˆ Original map.
  (Coords -> Tile) -- ˆ Map with doors open.
openDoors [] level x = level x
openDoors colors level x
  | checkOpenDoors colors (level x) = Floor
  | otherwise = level x

-- | Change state of the game according to pressed tile
changeState ::
  Tile ->
  State ->
  State
changeState (Button bc) (State coords level doors _ levels author)
  | not (oneOf bc doors) =
    (State coords level updatedDoors Nice levels author)
  where
    updatedDoors = bc : doors
changeState Exit (State _coords _level _doors _ levels _author) =
  takeNext levels
changeState (Trap direction) (State coords level doors _ levels author) =
  (State coords (closeTrap coords direction level) doors Surprised levels author)
changeState _ (State coords level doors _ levels author) =
  (State coords level doors Basic levels author)
changeState _ state = state

-- | Draw wall tile according to direction by pressing on trap tile
closeTrap ::
  Coords ->
  Direction ->
  (Coords -> Tile) ->
  (Coords -> Tile)
closeTrap (Coords i j) Up level (Coords x y)
  | x == i && (y - 1) == j = Wall
  | otherwise = level (Coords x y)
closeTrap (Coords i j) Down level (Coords x y)
  | x == i && (y + 1) == j = Wall
  | otherwise = level (Coords x y)
closeTrap (Coords i j) Left_ level (Coords x y)
  | (x + 1) == i && y == j = Wall
  | otherwise = level (Coords x y)
closeTrap (Coords i j) Right_ level (Coords x y)
  | (x - 1) == i && y == j = Wall
  | otherwise = level (Coords x y)

-- | Check tile to be door and his door color to belong given list of doors' colors
checkOpenDoors ::
  [DoorColor] ->
  Tile ->
  Bool
checkOpenDoors colors (Door dc)
  | oneOf dc colors = True
  | otherwise = False
checkOpenDoors _ _ = False

-- | Check doors' color to be equal to given doors' color
eqDoorColor :: DoorColor -> DoorColor -> Bool
eqDoorColor c1 c2 
 | (doorColorToColor c1) ==  (doorColorToColor c2) = True
 | otherwise = False

-- | Check doors' color to belong to given list of doors' colors
oneOf :: DoorColor -> [DoorColor] -> Bool
oneOf _ [] = False
oneOf color (x : xs)
  | eqDoorColor color x = True
  | otherwise = oneOf color xs

-- #########################

-- | Multiple levels support
-- #########################

-- | Screen to display at the end af all levels
endScreen :: Picture
endScreen = (lettering "Congratulations! You won!") <> colored (light pink) (solidRectangle 21 21)

-- | Initialise game 'State' for a given 'LevelMap'.
initLevelMap :: [Level] -> Level -> State
initLevelMap levels (Level author coords levelMap doors) = State coords levelMap doors Basic levels author

-- | Get state with the next level
takeNext
  :: [Level] -- ˆ A list of levels.
  -> State
takeNext [] = Simple endScreen
takeNext (level:levels) = initLevelMap levels level