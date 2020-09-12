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
run = activityOf (withManyLevels allLevels initLevelMap isLevelComplete (initLevelMap level1)) handleWorld drawWorld

-- ############################################

-- | Data declarations and basic pieces of game
-- ############################################

-- | Possible types of tiles
-- data Tile = Wall | Floor | Door DoorColor | Button DoorColor | Exit | Trap Direction

-- | Possible types of doors' colors
-- data DoorColor = Blue_ | Red_ | Green_ | Orange_ | Violet_ | Pink_

-- | Coordinates x and y of postion on map
-- data Coords = Coords Integer Integer

-- | Possible directions of payer's movements
-- data Direction = Up | Down | Left_ | Right_

-- | State of the game
data State = State Coords (Coords -> Tile) [DoorColor] Player

-- | Player emotions
data Player = Basic | Nice | Surprised | Love

-- | Make Color from DoorColor
-- doorColorToColor :: DoorColor -> Color
-- doorColorToColor Blue_ = blue
-- doorColorToColor Red_ = red
-- doorColorToColor Green_ = green
-- doorColorToColor Orange_ = orange
-- doorColorToColor Violet_ = purple
-- doorColorToColor Pink_ = pink

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

-- ##############################

-- | Draw first level of the game
-- ##############################
-- level1 :: Coords -> Tile

-- -- | Frame for map
-- level1 (Coords (-10) _) = Wall
-- level1 (Coords 10 _) = Wall
-- level1 (Coords _ (-10)) = Wall
-- level1 (Coords _ 10) = Wall
-- level1 (Coords x y)
--   | (y == (-5) && x >= 7) || y == -4 && x == 1
--       || (y < (-6) && y > (-8)) && x == 1
--       || ((x > 6 || x < 6) && (y == -8))
--       || ((y == -3) && (x < -8 || x > -5))
--       || ((x == 7) && (y == (-6)))
--       || (x == -3) && (y < -3 && y > -7)
--       || (y == 0) && x < 7
--       || (y == -1 && x == 6)
--       || (x == 5 || x == 4) && (y < 10 && y > 6)
--       || (x < 10 && x > 7) && y == 5
--       || (x < 7 && x > 3) && y == 5
--       || (y < 9 && y > 1) && (x == -7 || x == -1)
--       || (y < 8 && y > 0) && (x == -4 || x == 2) =
--     Wall
-- level1 (Coords (-8) (-9)) = Button Red_
-- level1 (Coords 9 (-6)) = Button Red_
-- level1 (Coords x y)
--   | ((x == 3 || x == 4 || x == 5) && (y == -4)) = Button Blue_
-- level1 (Coords x y)
--   | (x == -9) && (y < -3 && y > -8) = Button Pink_
-- level1 (Coords x y)
--   | (x == -9) && (y < 0 && y > -3) = Button Orange_
-- level1 (Coords (-9) 9) = Button Green_
-- level1 (Coords x y)
--   | ((x == 1) && (y == -5 || y == -6)) = Door Red_
-- level1 (Coords x y)
--   | (x > -9 && x < -4) && (y == -3) = Door Blue_
-- level1 (Coords x y)
--   | (x < 10 && x > 6) && (y == 0) = Door Orange_
-- level1 (Coords 5 6) = Door Pink_
-- level1 (Coords 4 6) = Door Green_
-- level1 (Coords x y)
--   | (x > -8 && x < 6) && (y == 9) = Door Green_
-- level1 (Coords 6 (-7)) = Trap Down
-- level1 (Coords 8 (-7)) = Trap Left_
-- level1 (Coords (-4) (-7)) = Trap Right_
-- level1 (Coords 7 (-2)) = Trap Left_
-- level1 (Coords 7 4) = Trap Up
-- level1 (Coords 1 8) = Trap Right_
-- level1 (Coords (-5) 8) = Trap Right_
-- level1 (Coords (-2) 1) = Trap Right_
-- level1 (Coords (-8) 1) = Trap Right_
-- level1 (Coords x y)
--   | (x == 8 || x == 7) && (y == 8 || y == 7) = Exit
-- level1 _ = Floor

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
  myLevelMapPictureBounded level (-10, 10) (-10, 10)

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

-- | Initial postion of the player
-- initialWorld :: State
-- initialWorld = State (Coords (8) (-9)) level1 [] Basic

-- | Draw the world with the player
drawWorld :: State -> Picture
drawWorld (State coords level doors player) = drawPlayerAt coords player <> drawLevelMap (openDoors doors level)

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
tryMove direction (State coords level doors player)
  | canMove newCoordsTile doors =
    changeState newCoordsTile (State newCoords level doors player)
  | otherwise = (State coords level doors player)
  where
    newCoords = changeCoords direction coords
    newCoordsTile = level newCoords

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
changeState (Button bc) (State coords level doors _)
  | not (oneOf bc doors) =
    (State coords level updatedDoors Nice)
  where
    updatedDoors = bc : doors
changeState Exit (State coords level doors _) =
  (State coords level doors Love)
changeState (Trap direction) (State coords level doors _) =
  (State coords (closeTrap coords direction level) doors Surprised)
changeState _ (State coords level doors _) =
  (State coords level doors Basic)

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
-- eqDoorColor Blue_ Blue_ = True
-- eqDoorColor Green_ Green_ = True
-- eqDoorColor Orange_ Orange_ = True
-- eqDoorColor Pink_ Pink_ = True
-- eqDoorColor _ _ = False

-- | Check doors' color to belong to given list of doors' colors
oneOf :: DoorColor -> [DoorColor] -> Bool
oneOf _ [] = False
oneOf color (x : xs)
  | eqDoorColor color x = True
  | otherwise = oneOf color xs

-- #########################

-- | Multiple levels support
-- #########################

-- | Initialise game 'State' for a given 'LevelMap'.
initLevelMap :: Level -> State
initLevelMap (Level _author coords levelMap doors) = State coords levelMap doors Basic

-- | Is current level complete given some game 'State'?
isLevelComplete :: State -> Bool
isLevelComplete (State coords level _ _) =
    case (level coords) of Exit -> True
                           _    -> False

-- endtScreen :: Picture
-- endScreen = colored yellow (solidRectangle 21 21)

-- | Turn an interactive program into one with multiple levels.
withManyLevels
  :: [level] -- ˆ A list of levels.
  -> (level -> world) -- ˆ Initialise world for level.
  -> (world -> Bool) -- ˆ Is this level complete?
  -> world -- ˆ 'interactionOf'.
  -> world
withManyLevels [] _ _ prevWorld = prevWorld
withManyLevels (level:levels) initWorld isComplete prevWorld
  | isComplete prevWorld = withManyLevels levels initWorld isComplete prevWorld
  | otherwise            = initWorld level
