{-
   Another RayCasting experiment. 

   Copyright 2012 Joel Svensson  
-} 

module Main where 

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as FONT

import Control.Monad
import qualified Control.Monad.State.Strict as S 

import Data.Word
import Data.Int
import Data.List hiding (intersect)
import Data.Maybe

import CExtras
import MathExtras

import Debug.Trace
----------------------------------------------------------------------------
-- A world of Walls
data World = World {worldWalls :: [Wall]} 

----------------------------------------------------------------------------
-- A wall is either a portal or a visible wall
data Wall 
     = Portal Line Vector2D World 
     --  The vector is the Normal (pointing in whatever direction is into the world) 
     | Wall Line Int
            
mkWall :: Point2D -> Point2D -> Int -> Wall             
mkWall p1 p2 ident  = Wall (Line p1 p2) ident 
mkPortal :: Point2D -> Point2D -> Vector2D -> World -> Wall 
mkPortal p1 p2 v world = Portal (Line p1 p2) v world
            

----------------------------------------------------------------------------
-- test 
{- 
testWorld1 = World [mkWall (-256,-256) (-256, 256) 1, 
                    mkWall (-256, 256) (-128, 384) 2, 
                    mkWall (-128, 384) ( 0  , 256) 3,
                    mkWall ( 0  , 256) ( 256, 256) 4, 
                    mkWall ( 256, 256) ( 256,-256) 5, 
                    mkWall ( 256,-256) (-256,-256) 6]
-} 

testWorld1 = World [mkPortal ( 0, 0) ( 0, 256) (1,0) testWorld2, 
                    mkWall   ( 0, 256) ( 256, 256) 2,
                    mkWall   ( 256, 256) ( 256, 0) 3, 
                    mkWall   ( 256, 0) ( 0, 0) 4] 
                   
                   
                   
testWorld2 = World [mkPortal ( 0, 0) ( 0, 256) (-1,0) testWorld1,
                    mkWall   ( 0, 256) (-512, 256) 5,
                    mkWall   (-512, 256) (-512, 0) 6,
                    mkWall   (-512, 0) ( 0, 0) 7]

----------------------------------------------------------------------------
-- some constants
viewDistance   = floori_ (fromIntegral windowWidth * 0.6)  
walkSpeed      = wallWidth `div` 16

lightRadius    = 128.0


wallHeight, wallWidth :: Int32 
wallHeight      = 256
wallWidth       = 256

textureWidth, textureHeight :: Int32 
textureWidth    = 256 
textureHeight   = 256


viewerHeight    = wallHeight `div` 2
viewportCenterY = windowHeight `div` 2
viewportCenterX = windowWidth `div` 2

windowWidth     = 800  -- number of rays ! 
windowHeight    = 600

----------------------------------------------------------------------------
-- Slice : One slice of wall

data Slice = Slice {sliceTop :: Int32,
                    sliceBot :: Int32, 
                    sliceTex :: Int32,
                    sliceTexCol :: Int32,
                    sliceIntensity :: Float}

type Angle = Float 

---------------------------------------------------------------------------- 
-- castRay
 
castRay :: World -> Point2D -> Angle -> Int32 -> Slice 
castRay world pos angle column = Slice top bot texValue texCol (min 1.0 (lightRadius/dist))  
  where 
    ray  = mkRay pos (angle + columnAngle)
    columnAngle = atan $ fromIntegral col / fromIntegral viewDistance
    top  = bot - height 
    bot  = floori_ $ fromIntegral viewportCenterY + (fromIntegral height / 2) 
    height = floori_ $ fromIntegral (viewDistance * wallHeight) / dist
    dist = dist' * cos columnAngle
    col = column - viewportCenterX
    (dist', texValue, texCol) = castRay2 world ray 
 
   


----------------------------------------------------------------------------
-- castRay2



wallIntersect :: Ray -> Wall -> Maybe Point2D    
wallIntersect ray (Wall line id)  = intersect ray line   
wallIntersect ray (Portal line v _) = if (vecDot (rayDeltas ray) v <= 0.0) 
                                      then intersect ray line
                                      else Nothing 
distanceAlongLine p (Line s _) 
  = distance p s                                    
                                    
castRay2 :: World ->  Ray  -> (Float,Int32,Int32)
castRay2 world ray = 
         case wall of 
            (Just (Wall _ _)) -> (d,1,offset) -- cheat a bit for now  
            (Just (Portal _ _ w')) -> {-trace "Portal" $-}  castRay2 w' ray 
            Nothing -> (d,1,offset)
  where 
    walls = worldWalls world              
    
    -- test intersection against all walls 
    intersections = map (wallIntersect ray) walls
    
    distances     = [(distance (rayStart ray) p,p,Just l) | (Just p,l) <- zip intersections walls]
    dist'         = sortBy (\(x,_,_) (y,_,_) -> compare x y) distances  
    dist          = if null dist' then (1024.0,(0,0),Nothing) else (head dist')
    
    -- TODO: Clean this mess up 
                                                                   
    d = (\(x,_,_) -> x) dist 
    offset = tex dist 
    wall = (\(_,_,w) -> w) dist      
                    
    tex (_,p,Just (Wall l _)) = floori_ (distanceAlongLine p l)  `mod` textureWidth
    tex (_,_,Just (Portal _ _ _) ) = 45
    tex (_,_,Nothing) = 32         
                       
    minimum' [(a,i)] = (a,i) 
    minimum' ((a1,i1):(a2,i2):xs) = if a1 < a2 
                                    then minimum' ((a1,i1):xs)
                                    else minimum' ((a2,i2):xs)

-- TODO: improve on these (better names)   
    
posRayDx  (Ray _ (dx,_)) = dx > 0   
posRayDy  (Ray _ (_,dy)) = dy > 0 
rayX      (Ray (x,_) _) = x
rayY      (Ray (_,y) _) = y 
rayDx     (Ray _ (dx,_)) = dx 
rayDy     (Ray _ (_,dy)) = dy 
rayStart  (Ray s _) = s 
rayDeltas (Ray _ d) = d 

---------------------------------------------------------------------------- 
-- 
type Vector2D = (Float,Float) 
type Point2D  = (Float,Float) -- floats now ? 

vecAdd (x,y) (ax,ay) = (x+ax,y+ay)
vecDot (x1,y1) (x2,y2) = x1*x2 + y1*y2

data Ray     = Ray  Point2D Vector2D -- Point direction representation    

mkRay :: Point2D -> Float -> Ray 
mkRay p r    = Ray p (-1024.0*sin r, 1024.0*cos r)  

data Line    = Line Point2D Point2D -- two points on the line  

distance :: Point2D -> Point2D -> Float
distance (x1, y1) (x2, y2) = 
  sqrt $ xd*xd+yd*yd
    where 
      xd = x2 - x1
      yd = y2 - y1

-- Intersection between ray and line. 
-- TODO: should there be a case for coincident ray/line 
      
pointOnSegment (x,y) (lx,ly) (lx',ly') = (x >= (min lx lx') && x <= (max lx lx'))   && 
                                         (y >= (min ly ly') && y <= (max ly ly'))

      
intersect :: Ray -> Line -> Maybe Vector2D 
intersect (Ray p1 d1) (Line p2 d2) = if det == 0 
                                     then Nothing 
                                     else 
                                       if pointOnSegment (x,y) p2 d2 &&
                                          pointOnSegment (x,y) p1 (p1 `vecAdd` d1)
                                       then 
                                            Just (x,y) 
                                       else Nothing
 where  
   (a1,b1,c1) = convertRay p1 d1
   (a2,b2,c2) = convertLine p2 d2 
   det = a1*b2 - a2*b1
   
   x' = (b2*c1 - b1*c2)
   y' = (a1*c2 - a2*c1)

   x = x' / det
   y = y' / det

 
convertRay  (x, y) (dx, dy) = (a,b,c) 
  where 
    a = dy             -- (y+dy) - y  
    b = -dx            -- x - (x+dx)
    c = a*x+b*y
   

convertLine (x1,y1) (x2,y2) = (a,b,c) 
  where 
    a = y2 - y1 
    b = x1 - x2
    c = a*x1+b*y1 

----------------------------------------------------------------------------
-- rendering 
renderWalls :: World -> Point2D -> Angle -> [Surface] -> Surface -> IO [Slice]
renderWalls world pos angle textures surf = 
  do 
    zipWithM_ (drawSlice textures surf) [0..windowWidth-1] slices 
    return slices
  where 
    slices = map (castRay world pos angle)  [0..windowWidth-1]
    
drawSlice :: [Surface] -> Surface -> Int32 -> Slice -> IO () 
drawSlice textures surf col slice = 
  texVLineLit (fromIntegral col) 
              (fromIntegral (sliceTop slice)) 
              (fromIntegral (sliceBot slice)) 
              surf 
              (fromIntegral (sliceTexCol slice))
              0 
              (fromIntegral textureHeight) 
              (textures  !! (fromIntegral (sliceTex slice - 1)))
              (sliceIntensity slice) 
  

----------------------------------------------------------------------------
-- Main !
main = do 
  SDL.init [InitEverything] 
  FONT.init  
    
  fnt <- openFont "Data/LiberationMono-Regular.ttf" 14 
    
  setVideoMode (fromIntegral windowWidth) 
               (fromIntegral windowHeight) 32 []

  
  screen <- getVideoSurface
  -- toggleFullscreen screen
  
  
  
  let pf = surfaceGetPixelFormat screen
  
  wallTextures <- sequence [conv pf =<< loadBMP "Data/textureLarge1.bmp"
                           ,conv pf =<< loadBMP "Data/textureLarge2.bmp"]
                 

  initialTicks <- getTicks
  eventLoop screen wallTextures -- testTexture floorTex
    testWorld1 
    fnt
    initialTicks 
    0
    0.0
    (False,False,False,False) -- Keyboard state
    (0.0,128 ,128)
  
  FONT.quit
  SDL.quit
    where 
      conv pf t = convertSurface t pf []
  
----------------------------------------------------------------------------
-- process events and draw graphics 
eventLoop :: Surface 
             -> [Surface] 
             -> World
             -> Font
             -> Word32
             -> Int32
             -> Float
             -> (Bool,Bool,Bool,Bool) 
             -> (Float,Float, Float) 
             -> IO ()
eventLoop screen wallTextures currWorld fnt ticks frames fps (up,down,left,right) (r,x,y) = do 
  
  let pf = surfaceGetPixelFormat screen
  
  pix <- mapRGB pf 16 16 16 
  
  -- Clear screen
  fillRect screen 
           (Just (Rect 0 0 (fromIntegral windowWidth) (fromIntegral windowHeight))) 
           pix
  
  -- draw all walls
  renderWalls currWorld (x,y) r wallTextures screen

  ticks2 <- getTicks 
  let (ticks',fps') = if ( ticks2 - ticks >= 1000)                            
                      then (ticks2,fromIntegral frames / (fromIntegral ticks' / 1000))
                      else (1,fps)     
  
  txt <- renderTextSolid fnt ("FPS: " ++ show fps') (Color 255 255 255)
  blitSurface txt Nothing screen Nothing

  SDL.flip screen
  
  -- process events 
  e <- pollEvent
  
  
  
  let (up',down',left',right',b) = 
        case e of 
          (KeyDown k) -> 
            case (symKey k) of 
              SDLK_LEFT    -> (up,down,True,right,False)
              SDLK_RIGHT   -> (up,down,left,True,False)
              SDLK_UP      -> (True,down,left,right,False)
              SDLK_DOWN    -> (up,True,left,right,False)
              SDLK_ESCAPE  -> (up,down,left,right,True)
              otherwise    -> (up,down,left,right,False)
          (KeyUp k) -> 
            case (symKey k) of 
              SDLK_LEFT  -> (up,down,False,right,False)
              SDLK_RIGHT -> (up,down,left,False,False)
              SDLK_UP    -> (False,down,left,right,False)
              SDLK_DOWN  -> (up,False,left,right,False)
              otherwise  -> (up,down,left,right,False)
          Quit -> (up,down,left,right,True) 
          otherwise -> (up,down,left,right,False)
  let (r',x',y') = (moveLeft left' . moveRight right' . moveUp up' . moveDown down') (r,x,y)         
          
  let portals       = filter isPortal (worldWalls currWorld)
  let portIntersect = filter ((/= Nothing) . fst) [(intersect (Ray (x,y) (x'-x,y'-y)) l,p) | p@(Portal l _ _) <-  portals]  
  let currWorld'    = 
        case (map snd portIntersect) of 
          [] -> currWorld
          [(Portal _ _ world')] -> world'
          _ -> error "what!"
      
  unless b $ eventLoop screen wallTextures currWorld' fnt ticks' (frames+1) fps' (up',down',left',right') (r',x',y')     
  
  where 
    
    moveLeft  b (r,x,y) = if b then (r-0.04,x,y) else (r,x,y) 
    moveRight b (r,x,y) = if b then (r+0.04,x,y) else (r,x,y) 
    moveUp    b (r,x,y) = if b && movementAllowed (x',y') then (r,x',y')   else (r,x,y) 
      where 
        x' = x - (fromIntegral walkSpeed*sin r)
        y' = y + (fromIntegral walkSpeed*cos r)
    moveDown  b (r,x,y) = if b && movementAllowed (x',y') then (r,x',y')   else (r,x,y) 
      where 
        x' = x + (fromIntegral walkSpeed*sin r)
        y' = y - (fromIntegral walkSpeed*cos r)
    movementAllowed (px,py) = True -- Just allow it. for now.

    
    isPortal (Portal _ _ _) = True
    isPortal _ = False