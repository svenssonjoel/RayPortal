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

import Foreign.Ptr 
import Foreign.Storable
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
     -- The Int is an identifyer that points out what texture to use. 
            
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
testWorld1 = World [mkWall (-512,-512) (-512, 512) 1, 
                    mkWall (-512, 512) ( 512, 512) 2,
                    mkWall ( 512, 512) ( 512,-512) 3, 
                    mkWall ( 512,-512) (-512,-512) 4]

-}
testWorld1 = World [mkWall (-512,-512) (-512,-128) 1, 
                    mkWall (-512,-128) (-640, 0)   1, 
                    mkWall (-640, 0  ) (-512, 128) 1,  
                    mkWall (-512, 128) (-512, 512) 1,  
                    
                    mkWall (-512, 512) ( 512, 512) 2,
                    mkWall ( 512, 512) ( 512,-512) 3, 
                    mkWall ( 512,-512) (-512,-512) 4]

{-
testWorld1 = World [mkPortal ( 0, 0) ( 0, 256) (1,0) testWorld2, 
                    mkWall   ( 0, 256) ( 256, 256) 2,
                    mkWall   ( 256, 256) ( 256, 0) 3, 
                    mkWall   ( 256, 0) ( 0, 0) 4] 
                   
                   
                   
testWorld2 = World [mkPortal ( 0, 0) ( 0, 256) (-1,0) testWorld1,
                    mkWall   ( 0, 256) (-512, 256) 5,
                    mkWall   (-512, 256) (-512, 0) 6,
                    mkWall   (-512, 0) ( 0, 0) 7]
-}
----------------------------------------------------------------------------
-- some constants

{- 
   Set up for a 60degree field of view. 
   FOV = atan (windowWidth / viewDistance) 
       = atan (windowWidth / windowWidth * 0.6) 
       = atan (1 / 0.6)  
       ~ 60degrees 
-} 
viewDistance   = floori_ (fromIntegral windowWidth * 0.6)  
walkSpeed      = wallWidth `div` 16
maxVisible     = 2048

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
                    sliceIntensity :: Float,
                    sliceDistance :: Float}
-- Having sliceDistance and Top,Bot, Intensity is a bit redundant. 
-- Top,Bot and Intensity can be computed from Distance.


type Angle = Float 

---------------------------------------------------------------------------- 
-- castRay
 
castRay :: World -> Point2D -> Angle -> Int32 -> Slice 
castRay world pos angle column = Slice top bot texValue texCol (min 1.0 (lightRadius/dist)) dist  
  where 
    ray = mkRay pos (angle - colAngle) 
    col = column - viewportCenterX
    colAngle = atan $ fromIntegral col / fromIntegral viewDistance
    
    top  = bot - height 
    bot  = floori_ $ fromIntegral viewportCenterY + (fromIntegral height / 2) 
    height = floori_ $ fromIntegral (viewDistance * wallHeight) / dist
    
    
    dist = dist' * cos colAngle
    
    (dist', texValue, texCol) = castRay2 world ray 
 
   


----------------------------------------------------------------------------
-- castRay2 (needs a better name) 



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
    dist          = if null dist' 
                    then (fromIntegral maxVisible,(0,0),Nothing) 
                    else  (head dist')
    
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
vecSub (x,y) (ax,ay) = (x-ax,y-ay)
vecDot (x1,y1) (x2,y2) = x1*x2 + y1*y2

data Ray     = Ray  Point2D Vector2D -- Point direction representation    

mkRay :: Point2D -> Float -> Ray 
mkRay p r    = Ray p (-fromIntegral maxVisible*sin r, fromIntegral maxVisible*cos r)  
-- why -sin r :=x and cos r := y  
-- see: http://www.dpfiles.com/dpfileswiki/index.php?title=Black_Art_of_3D_Game_Programming%2C_Chapter_10:_3D_Fundamentals#Rotation
-- (really bad explanaion though) 

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
  

-- TODO: use depth info 
-- TODO: Lighting ? 
drawTransparent :: Surface -> Surface -> Rect -> IO ()                
drawTransparent  tr surf (Rect x y w h)  = 
  do 
    seeThrough <- mapRGB pf 255 0 255 
    targPixels <- castPtr `fmap` surfaceGetPixels surf
    srcPixels  <- castPtr `fmap` surfaceGetPixels tr 
              
                  
    sequence_ [do 
                  pixel <-  peekElemOff srcPixels (fromIntegral (floori_ (fromIntegral i*rx)+(fromIntegral columns)*floori_ (fromIntegral j *ry)))
                  if (Pixel pixel) /= seeThrough 
                  then pokeElemOff targPixels (start+(i*width+j)) (pixel :: Word32) 
                  else return ()
              | i <- [0..w-1] , j <- [0..h-1]] 
                  
    where 
      rx      = (fromIntegral columns / fromIntegral w) 
      ry      = (fromIntegral rows / fromIntegral h) 
 
      start   = x + y * width 
      width   = surfaceGetWidth surf
      pf      = surfaceGetPixelFormat surf
      columns = surfaceGetWidth tr  
      rows    = surfaceGetHeight tr  

----------------------------------------------------------------------------
-- drawTransparentZ with z buffer check and clipping 
-- TODO: Write this fun in C (and compare performance)
--       - will it be problematic to pass the depths list to C world (inefficient?) 
--       - store the depths not in a list then ?  
drawTransparentZ :: Surface -> Surface -> Rect -> Float -> [Float] -> IO ()                
drawTransparentZ  tr surf (Rect x y w h) depth depths 
  | outside = return () -- sprite is completely outside of target surface  
  | otherwise = 
    do 
      seeThrough <- mapRGB pf 255 0 255 
      targPixels <- castPtr `fmap` surfaceGetPixels surf
      srcPixels  <- castPtr `fmap` surfaceGetPixels tr 
              
                  
      sequence_ [do 
                  pixel <- peekElemOff srcPixels 
                                       (fromIntegral (floori_ (xJump+(fromIntegral i*rx)))+
                                        (fromIntegral columns)*
                                        fromIntegral (floori_ (yJump+(fromIntegral j *ry))))
                                       
                  -- how bad is it to use a depths list (lookups are linear                      
                  -- but there are only a maximum of viewportWidth lookups per frame.
                  -- Probably bad anyway
                  if ((Pixel pixel) /= seeThrough && depth < (depths !! (clippedX+i)))  
                  then pokeElemOff targPixels (start+(i+width*j)) (pixel :: Word32) 
                  else return ()
                | i <- [0..clippedW-1] , j <- [0..clippedH-1]] 
                  
    where 
      rx      = (fromIntegral columns / fromIntegral w) 
      ry      = (fromIntegral rows / fromIntegral h) 


      -- if completely outside.  
      outside = (x > width || y > height || 
                 x < -w || y < -h)
 
      clippedX = x1' 
      clippedY = y1' 
 
      xJump    = rx * fromIntegral (clippedX - x) --how far to jump in texture
      yJump    = ry * fromIntegral (clippedY - y)

      (x1',y1') = (if x < 0 then 0 else x, if y < 0 then 0 else y) 
      (x2',y2') = (if (x+w) >= width then width-1 else x+w, 
                   if (y+h) >= height then height-1 else y+h) 

      clippedW = x2'-x1'
      clippedH = y2'-y1'



      start   = clippedX + clippedY * width 
      width   = surfaceGetWidth surf
      height  = surfaceGetHeight surf
     
      pf      = surfaceGetPixelFormat surf
      columns = surfaceGetWidth tr  
      rows    = surfaceGetHeight tr  

      

----------------------------------------------------------------------------
-- TODO

data GameState = GameState 

type GS = S.StateT GameState IO           

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
                 
  
  monster <- conv pf =<< loadBMP "Data/eye1.bmp"  

  initialTicks <- getTicks
  eventLoop screen wallTextures -- testTexture floorTex
    monster 
    testWorld1 
    fnt
    initialTicks 
    0
    0.0
    (0,0) 
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
             -> Surface
             -> World
             -> Font
             -> Word32
             -> Int32
             -> Float
             -> (Float,Float) -- monster location 
             -> (Bool,Bool,Bool,Bool) 
             -> (Float,Float, Float) 
             -> IO ()
eventLoop screen wallTextures monster currWorld fnt ticks frames fps (mx,my) (up,down,left,right) (r,x,y) = do 
  
  let pf = surfaceGetPixelFormat screen
  
  pix <- mapRGB pf 16 16 16 
  
  -- Clear screen
  fillRect screen 
           (Just (Rect 0 0 (fromIntegral windowWidth) (fromIntegral windowHeight))) 
           pix
  
  -- draw all walls
  slices <- renderWalls currWorld (x,y) r wallTextures screen
  
  let dists  = map sliceDistance slices 
  
  -- Compute screen coordinates of monster based on its world coordinates. 
  -- DONE: Figure out how to do this.
  -- TODO: Break out into a function    
  let mx' = (mx-x) 
      my' = (my-y)  
            
      monsterViewX = mx' * cos (-r) - my' * sin (-r)
      monsterViewY = my' * cos (-r) + mx' * sin (-r) 
      
      -- 
      a = monsterViewX*monsterViewX
      b = monsterViewY*monsterViewY
      mdist = sqrt (a+b)
      
      
  
  if ( monsterViewY >= 0) 
    then 
    do 
      let 
        mw = fromIntegral $ floori_ (256*(fromIntegral viewDistance/ mdist))
        mh = fromIntegral $ floori_ (256*(fromIntegral viewDistance/ mdist))
        projx = monsterViewX*fromIntegral viewDistance / monsterViewY  
                
        projx_ = (fromIntegral (floori_ projx)) + (400 - (mw `div` 2))
      drawTransparentZ monster screen (Rect projx_ (300-(mh `div` 2)) mw mh) mdist dists
   
    else return ()
         
  ticks2 <- getTicks 
  let (ticks',fps') = if ( ticks2 - ticks >= 1000)                            
                      then (ticks2,fromIntegral frames / (fromIntegral ticks' / 1000))
                      else (1,fps)     
                           
{-                            
 --  renderMsg fnt ("FPS: " ++ show fps') (0,0) screen                          
  -- Causes segfault!  
  txt <- renderTextSolid fnt ("FPS: " ++ show fps') (Color 255 255 255) 
  txt1 <- renderTextSolid fnt ("pos: " ++ show (x,y)) (Color 255 255 255)  
  txt2 <- renderTextSolid fnt ("mpos: " ++ show (monsterViewX,monsterViewY)) (Color 255 255 255)  
  txt3 <- renderTextSolid fnt ("mprojx: " ++ show projx) (Color 255 255 255)  
  txt4 <- renderTextSolid fnt ("morig: " ++ show (mx,my)) (Color 255 255 255)  
  txt5 <- renderTextSolid fnt ("mtrans: " ++ show (mx',my')) (Color 255 255 255)  
  txt6 <- renderTextSolid fnt ("radians: " ++ show r) (Color 255 255 255)  
--  txt3 <- renderTextSolid fnt ("mprojx: " ++ show projx) (Color 255 255 255)  
  
  
  blitSurface txt Nothing screen Nothing
  blitSurface txt1 Nothing screen (Just (Rect 0 15 800 600))
  blitSurface txt2 Nothing screen (Just (Rect 0 30 800 600))
  blitSurface txt3 Nothing screen (Just (Rect 0 45 800 600))
  blitSurface txt4 Nothing screen (Just (Rect 0 60 800 600))
  blitSurface txt5 Nothing screen (Just (Rect 0 75 800 600))
  blitSurface txt6 Nothing screen (Just (Rect 0 100 800 600))
  freeSurface txt
  freeSurface txt1
  freeSurface txt2
  freeSurface txt3
  freeSurface txt4
  freeSurface txt5
  freeSurface txt6
-}   

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
      
  unless b $ eventLoop screen wallTextures monster currWorld' fnt ticks' (frames+1) fps' (mx,my) (up',down',left',right') (r',x',y')     
  
  where 
    
    moveLeft  b (r,x,y) = if b then (r+0.04,x,y) else (r,x,y) 
    moveRight b (r,x,y) = if b then (r-0.04,x,y) else (r,x,y) 
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
    
    
-- Causes segfault     
renderMsg fnt message (x,y) surf 
  = do 
    txt <- renderTextSolid fnt message (Color 255 255 255)  
    blitSurface txt Nothing surf (Just (Rect x y 800 600))
    freeSurface txt
  
    
    