{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Monad
import Data.Bifunctor
import Data.List
import Data.Maybe
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.IO.Game
import Perceptron
import System.Console.ANSI
import System.IO
import System.Random
import Text.Printf
import Text.Read (readMaybe)

data State = State
    { points :: [((Float, Float), Int)]
    , perceptron :: Perceptron
    , running :: Bool
    , windowWidth :: Int
    , windowHeight :: Int
    , zoom :: Float
    , pointRadius :: Float }

data PointsPreset = And | Or | Custom deriving (Show, Enum, Bounded)

randomWeight :: IO Float
randomWeight = randomRIO (-0.1, 0.1)

main :: IO ()
main = do
    putStrLn "To use the default value, just press enter"
    pointsPreset <- askEnum "Choose points" Custom
    activationFn <- askEnum "Activation function" LogisticFn
    rate <- ask "Learning rate" 0.5
    fps <- ask "Frames per second" $ case activationFn of
        BooleanFn -> 3
        LogisticFn -> 30
    threshold : weights <- forM ["threshold", "x weight", "y weight"] $ \x ->
        ask ("Initial " ++ x) =<< randomWeight
    windowWidth <- ask "Window width" 600
    windowHeight <- ask "Window height" 600
    zoom <- ask "Zoom" $ fromIntegral (min windowWidth windowHeight) / 4
    pointRadius <- ask "Point radius" 10
    running <- askEnum "Initially running" True
    let initState = State
            { points = case pointsPreset of
                And -> zip boolPoints [0, 0, 0, 1]
                Or -> zip boolPoints [0, 1, 1, 1]
                Custom -> []
            , perceptron = Perceptron {..}
            , .. }
        boolPoints = [(0, 0), (0, 1), (1, 0), (1, 1)]
        window = InWindow "Perceptron" (windowWidth, windowHeight) (1366 - windowWidth, 0)
    logStateInit initState
    playIO window black fps initState plot listener $ const update
    where
    ask question def = do
        printf "%v (default is %v): " question def
        hFlush stdout
        fromMaybe def . readMaybe <$> getLine
    askEnum :: forall a. (Show a, Enum a, Bounded a) => String -> a -> IO a
    askEnum question def = do
        zipWithM_ (\n x -> putStrLn $ show n ++ ". " ++ show x) [1..] [(minBound :: a)..]
        flip fmap (ask question $ fromEnum def + 1) $ \answer ->
            if answer >= 1 && answer <= fromEnum (maxBound :: a) + 1
                then toEnum $ answer - 1 else def

update :: State -> IO State
update State {..}
    | running = logState State
        { perceptron = foldr learn perceptron $ map (first $ \(x, y) -> [x, y]) points, .. }
    | otherwise = return State {..}

plot :: State -> IO Picture
plot State { perceptron = Perceptron {..}, .. } = return $ Pictures
    $ axis [(-fromIntegral windowWidth / 2, 0), (fromIntegral windowWidth / 2, 0)]
    : axis [(0, -fromIntegral windowHeight / 2), (0, fromIntegral windowHeight / 2)]
    : perceptronLine
    : map plotPoint points
    where
    axis = Color white . Line
    perceptronLine = Color azure $ Line $ flip map [-1, 1] $ \a ->
        fromMaybe (a * zoom, 0) $ intersectLineLine
            (0, zoom * threshold / last weights)
            (zoom * threshold / head weights, 0)
            (0, a * fromIntegral windowHeight / 2)
            (1, a * fromIntegral windowHeight / 2)
    plotPoint ((x, y), t) = Color (if toEnum t then green else red)
        $ Translate (x * zoom) (y * zoom) $ circleSolid pointRadius

listener :: Event -> State -> IO State
listener event state = listener' state event >>= logState

listener' :: State -> Event -> IO State
listener' State {..} (EventKey key Down _ (join bimap (/ zoom) -> (x, y)))
    | MouseButton LeftButton <- key = return $ point 1
    | MouseButton RightButton <- key = return $ point 0
    | SpecialKey KeySpace <- key = return State { running = not running, .. }
    | Char 'l' <- key = return State { perceptron = Perceptron { rate = rate + 0.1, .. }, .. }
    | Char ';' <- key = return State { perceptron = Perceptron { rate = rate - 0.1, .. }, .. }
    | Char 'r' <- key = do
        t : w <- replicateM 3 randomWeight
        return State { perceptron = Perceptron { threshold = t, weights = w, .. }, .. }
    | Char 'c' <- key = return State { points = [], .. }
    where
    point n = State
        { points = case find inPoint points of
            Just pt -> delete pt points
            Nothing -> ((x, y), n) : points
        , .. }
    inPoint ((x1, y1), _) = sqrt ((y - y1) ^ 2 + (x - x1) ^ 2) < pointRadius / zoom
    Perceptron {..} = perceptron
listener' state _ = return state

controls, variables :: [String]
controls =
    [ "Perceptron"
    , ""
    , "Left click to add a green dot"
    , "Right click to add a red dot"
    , "Click on an existing dot to remove it"
    , "Space bar to pause"
    , "'l' to increase learning rate"
    , "';' to decrease learning rate"
    , "'r' to randomize perceptron"
    , "'c' to clear all dots"
    , "" ]
variables =
    [ "X weight: "
    , "Y weight: "
    , "Threshold: "
    , "Learning rate: "
    , "" ]

logStateInit :: State -> IO ()
logStateInit State {..} = do
    clearScreen
    hideCursor
    setCursorPosition 0 0
    mapM_ putStrLn $ controls ++ variables

logState :: State -> IO State
logState State {..} = do
    forM_ (zip3 [0..] variables values) $ \(i, var, val) -> do
        setCursorPosition (length controls + i) $ length var
        putStr val
    return State {..}
    where
    values =
        [ printf "%f" $ head weights
        , printf "%f" $ last weights
        , printf "%f" threshold
        , printf "%f" rate
        , if running then "Running" else "Stopped" ]
    Perceptron {..} = perceptron
