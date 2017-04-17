{-# LANGUAGE RecordWildCards #-}

module Perceptron where

data ActivationFn = BooleanFn | LogisticFn deriving (Show, Enum, Bounded)

data Perceptron = Perceptron
    { weights :: [Float]
    , threshold :: Float
    , rate :: Float
    , activationFn :: ActivationFn }

activate :: Perceptron -> [Float] -> Float
activate Perceptron {..} = case activationFn of
    BooleanFn -> fromIntegral . fromEnum . (> threshold) . sum . zipWith (*) weights
    LogisticFn -> recip . succ . exp . negate . subtract threshold . sum . zipWith (*) weights

learn :: ([Float], Int) -> Perceptron -> Perceptron
learn (inputs, target) Perceptron {..} = Perceptron
    { weights = zipWith learnWeight weights inputs
    , threshold = learnWeight threshold (-1)
    , .. }
    where
    learnWeight weight input =
        weight + rate * (fromIntegral target - activate Perceptron {..} inputs) * input