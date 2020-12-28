-- |
-- Module      :  DobutokO.Sound.Complex
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music from a file (or its part). 
-- Uses SoX inside. Is more complicated than dobutokO2 and uses its functionality.

{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Complex (
  -- * Another way to generalize the simple functions of dobutokO2 package
  testSoundGen2G
  , soundGen3G
  , soundGen3G_O
  , soundGen3G_O2
  , soundGen3G_O2G
  -- ** With MN control
  , testSoundGen2GMN
  , soundGen3GMN
  , soundGen3G_OMN
  , soundGen3G_O2MN
  , soundGen3G_O2GMN
  , h1
  , h2
  -- ** With Params control
  , soundGen3G_OPar
  , soundGen3G_O2Par
  , soundGen3G_O2GPar
  , soundGen3G_OMNPar
  , soundGen3G_O2MNPar
  , soundGen3G_O2GMNPar
  , h2Params
) where

import Numeric
import Data.Maybe (fromJust,fromMaybe)
import qualified Data.Vector as V
import System.Process
import EndOfExe
import System.Directory
import GHC.Int (Int64)
import DobutokO.Sound.Keyboard
import DobutokO.Sound.Functional.Params
import DobutokO.Sound.Functional.Basics
import DobutokO.Sound.Functional.Elements

-- | Tesing variant of the 'soundGen3G' with predefined three last functional arguments.
testSoundGen2G :: FilePath -> Float -> String -> IO ()
testSoundGen2G = testSoundGen2GMN (-1) (-1)

-- | Tesing variant of the 'soundGen3GMN' with predefined last functional arguments.
testSoundGen2GMN :: Int64 -> Int64 -> FilePath -> Float -> String -> IO ()
testSoundGen2GMN m n1 file y zs = do
  vecA0 <- fmap (V.map (`quotRem` 108)) ((if m == (-1) && n1 == (-1) then readFileDoubles else readFileDoublesMN m n1) file) -- >>= print
  let n = V.length vecA0
      freq0 j = V.unsafeIndex notes (snd . V.unsafeIndex vecA0 $ j `rem` n)
      f0 t = V.fromList [(0.05763181818181818 * t, 0.3598),(1.112159090909091 * t, 0.4588962),(2 * t, 0.6853),(3 * t, 0.268),(4 * t, 0.6823),(5 * t, 0.53)]
      fA1 j = fAddFElem (freq0 (j + 1),0.5) f0 gAdd04 
      fR1 j = fRemoveFElem (freq0 (j + 1),0.5) f0 gRem03
      vecB = V.imap (\j r -> (V.unsafeIndex notes (snd r),
       case fst r of
         0 -> f0
         1 -> fA1 j
         2 -> fA1 j
         3 -> fA1 j
         4 -> fA1 j
         _ -> fR1 j)) vecA0
      v2 = str2DurationsDef n zs y 
      zeroN = numVZeroesPre vecB in V.imapM_ (\j (x,k) -> do
        h1 (\_ -> k (1.1 * freq0 j)) (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) 
        renameFile ("result.wav") $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") vecB
  endFromResult

-- | Generates a sequence of sounds with changing timbre. Uses several functions as parameters. 
soundGen3G :: FilePath -> Float -> String -> ((Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO) 
  -> ((Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO) -> (Float -> OvertonesO) -> IO ()
soundGen3G = soundGen3GMN (-1) (-1)

-- | Generates a sequence of sounds with changing timbre. Uses several functions as parameters. To specify how many sounds the resulting files 
-- will provide, you use first two 'Int64' arguments, the first of which is a number of dropped elements for 'readFileDoubles' and the second one 
-- is a number of produced sounds (and, respectively, number of taken elements).
soundGen3GMN :: Int64 -> Int64 -> FilePath -> Float -> String -> ((Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO) 
  -> ((Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO) -> (Float -> OvertonesO) -> IO ()
soundGen3GMN m n1 file y zs gAdd gRem f0 = do
  vecA0 <- fmap (V.map (`quotRem` 108)) ((if m == (-1) && n1 == (-1) then readFileDoubles else readFileDoublesMN m n1) file) -- >>= print
  let n = V.length vecA0
      freq0 j = V.unsafeIndex notes (snd . V.unsafeIndex vecA0 $ j `rem` n)
      fA1 j = fAddFElem (freq0 (j + 1),0.5) f0 gAdd
      fR1 j = fRemoveFElem (freq0 (j + 1),0.5) f0 gRem
      vecB = V.imap (\j r -> (V.unsafeIndex notes (snd r),
       case fst r of
         0 -> f0
         1 -> fA1 j
         2 -> fA1 j
         3 -> fA1 j
         4 -> fA1 j
         _ -> fR1 j)) vecA0
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j (x,k) -> do
        h1 (\_ -> k (1.1 * freq0 j)) (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) 
        renameFile ("result.wav") $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") vecB
  endFromResult

-- | Generates a sequence of sounds with changing timbre. Uses several functions as parameters. Unlike the 'soundGen3G', the last two 
-- functions as arguments for their first argument have not ('Float','Float'), but 'V.Vector' of them so are applied to 'OvertonesO'. To 
-- provide a generalized functionality, it uses two additional functions @freq0 :: Int -> OvertonesO@ and @proj :: OvertonesO -> OvertonesO@ 
-- to define the first element to which are applied @gAdds@ and @gRems@ and the way to obtain a internal 'OvertonesO'.
-- Besides, it lifts notes into specified with the first two 'Int' arguments enku (see 'liftInEnku'). 
-- The 'Float' argument is a average duration of the sounds.
soundGen3G_O :: Int -> Int -> Float -> FilePath -> Float -> String -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) 
  -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) -> (Int -> OvertonesO) -> (OvertonesO -> OvertonesO) -> 
    (Float -> OvertonesO) -> IO ()
soundGen3G_O = soundGen3G_OMN (-1) (-1)

-- | Generalized version of the 'soundGen3G_O' where 'liftInParams' is used instead of lifting with the 'liftInEnku'. This allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
soundGen3G_OPar :: Params -> Float -> FilePath -> Float -> String -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) 
  -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) -> (Int -> OvertonesO) -> (OvertonesO -> OvertonesO) -> 
    (Float -> OvertonesO) -> IO ()
soundGen3G_OPar = soundGen3G_OMNPar (-1) (-1)

-- | Generates a sequence of sounds with changing timbre. Uses several functions as parameters. To specify how many sounds the resulting files 
-- will provide, you use first two 'Int64' arguments, the first of which is a number of dropped elements for 'readFileDoubles' and the second one 
-- is a number of produced sounds (and, respectively, number of taken elements).
soundGen3G_OMN :: Int64 -> Int64 -> Int -> Int -> Float -> FilePath -> Float -> String -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) 
  -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) -> (Int -> OvertonesO) -> (OvertonesO -> OvertonesO) -> 
    (Float -> OvertonesO) -> IO ()
soundGen3G_OMN m1 n1 m ku freq1 file y zs gAdds gRems freq0 proj f0 = do
  vecA0 <- fmap (V.map (`quotRem` 108)) ((if m1 == (-1) && n1 == (-1) then readFileDoubles else readFileDoublesMN m1 n1) file) -- >>= print
  let n = V.length vecA0
      fA1 j = fAddFElems (proj . freq0 $ j) f0 gAdds
      fR1 j = fRemoveFElems (proj . freq0 $ j) f0 gRems
      vecB = V.imap (\j r -> (V.unsafeIndex notes (snd r),
       case fst r of
         0 -> f0
         1 -> fA1 j
         2 -> fA1 j
         3 -> fA1 j
         4 -> fA1 j
         _ -> fR1 j)) vecA0
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j (x,k) -> do
        h2 (k x) (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) m ku freq1
        renameFile ("result.wav") $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") vecB
  endFromResult  

-- | Generalized version of the 'soundGen3G_OMN' where 'liftInParams' is used instead of lifting with the 'liftInEnku'. This allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
soundGen3G_OMNPar :: Int64 -> Int64 -> Params -> Float -> FilePath -> Float -> String -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) 
  -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) -> (Int -> OvertonesO) -> (OvertonesO -> OvertonesO) -> 
    (Float -> OvertonesO) -> IO ()
soundGen3G_OMNPar m1 n1 params freq1 file y zs gAdds gRems freq0 proj f0 = do
  vecA0 <- fmap (V.map (`quotRem` 108)) ((if m1 == (-1) && n1 == (-1) then readFileDoubles else readFileDoublesMN m1 n1) file) -- >>= print
  let n = V.length vecA0
      fA1 j = fAddFElems (proj . freq0 $ j) f0 gAdds
      fR1 j = fRemoveFElems (proj . freq0 $ j) f0 gRems
      vecB = V.imap (\j r -> (V.unsafeIndex notes (snd r),
       case fst r of
         0 -> f0
         1 -> fA1 j
         2 -> fA1 j
         3 -> fA1 j
         4 -> fA1 j
         _ -> fR1 j)) vecA0
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j (x,k) -> do
        h2Params (k x) (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) params freq1
        renameFile ("result.wav") $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") vecB
  endFromResult    

-- | Generates a sequence of sounds with changing timbre. Is a generalized version of the 'soundGen3G_O', instead of predefined conversion function 
-- inside, it uses a user-defined one. 
-- 
-- > soundGen3G_O = soundGen3G_O2 
-- with the first argument 
-- 
-- > conversionFII (f0,fA1,fR1) = imap (\j r -> (unsafeIndex notes (snd r),
-- 
-- >      case fst r of
-- 
-- >        0 -> f0
-- 
-- >        1 -> fA1 j
-- 
-- >        2 -> fA1 j
-- 
-- >        3 -> fA1 j
-- 
-- >        4 -> fA1 j
-- 
-- >        _ -> fR1 j))
-- 
-- 
soundGen3G_O2 :: ((Float -> OvertonesO,Int -> Float -> OvertonesO,Int -> Float -> OvertonesO) -> V.Vector (Int,Int) -> 
  V.Vector (Float,Float -> OvertonesO)) -> Int -> Int -> Float -> FilePath -> Float -> String -> 
    (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) -> 
      (Int -> OvertonesO) -> (OvertonesO -> OvertonesO) -> (Float -> OvertonesO) -> IO ()
soundGen3G_O2 = soundGen3G_O2MN (-1) (-1)

-- | Generalized version of the 'soundGen3G_O2' where 'liftInParams' is used instead of lifting with the 'liftInEnku'. This allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
soundGen3G_O2Par :: ((Float -> OvertonesO,Int -> Float -> OvertonesO,Int -> Float -> OvertonesO) -> V.Vector (Int,Int) -> 
  V.Vector (Float,Float -> OvertonesO)) -> Params -> Float -> FilePath -> Float -> String -> 
    (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) -> 
      (Int -> OvertonesO) -> (OvertonesO -> OvertonesO) -> (Float -> OvertonesO) -> IO ()
soundGen3G_O2Par = soundGen3G_O2MNPar (-1) (-1)

-- | Generates a sequence of sounds with changing timbre. Is a generalized version of the 'soundGen3G_O2'. To specify how many sounds the resulting files 
-- will provide, you use first two 'Int64' arguments, the first of which is a number of dropped elements for 'readFileDoubles' and the second one 
-- is a number of produced sounds (and, respectively, number of taken elements).
soundGen3G_O2MN :: Int64 -> Int64 -> ((Float -> OvertonesO,Int -> Float -> OvertonesO,Int -> Float -> OvertonesO) -> V.Vector (Int,Int) -> 
  V.Vector (Float,Float -> OvertonesO)) -> Int -> Int -> Float -> FilePath -> Float -> String -> 
    (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) -> 
      (Int -> OvertonesO) -> (OvertonesO -> OvertonesO) -> (Float -> OvertonesO) -> IO ()
soundGen3G_O2MN m1 n1 conversionFII m ku freq1 file y zs gAdds gRems freq0 proj f0 = do
  vecA0 <- fmap (V.map (`quotRem` 108)) ((if m1 == (-1) && n1 == (-1) then readFileDoubles else readFileDoublesMN m1 n1) file) -- >>= print
  let n = V.length vecA0
      fA1 j = fAddFElems (proj . freq0 $ j) f0 gAdds
      fR1 j = fRemoveFElems (proj . freq0 $ j) f0 gRems
      vecB = conversionFII (f0,fA1,fR1) vecA0
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j (x,k) -> do
        h2 (k x) (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) m ku freq1
        renameFile ("result.wav") $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") vecB
  endFromResult

-- | Generalized version of the 'soundGen3G_O2MN' where 'liftInParams' is used instead of lifting with the 'liftInEnku'. This allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
soundGen3G_O2MNPar :: Int64 -> Int64 -> ((Float -> OvertonesO,Int -> Float -> OvertonesO,Int -> Float -> OvertonesO) -> V.Vector (Int,Int) -> 
  V.Vector (Float,Float -> OvertonesO)) -> Params -> Float -> FilePath -> Float -> String -> 
    (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) -> 
      (Int -> OvertonesO) -> (OvertonesO -> OvertonesO) -> (Float -> OvertonesO) -> IO ()
soundGen3G_O2MNPar m1 n1 conversionFII params freq1 file y zs gAdds gRems freq0 proj f0 = do
  vecA0 <- fmap (V.map (`quotRem` 108)) ((if m1 == (-1) && n1 == (-1) then readFileDoubles else readFileDoublesMN m1 n1) file) -- >>= print
  let n = V.length vecA0
      fA1 j = fAddFElems (proj . freq0 $ j) f0 gAdds
      fR1 j = fRemoveFElems (proj . freq0 $ j) f0 gRems
      vecB = conversionFII (f0,fA1,fR1) vecA0
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j (x,k) -> do
        h2Params (k x) (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) params freq1
        renameFile ("result.wav") $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") vecB
  endFromResult          

-- | Generates a sequence of sounds with changing timbre. Is a generalized version of the 'soundGen3G_O2', but for the conversion function conversionFII as its 
-- tuple first argument uses not the tuple of the three functions, but a tuple of three 'V.Vector' of functions of the respective types, that allows to 
-- specify more comlex behaviour and different variants inside the function itself, not its inner function parts. 'V.Vector' as a data type is used 
-- instead of more common list because it has similar functionality and besides provides easier and quicker access to its elements. So these are the 
-- following vectors of functions: @vf :: Vector (Float -> OvertonesO)@ (no changing a function for timbre generation), 
-- @vfA :: Vector (Int -> Float -> OvertonesO)@ (for \"adding\" overtones to the function for timbre generation), 
-- and @vfR :: Vector (Int -> Float -> OvertonesO@ (for \"removing\" overtones from the function for timbre generation).
-- 
soundGen3G_O2G :: ((V.Vector (Float -> OvertonesO),V.Vector (Int -> Float -> OvertonesO),V.Vector (Int -> Float -> OvertonesO)) -> V.Vector (Int,Int) -> 
  V.Vector (Float,Float -> OvertonesO)) -> V.Vector (Float -> OvertonesO) -> V.Vector (Int -> Float -> OvertonesO) -> V.Vector (Int -> Float -> OvertonesO) -> 
    Int -> Int -> Float -> FilePath -> Float -> String -> IO ()
soundGen3G_O2G = soundGen3G_O2GMN (-1) (-1)

-- | Generalized version of the 'soundGen3G_O2G' where 'liftInParams' is used instead of lifting with the 'liftInEnku'. This allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
soundGen3G_O2GPar :: ((V.Vector (Float -> OvertonesO),V.Vector (Int -> Float -> OvertonesO),V.Vector (Int -> Float -> OvertonesO)) -> V.Vector (Int,Int) -> 
  V.Vector (Float,Float -> OvertonesO)) -> V.Vector (Float -> OvertonesO) -> V.Vector (Int -> Float -> OvertonesO) -> V.Vector (Int -> Float -> OvertonesO) -> 
    Params -> Float -> FilePath -> Float -> String -> IO ()
soundGen3G_O2GPar = soundGen3G_O2GMNPar (-1) (-1)

-- | Generates a sequence of sounds with changing timbre. Is a generalized version of the 'soundGen3G_O2G'. To specify how many sounds the resulting files 
-- will provide, you use first two 'Int64' arguments, the first of which is a number of dropped elements for 'readFileDoubles' and the second one 
-- is a number of produced sounds (and, respectively, number of taken elements).
soundGen3G_O2GMN :: Int64 -> Int64 -> ((V.Vector (Float -> OvertonesO),V.Vector (Int -> Float -> OvertonesO),V.Vector (Int -> Float -> OvertonesO)) -> V.Vector (Int,Int) -> 
  V.Vector (Float,Float -> OvertonesO)) -> V.Vector (Float -> OvertonesO) -> V.Vector (Int -> Float -> OvertonesO) -> V.Vector (Int -> Float -> OvertonesO) -> 
    Int -> Int -> Float -> FilePath -> Float -> String -> IO ()
soundGen3G_O2GMN m1 n1 conversionFII vf vfA vfR m ku freq1 file y zs = do
  vecA0 <- fmap (V.map (`quotRem` 108)) ((if m1 == (-1) && n1 == (-1) then readFileDoubles else readFileDoublesMN m1 n1) file) -- >>= print
  let n = V.length vecA0
      vecB = conversionFII (vf,vfA,vfR) vecA0
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j (x,k) -> do
        h2 (k x) (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) m ku freq1
        renameFile ("result.wav") $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") vecB
  endFromResult

-- | Generalized version of the 'soundGen3G_O2GMN' where 'liftInParams' is used instead of lifting with the 'liftInEnku'. This allows e. g. to 
-- use some tonality. For more information, please, refer to 'filterInParams'.
soundGen3G_O2GMNPar :: Int64 -> Int64 -> ((V.Vector (Float -> OvertonesO),V.Vector (Int -> Float -> OvertonesO),V.Vector (Int -> Float -> OvertonesO)) -> V.Vector (Int,Int) -> 
  V.Vector (Float,Float -> OvertonesO)) -> V.Vector (Float -> OvertonesO) -> V.Vector (Int -> Float -> OvertonesO) -> V.Vector (Int -> Float -> OvertonesO) -> 
    Params -> Float -> FilePath -> Float -> String -> IO ()
soundGen3G_O2GMNPar m1 n1 conversionFII vf vfA vfR params freq1 file y zs = do
  vecA0 <- fmap (V.map (`quotRem` 108)) ((if m1 == (-1) && n1 == (-1) then readFileDoubles else readFileDoublesMN m1 n1) file) -- >>= print
  let n = V.length vecA0
      vecB = conversionFII (vf,vfA,vfR) vecA0
      zeroN = numVZeroesPre vecB
      v2    = str2DurationsDef n zs y in V.imapM_ (\j (x,k) -> do
        h2Params (k x) (x, (V.unsafeIndex v2 (j `rem` (V.length v2)))) params freq1
        renameFile ("result.wav") $ "result0" ++ prependZeroes zeroN (show (j + 1)) ++ ".wav") vecB
  endFromResult  

-- | For the given parameters generates a single sound with overtones or pause depending on the sign of the second element in the tuple of 'Float': 
-- if it is greater than zero then the sound is generated, if less -- the silence (pause), if it is equal to zero then it prints an informational message 
-- about a non-standard situation. 
h1 :: (Float -> OvertonesO) -> (Float, Float) -> IO ()
h1 f (x, y) = do
    let note0 = closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0)
        v0    = f note0
        ts = showFFloat (Just 4) (abs y) ""
    case compare y 0.0 of
     GT -> do 
       (_,_,herr) <- readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testA.wav", "synth", ts,"sine",showFFloat Nothing note0 ""] ""
       print herr
       partialTest_k v0 0 ts
       mixTest
     LT -> readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "result.wav", "synth", ts,"sine",showFFloat Nothing note0 "","vol","0"] "" >>= 
        \(_,_,herr) -> putStr herr
     _ -> putStrLn "Zero length of the sound! "  

-- | For the given parameters generates a single sound with overtones or pause depending on the sign of the second element in the tuple of 'Float': 
-- if it is greater than zero then the sound is generated, if less -- the silence (pause), if it is equal to zero then it prints an informational message 
-- about a non-standard situation. Unlike the 'h1' function, it lifts the frequency into the enku specified by the 'Int' arguments (see 'liftInEnku').
h2 :: OvertonesO -> (Float, Float) -> Int -> Int -> Float -> IO ()
h2 v (x, y) m ku freq1 = do
    let note0 = fromMaybe freq1 . liftInEnku m ku . closestNote $ (if x /= 0.0 then abs x else V.unsafeIndex notes 0)
        ts = showFFloat (Just 4) (abs y) ""
    case compare y 0.0 of
     GT -> do 
       (_,_,herr) <- readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testA.wav", "synth", ts,"sine",showFFloat Nothing note0 ""] ""
       print herr
       partialTest_k v 0 ts
       mixTest
     LT -> readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "result.wav", "synth", ts,"sine",showFFloat Nothing note0 "","vol","0"] "" >>= 
        \(_,_,herr) -> putStr herr
     _ -> putStrLn "Zero length of the sound! "  

-- | For the given parameters generates a single sound with overtones or pause depending on the sign of the second element in a tuple of 'Float': 
-- if it is greater than zero then the sound is generated, if less -- the silence (pause), if it is equal to zero then it prints an informational message 
-- about a non-standard situation.  Unlike the 'h1' function, it lifts into the requency specified by the 'Params' argument .
h2Params :: OvertonesO -> (Float, Float) -> Params -> Float -> IO ()
h2Params v (x, y) params freq1 = do
    let note01 = flip liftInParams params (if x /= 0.0 then abs x else V.unsafeIndex notes 0)
        ts = showFFloat (Just 4) (abs y) ""
        note0 = if note01 == 11440.0 then freq1 else note01
    case compare y 0.0 of
     GT -> do 
       (_,_,herr) <- readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "testA.wav", "synth", ts,"sine",showFFloat Nothing note0 ""] ""
       print herr
       partialTest_k v 0 ts
       mixTest
     LT -> readProcessWithExitCode (fromJust (showE "sox")) ["-r22050", "-n", "result.wav", "synth", ts,"sine",showFFloat Nothing note0 "","vol","0"] "" >>= 
        \(_,_,herr) -> putStr herr
     _ -> putStrLn "Zero length of the sound! "    
