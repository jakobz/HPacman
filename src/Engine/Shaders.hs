module Engine.Shaders where

import Control.Monad
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT as GLUT (reportErrors)
import Data.Text

shadersPath = "shaders\\"

newProgram :: String -> IO Program
newProgram name = do vs <- readAndCompileShader (shadersPath ++ name ++ ".vs")
                     fs <- readAndCompileShader (shadersPath ++ name ++ ".fs")
                     p <- linkShaders [vs] [fs]
                     return p

getUniformLocation p s = GL.get $ uniformLocation p s 

infoLogEmpty s = ss == "" || ss == "No errors." where ss = unpack $ strip $ pack s

readAndCompileShader :: Shader s => FilePath -> IO s
readAndCompileShader filePath = do
  src <- readFile filePath
  [shader] <- genObjectNames 1
  shaderSource shader $= [src]
  compileShader shader
  reportErrors
  ok <- get (compileStatus shader)
  infoLog <- get (shaderInfoLog shader)
  unless (infoLogEmpty infoLog) $
    mapM_ putStrLn ["Shader info log '" ++ filePath ++ ":", infoLog, "" ]
  unless ok $ do 
    deleteObjectNames [shader]
    ioError (userError "compilation failed")
  return shader

linkShaders :: [VertexShader] -> [FragmentShader] -> IO Program
linkShaders vs fs = do
  [prog] <- genObjectNames 1
  attachedShaders prog $=  (vs, fs)
  linkProgram prog
  reportErrors
  ok <- get (linkStatus prog)
  infoLog <- get (programInfoLog prog)
  unless (infoLogEmpty infoLog) $
    mapM_ putStrLn ["Program info log: ", infoLog, ""]
  unless ok $ do deleteObjectNames [prog]
                 ioError (userError "linking failed")  
  return prog
