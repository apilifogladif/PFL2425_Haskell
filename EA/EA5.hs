module IOUtils (
cls ,
goto , writeAt , writeListAt ,
Color (..) ,
clrFg , clrBg , clrReset ,
wait) where
import System.Posix ( usleep ) -- for Unix -based systems , modify / uncomment accordingly if another operating system is used , if needed
-- import System .Win32. Process (sleep) -- for Windows
-- Clears the console .
cls :: IO ()
cls = putStr "\ESC [2J"
-- Writes a character at the position (x,y) of the console .
-- (1 ,1) is the top -left corner .
writeAt :: ( Integral a, Show a) => (a,a) -> Char -> IO ()
writeAt p c = do
goto p
putChar c
goto :: ( Integral a, Show a) => (a,a) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
-- Writes the same character at multiple positions on the console .
writeListAt :: ( Integral a, Show a) => [(a,a)] -> Char -> IO ()
writeListAt [] _ = return ()
writeListAt (p:ps) c = do
writeAt p c
writeListAt ps c
data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White deriving (Eq ,Show ,Enum)
-- Changes the color of the foreground text
clrFg :: Color -> IO()
clrFg Black = putStr "\ESC [30m"
clrFg Red = putStr "\ESC [31m"
clrFg Green = putStr "\ESC [32m"
clrFg Yellow = putStr "\ESC [33m"
clrFg Blue = putStr "\ESC [34m"
clrFg Magenta = putStr "\ESC [35m"
clrFg Cyan = putStr "\ESC [36m"
clrFg White = putStr "\ESC [37m"
-- Changes the color of the background text
clrBg :: Color -> IO()
clrBg Black = putStr "\ESC [40m"
clrBg Red = putStr "\ESC [41m"
clrBg Green = putStr "\ESC [42m"
clrBg Yellow = putStr "\ESC [43m"
clrBg Blue = putStr "\ESC [44m"
clrBg Magenta = putStr "\ESC [45m"
clrBg Cyan = putStr "\ESC [46m"
clrBg White = putStr "\ESC [47m"
-- Resets the foreground and background color
clrReset :: IO ()
clrReset = putStr "\ESC [0m"
-- Waits for some milliseconds .
-- for Unix -based systems
wait :: Int -> IO ()
wait t = usleep (t*1000)
-- for Windows
-- wait :: Int -> IO ()
-- wait t = sleep t