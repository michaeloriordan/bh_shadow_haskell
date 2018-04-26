import System.IO
import Config (camera)
import BH_Shadow (calculate_shadow)

main :: IO()
main = do
    writeFile filename results where
        results = calculate_shadow camera :: String
        filename = "data.txt" :: FilePath
