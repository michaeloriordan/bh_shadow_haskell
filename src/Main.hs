import System.IO
import Config (camera)
import BH_Shadow (calculate_shadow)
import Type_Defs (vec2_to_string, Vec2)

main :: IO()
main = do
    writeFile filename results where
        shadow   = calculate_shadow camera :: Vec2
        results  = vec2_to_string shadow   :: String
        filename = "data.txt"              :: FilePath
