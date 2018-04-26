import System.IO
import Config (camera)
import BH_Shadow (calculate_shadow)

results = calculate_shadow camera

main = do
    writeFile "data.txt" results
