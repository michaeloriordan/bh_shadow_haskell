import System.IO
import Config (camera)
import Camera (init_pixels)
import BH_Shadow (init_photons, propagate_photons, data_to_save)

pixels          = init_pixels camera
initial_photons = init_photons pixels
final_photons   = propagate_photons initial_photons
results         = data_to_save final_photons pixels

main = do
    writeFile "data.txt" results
