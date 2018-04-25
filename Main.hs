import System.IO
import BH_Shadow (init_camera, init_photons, propagate_photons, data_to_save)

camera          = init_camera
initial_photons = init_photons camera
final_photons   = propagate_photons initial_photons
results         = data_to_save final_photons camera

main = do
    writeFile "data.txt" results
