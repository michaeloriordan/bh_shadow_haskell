data Photon = Photon [Double] [Double] deriving (Show)

x1 = [1.0, 2.0, 3.0, 4.0]
k1 = [1.0, 2.0, 3.0, 4.0]

x2 = [5.0, 6.0, 7.0, 8.0]
k2 = [5.0, 6.0, 7.0, 8.0]

xs = [x1, x2]
ks = [k1, k2]

p = Photon x1 k1

ps1 = map (Photon x1) [k1, k2]

ps2 = map (\x -> Photon x k1) [x1, x2]

ps3 = [Photon x k | (x, k) <- zip xs ks]

cxmax = 5
cxmin = -5
cymax = 5
cymin = -5

nx = 10
ny = 10

dx = (cxmax - cxmin) / nx
dy = (cymax - cymin) / ny

cxs = [cxmin, cxmin+dx .. cxmax]
cys = [cymin, cymin+dy .. cymax]

cpoints = [(x, y) | x <- cxs, y <- cys]
