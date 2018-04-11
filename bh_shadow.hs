data Vec4 = Vec4 Double Double Double Double deriving (Show)

data Photon = Photon Vec4 Vec4 deriving (Show)

x1 = Vec4 1 2 3 4
k1 = Vec4 1 2 3 4

x2 = Vec4 5 6 7 8
k2 = Vec4 5 6 7 8

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
