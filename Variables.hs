module Variables where 


vars:: [[Token]]
vars = [    [N 240, N 20, N 240, N 0, N 0, N 0, N 0, N 1000],
    [N 240, N 20, N 240, N 0, N 0, N 1, N 0, N 109],
    []]

data Token = N Int deriving(Show)
