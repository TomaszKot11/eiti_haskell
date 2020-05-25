tower_heights = [[3, 0, 1, 0], [0, 3, 0, 0], [0, 0, 0, 0], [0, 0, 4, 0]]

witam = [[2, 1, 4, 3], [3, 4, 2, 1], [1, 2, 3, 4], [4, 3, 1, 2]]

-- set - 2 rozwiazanie
tower_heights_2 = [[2, 1, 2, 2, 3], [3, 3, 0, 0, 0], [0, 0, 0, 0, 0], [2, 3, 3, 1, 4]]
solution_2 =  [[4, 5, 1, 3, 2], [3, 4, 5, 2, 1], [1, 3, 2, 5, 4], [5, 2, 4, 1, 3], [2, 1, 3, 4, 5]]


elo = [(y, x, tile) | (y, row) <- enumerate witam, (x, tile) <- enumerate row]
-- wezmy te ktore trzeba sprawdzic
papa = [(y, x, tile) | (y, row) <- enumerate tower_heights, (x, tile) <- enumerate row, tile /= 0]
