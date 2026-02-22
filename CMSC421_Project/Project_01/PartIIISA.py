import sys
import random
import numpy as np

from search import Problem, simulated_annealing, exp_schedule


def tour_cost(dist, tour):
    total = 0.0
    n = len(tour)
    for i in range(n - 1):
        total += dist[tour[i], tour[i + 1]]
    total += dist[tour[-1], tour[0]]
    return total


class TSPProblem(Problem):

    def __init__(self, dist, initial=None):
        self.dist = dist
        self.n = len(dist)

        if initial is None:
            perm = list(range(self.n))
            rest = perm[1:]
            random.shuffle(rest)
            initial = tuple([0] + rest)
        else:
            initial = tuple(initial)

        super().__init__(initial)

    def actions(self, state):
        n = self.n
        return [(i, j) for i in range(1, n - 1) for j in range(i + 1, n)]

    def result(self, state, action):
        i, j = action
        s = list(state)
        s[i], s[j] = s[j], s[i]
        return tuple(s)

    def value(self, state):
        return -tour_cost(self.dist, state)


matrix_file = sys.argv[1]
if len(sys.argv) >= 3:
    random.seed(int(sys.argv[2]))

k = float(sys.argv[3]) if len(sys.argv) >= 4 else 2000.0
alpha = float(sys.argv[4]) if len(sys.argv) >= 5 else 0.0005
limit = int(sys.argv[5]) if len(sys.argv) >= 6 else 50000

dist = np.loadtxt(matrix_file)
problem = TSPProblem(dist)

schedule = exp_schedule(k, alpha, limit)
best_tour = simulated_annealing(problem, schedule)
best_cost = tour_cost(dist, best_tour)

print(list(best_tour) + [best_tour[0]])
print(best_cost)

