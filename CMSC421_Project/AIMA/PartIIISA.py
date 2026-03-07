import sys
import random
import numpy as np
from search import Problem, simulated_annealing, exp_schedule
import time
import csv
import math


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

def simulated_annealing_trace(problem, schedule, trace=None):
    current = problem.initial
    current_val = problem.value(current)

    best = current
    best_val = current_val

    t = 0
    while True:
        t += 1
        T = schedule(t)
        if T == 0:
            return best

        actions = problem.actions(current)
        if not actions:
            return best

        a = random.choice(actions)
        nxt = problem.result(current, a)
        nxt_val = problem.value(nxt)
        delta = nxt_val - current_val  # higher value is better (since value=-cost)

        if delta > 0 or random.random() < math.exp(delta / T):
            current, current_val = nxt, nxt_val

        if current_val > best_val:
            best, best_val = current, current_val

        if trace is not None:
            trace.append(-best_val)  # convert back to cost

matrix_file = sys.argv[1]

k = float(sys.argv[2]) if len(sys.argv) >= 3 else 2000.0
alpha = float(sys.argv[3]) if len(sys.argv) >= 4 else 0.0005
limit = int(sys.argv[4]) if len(sys.argv) >= 5 else 50000

dist = np.loadtxt(matrix_file)
problem = TSPProblem(dist)

schedule = exp_schedule(k, alpha, limit)
t0r = time.time_ns()
t0c = time.process_time_ns()
csv_out = sys.argv[5] if len(sys.argv) >= 6 else "sa_trace.csv"

trace = []
best_tour = simulated_annealing_trace(problem, schedule, trace=trace)

with open(csv_out, "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["iter", "best_cost"])
    for i, c in enumerate(trace, start=1):
        w.writerow([i, c])
t1c = time.process_time_ns()
t1r = time.time_ns()

best_cost = tour_cost(dist, best_tour)
real_ns = t1r - t0r
cpu_ns  = t1c - t0c

if cpu_ns == 0:
    R = 500
    t0c = time.process_time_ns()
    for _ in range(R):
        simulated_annealing(problem, schedule)
    t1c = time.process_time_ns()
    cpu_ns  = (t1c - t0c) // R

if real_ns == 0:
    R = 500
    t0r = time.time_ns()
    for _ in range(R):
        simulated_annealing(problem, schedule)
    t1r = time.time_ns()
    real_ns = (t1r - t0r) // R

print(list(best_tour) + [best_tour[0]])
print(best_cost)
print(real_ns)
print(cpu_ns)
