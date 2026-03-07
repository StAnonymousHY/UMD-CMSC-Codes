import sys
import random
import numpy as np
import time
import csv  # add at top


def tour_cost(dist, tour):
    total = 0.0
    n = len(tour)
    for i in range(n - 1):
        total += dist[tour[i], tour[i + 1]]
    total += dist[tour[-1], tour[0]]
    return total


def random_tour(n):
    tour = list(range(n))
    rest = tour[1:]
    random.shuffle(rest)
    return [0] + rest


def random_swap_neighbor(tour):
    n = len(tour)
    i, j = random.sample(range(1, n), 2)
    new_tour = tour.copy()
    new_tour[i], new_tour[j] = new_tour[j], new_tour[i]
    return new_tour


def random_restart_hill_climbing(dist, num_restarts, trace=None):
    n = len(dist)
    best_tour = None
    best_cost = float("inf")

    max_no_improve = n * n

    for _ in range(num_restarts):
        curr = random_tour(n)
        curr_cost = tour_cost(dist, curr)

        # update global best at restart start
        if curr_cost < best_cost:
            best_tour, best_cost = curr, curr_cost
        if trace is not None:
            trace.append(best_cost)

        no_improve = 0
        while no_improve < max_no_improve:
            neigh = random_swap_neighbor(curr)
            neigh_cost = tour_cost(dist, neigh)

            if neigh_cost < curr_cost:
                curr, curr_cost = neigh, neigh_cost
                no_improve = 0
            else:
                no_improve += 1

            # update global best after each neighbor evaluation
            if curr_cost < best_cost:
                best_tour, best_cost = curr, curr_cost
            if trace is not None:
                trace.append(best_cost)

    return best_tour, best_cost

matrix_file = sys.argv[1]
num_restarts = int(sys.argv[2]) if len(sys.argv) >= 3 else 200


dist = np.loadtxt(matrix_file)

t0r = time.time_ns()
t0c = time.process_time_ns()
csv_out = sys.argv[3] if len(sys.argv) >= 4 else "hc_trace.csv"

trace = []
tour, cost = random_restart_hill_climbing(dist, num_restarts, trace=trace)

with open(csv_out, "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["iter", "best_cost"])
    for i, c in enumerate(trace):
        w.writerow([i, c])
t1c = time.process_time_ns()
t1r = time.time_ns()

real_ns = t1r - t0r
cpu_ns  = t1c - t0c

if cpu_ns == 0:
    R = 500
    t0c = time.process_time_ns()
    for _ in range(R):
        random_restart_hill_climbing(dist, num_restarts)
    t1c = time.process_time_ns()
    cpu_ns  = (t1c - t0c) // R

if real_ns == 0:
    R = 500
    t0r = time.time_ns()
    for _ in range(R):
        random_restart_hill_climbing(dist, num_restarts)
    t1r = time.time_ns()
    real_ns = (t1r - t0r) // R

print(tour + [tour[0]])
print(cost)
print(real_ns)
print(cpu_ns)