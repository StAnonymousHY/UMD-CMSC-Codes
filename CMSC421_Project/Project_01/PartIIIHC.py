import sys
import random
import numpy as np


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


def random_restart_hill_climbing(dist, num_restarts):
    n = len(dist)
    best_tour = None
    best_cost = float("inf")

    max_no_improve = n * n

    for _ in range(num_restarts):
        curr = random_tour(n)
        curr_cost = tour_cost(dist, curr)

        no_improve = 0
        while no_improve < max_no_improve:
            neigh = random_swap_neighbor(curr)
            neigh_cost = tour_cost(dist, neigh)

            if neigh_cost < curr_cost:
                curr = neigh
                curr_cost = neigh_cost
                no_improve = 0
            else:
                no_improve += 1

        if curr_cost < best_cost:
            best_tour = curr
            best_cost = curr_cost

    return best_tour, best_cost

matrix_file = sys.argv[1]
num_restarts = int(sys.argv[2]) if len(sys.argv) >= 3 else 200


dist = np.loadtxt(matrix_file)

tour, cost = random_restart_hill_climbing(dist, num_restarts)

print(tour + [tour[0]])
print(cost)