import sys
import random
import numpy as np
import time

def tour_cost(dist, tour):
    total = 0.0
    n = len(tour)
    for i in range(n - 1):
        total += dist[tour[i], tour[i + 1]]
    total += dist[tour[-1], tour[0]]
    return total

def score(dist, tour):
    return -tour_cost(dist, tour)

def random_tour(n):
    tour = list(range(n))
    rest = tour[1:]
    random.shuffle(rest)
    return [0] + rest

def tournament_select(population, dist, k):
    candidates = random.sample(population, k)
    return max(candidates, key=lambda t: score(dist, t))

def order_crossover_OX(p1, p2):
    n = len(p1)
    child = [-1] * n
    child[0] = 0
    a, b = sorted(random.sample(range(1, n), 2))
    child[a:b + 1] = p1[a:b + 1]
    used = set(child[a:b + 1])
    used.add(0)
    fill_positions = [i for i in range(1, n) if child[i] == -1]
    fill_values = [x for x in p2 if x not in used]
    for i, x in zip(fill_positions, fill_values):
        child[i] = x
    return child

def mutate_swap(tour):
    n = len(tour)
    i, j = random.sample(range(1, n), 2)
    tour[i], tour[j] = tour[j], tour[i]

def genetic_algorithm_tsp(dist,mutation_chance, population_size, num_generations, tournament_k):
    n = dist.shape[0]
    population = [random_tour(n) for _ in range(population_size)]
    for _ in range(num_generations):
        children: list[list[int]] = []
        while len(children) < population_size:
            p1 = tournament_select(population, dist, tournament_k)
            p2 = tournament_select(population, dist, tournament_k)
            if p1 is p2 and population_size > 2:
                continue
            c1 = order_crossover_OX(p1, p2)
            c2 = order_crossover_OX(p2, p1)
            if random.random() < mutation_chance:
                mutate_swap(c1)
            if random.random() < mutation_chance:
                mutate_swap(c2)
            children.append(c1)
            if len(children) < population_size:
                children.append(c2)
        combined = population + children
        combined.sort(key=lambda t: score(dist, t), reverse=True)
        population = combined[:population_size]
    best = max(population, key=lambda t: score(dist, t))
    best_cost = tour_cost(dist, best)
    return best, best_cost


matrix_file = sys.argv[1]
mutation_chance = float(sys.argv[2]) if (len(sys.argv) > 2) else 0.1
population_size = int(sys.argv[3]) if (len(sys.argv) > 3) else 100
num_generations = int(sys.argv[4]) if (len(sys.argv) > 4) else 500
tournament_k = int(sys.argv[5]) if (len(sys.argv) > 5) else 3
dist = np.loadtxt(matrix_file)
t0r = time.time_ns()
t0c = time.process_time_ns()
tour, cost = genetic_algorithm_tsp(dist, mutation_chance, population_size, num_generations, tournament_k)
t1c = time.process_time_ns()
t1r = time.time_ns()

real_ns = t1r - t0r
cpu_ns  = t1c - t0c

if cpu_ns == 0:
    R = 500
    t0c = time.process_time_ns()
    for _ in range(R):
        t, c = genetic_algorithm_tsp(dist, mutation_chance, population_size, num_generations, tournament_k)
    t1c = time.process_time_ns()
    cpu_ns  = (t1c - t0c) // R

if real_ns == 0:
    R = 500
    t0r = time.time_ns()
    for _ in range(R):
        t, c = genetic_algorithm_tsp(dist, mutation_chance, population_size, num_generations, tournament_k)
    t1r = time.time_ns()
    real_ns = (t1r - t0r) // R

print("Tour: ", tour + [tour[0]])
print("Tour cost: ", cost)
print("Real time: ", real_ns)
print("CPU time: ", cpu_ns)
