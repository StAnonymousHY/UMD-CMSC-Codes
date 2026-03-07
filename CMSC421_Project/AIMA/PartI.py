import sys
import numpy as np
import random
import time


def tour_cost(dist, tour):
    sum = 0
    for i in range(len(tour) - 1):
        sum += dist[tour[i], tour[i+1]]
    return sum

def min_dist(dist, curr, unvisited):
    min = float('inf')
    result = 0
    for i in unvisited:
        if(min > dist[curr, i]):
            min = dist[curr, i]
            result = i
    return result

def nearest_neighbor(dist):
    start = 0
    n = len(dist)
    unvisited = set(range(n))
    unvisited.remove(start)
    tour = [start]
    curr = start
    while unvisited:
        next = min_dist(dist, curr, unvisited)
        tour.append(next)
        unvisited.remove(next)
        curr = next
    tour.append(start)
    return tour


def two_opt(dist, tour):
    result = tour.copy()
    improved = True
    while improved:
        improved = False
        for i in range(1, len(result)-2):
            original = dist[result[i-1],result[i]] + dist[result[i+1],result[i+2]]
            swapped = dist[result[i-1],result[i+1]] + dist[result[i],result[i+2]]
            if swapped < original:
                temp = result[i]
                result[i] = result[i+1]
                result[i+1] = temp
                improved = True
                break
    return result


def nn2(dist):
    return two_opt(dist, nearest_neighbor(dist))

def knn(dist, curr, unvisited, k):
    cnt = 0
    temp = unvisited.copy()
    result = []
    while cnt < k and len(temp) > 0:
        city = min_dist(dist, curr, temp)
        result.append(city)
        temp.remove(city)
        cnt += 1
    return result

def rrnn(dist, k, repeats):
    start = 0
    n = len(dist)
    best_tour = []
    best_cost = float("inf")
    for i in range(repeats):
        unvisited = set(range(n))
        unvisited.remove(start)
        tour = [start]
        curr = start
        while unvisited:
            cand = knn(dist, curr, unvisited, k)
            next = random.choice(cand)
            tour.append(next)
            unvisited.remove(next)
            curr = next
        tour.append(start)
        tour = two_opt(dist, tour)
        cost = tour_cost(dist, tour)
        if cost < best_cost:
            best_cost = cost
            best_tour = tour
    return best_tour



fname = sys.argv[1]
alg = sys.argv[2]
dist = np.loadtxt(fname)

t0_real = time.time_ns()
t0_cpu = time.process_time_ns()

if alg == "nn":
    tour = nearest_neighbor(dist)
elif alg == "nn2":
    tour = nn2(dist)
elif alg == "rrnn":
    tour = rrnn(dist, (int(sys.argv[3]) if len(sys.argv) > 3 else 2), (int(sys.argv[4]) if len(sys.argv) > 4 else 800))

t1_cpu = time.process_time_ns()
t1_real = time.time_ns()

real_ns = t1_real - t0_real
cpu_ns = t1_cpu - t0_cpu

if cpu_ns == 0:
    R = 500
    t0_cpu = time.process_time_ns()
    for _ in range(R):
        if alg == "nn":
            tour = nearest_neighbor(dist)
        elif alg == "nn2":
            tour = nn2(dist)
        elif alg == "rrnn":
            tour = rrnn(dist, (int(sys.argv[3]) if len(sys.argv) > 3 else 2), (int(sys.argv[4]) if len(sys.argv) > 4 else 800))
    t1_cpu = time.process_time_ns()
    cpu_ns = (t1_cpu - t0_cpu) // R

if real_ns == 0: 
    R = 500
    t0_real = time.time_ns()
    for _ in range(R):
        if alg == "nn":
            tour = nearest_neighbor(dist)
        elif alg == "nn2":
            tour = nn2(dist)
        elif alg == "rrnn":
            tour = rrnn(dist, (int(sys.argv[3]) if len(sys.argv) > 3 else 2), (int(sys.argv[4]) if len(sys.argv) > 4 else 800))
    t1_real = time.time_ns()
    real_ns = (t1_real - t0_real) // R

print(tour)
print(tour_cost(dist, tour))
print(real_ns)
print(cpu_ns)