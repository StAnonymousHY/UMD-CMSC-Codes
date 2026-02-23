import sys, numpy as np
from search import Problem, astar_search
import time

def findMin(nodes, inT, best):
    currMin = float('inf')
    result = nodes[0]
    for x in nodes: 
        if x not in inT and best[x] < currMin:
            currMin = best[x]
            result = x
    return result


def prim_mst(d, nodes):
    if len(nodes) <= 1: return 0.0
    nodes = list(nodes)
    s = nodes[0]
    inT = {s}
    best = {}
    for v in nodes:
        if v != s:
            best[v] = d[s, v]
    cost = 0.0
    while len(inT) < len(nodes):
        v = findMin(nodes, inT, best)
        cost += best[v]
        inT.add(v)
        for w in nodes:
            if w not in inT and d[v, w] < best[w]:
                best[w] = d[v, w]
    return cost

class TSP(Problem):
    def __init__(self, d, start=0):
        self.d, self.n, self.s = d, d.shape[0], start
        self.expanded = 0
        super().__init__((start, 1 << start), None)

    def actions(self, st):
        cur, mask = st
        allm = (1 << self.n) - 1
        if mask == allm: return [self.s] if cur != self.s else []
        return [j for j in range(self.n) if ((mask >> j) & 1) == 0]

    def result(self, st, a):
        cur, mask = st
        return (a, mask | (1 << a))

    def goal_test(self, st):
        cur, mask = st
        return mask == (1 << self.n) - 1 and cur == self.s

    def path_cost(self, c, s1, a, s2):
        cur, _ = s1
        return c + self.d[cur, a]

    def h(self, node):
        self.expanded += 1
        cur, mask = node.state
        unvis = [j for j in range(self.n) if ((mask >> j) & 1) == 0]
        return prim_mst(self.d, unvis)

d = np.loadtxt(sys.argv[1])
start = int(sys.argv[2]) if len(sys.argv) > 2 else 0

prob = TSP(d, start)
t0r = time.time_ns()
t0c = time.process_time_ns()
goal = astar_search(prob)
t1c = time.process_time_ns()
t1r = time.time_ns()

tour = [start] + goal.solution()
if tour[-1] != start:
    tour.append(start)

cost = sum(d[tour[i], tour[i+1]] for i in range(len(tour) - 1))
real_ns = t1r - t0r
cpu_ns  = t1c - t0c
expanded = prob.expanded

if cpu_ns == 0:
    t0c = time.process_time_ns()
    R = 500
    for _ in range(R):
        astar_search(TSP(d, start))
    t1c = time.process_time_ns()
    cpu_ns  = (t1c - t0c) // R

if real_ns == 0:
    t0r = time.time_ns()
    R = 500
    for _ in range(R):
        astar_search(TSP(d, start))
    t1r = time.time_ns()
    real_ns = (t1r - t0r) // R

print("Tour: ", tour)
print("Tour cost: ", cost)
print("Real time: ", real_ns)
print("CPU time", cpu_ns)
print("Nodes expanded: ", expanded)