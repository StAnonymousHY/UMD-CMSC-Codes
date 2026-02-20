import sys, numpy as np
from search import Problem, astar_search

def prim_mst(d, nodes):
    if len(nodes) <= 1: return 0.0
    nodes = list(nodes)
    s = nodes[0]
    inT = {s}
    best = {v: d[s, v] for v in nodes if v != s}
    cost = 0.0
    while len(inT) < len(nodes):
        v = min((x for x in nodes if x not in inT), key=lambda x: best[x])
        cost += best[v]
        inT.add(v)
        for w in nodes:
            if w not in inT and d[v, w] < best[w]:
                best[w] = d[v, w]
    return cost

class TSP(Problem):
    def __init__(self, d, start=0):
        self.d, self.n, self.s = d, d.shape[0], start
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
        cur, mask = node.state
        unvis = [j for j in range(self.n) if ((mask >> j) & 1) == 0]
        return prim_mst(self.d, unvis)

d = np.loadtxt(sys.argv[1])
start = int(sys.argv[2]) if len(sys.argv) > 2 else 0
goal = astar_search(TSP(d, start))
tour = [start] + goal.solution()
cost = sum(d[tour[i], tour[i+1]] for i in range(len(tour)-1))
print(tour)
print(cost)