import math

def print2DArr(arr):
    for i in arr:
        for j in i:
            print(j, end=' ')
        print()

inf = math.inf

DIST = [
    [0, inf, inf, 10],
    [inf, 0, 5, 20],
    [inf, 10, 0, 5],
    [inf, inf, 15, 0]
]

N = "N"

PRED = [
    [N, N, N, 0],
    [N, N, 1, 1],
    [N, 2, N, 2],
    [N, N, 3, N]
]

for k in range(len(DIST)):
    for i in range(len(DIST)):
        for j in range(len(DIST)):
            if DIST[i][j] > DIST[i][k]+DIST[k][j]:
                DIST[i][j] = DIST[i][k]+DIST[k][j]
                PRED[i][j] = k
    print("Pass by ", k)
    print("DIST")
    print2DArr(DIST)
    print("PRED")
    print2DArr(PRED)
    print()