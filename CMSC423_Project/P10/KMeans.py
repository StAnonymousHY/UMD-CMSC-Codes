input = open("input", "r")
FirstLine = input.readline().strip().split()
k = int(FirstLine[0])
dimension = int(FirstLine[1])
points = []
for line in input:
    points.append([float(s) for s in (line.strip().split())])

def FindCenter(points, dimension):
    center = []
    for i in range(dimension):
        center.append(0)
    
    for i in points:
        for j in range(len(i)):
            center[j] += i[j]
    
    for i in range(dimension):
        center[i] /= len(points)
    
    return center

def FindDistance(point1, point2):
    sum = 0
    for i in range(len(point1)):
        sum += pow(point1[i]-point2[i], 2)
    return pow(sum,0.5)

def FindNearestPoints(centers, points):
    clusters = []
    for i in range(len(centers)):
        clusters.append([])
    for i in points:
        min = FindDistance(centers[0], i)
        index = 0
        for j in range(len(centers)):
            dist = FindDistance(centers[j], i)
            if dist < min: 
                min = dist
                index = j
        clusters[index].append(i)
    return clusters

centers = []
for i in range(k):
    centers.append(points[i])
OldCenters = centers.copy()
clusters = FindNearestPoints(centers, points)
for i in range(k):
    NewCenter = FindCenter(clusters[i],dimension)
    centers[i] = NewCenter
while OldCenters != centers:
    OldCenters = centers.copy()
    clusters = FindNearestPoints(centers, points)
    for i in range(k):
        NewCenter = FindCenter(clusters[i],dimension)
        centers[i] = NewCenter

for i in centers:
    for j in i:
        print(j, end = ' ')
    print()