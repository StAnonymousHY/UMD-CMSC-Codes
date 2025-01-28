def FindSmallestDist(DistDict):
    min = 0
    for i in DistDict.keys():
        if(i[0]!=i[1]):
            min = DistDict[i]
            break
        
    result = (0,0)
    
    for i in DistDict.keys():
        if (i[0]!=i[1]) and (DistDict[i]<=min):
            min = DistDict[i]
            result = (i[1], i[0])
    
    return (result[0],result[1])

def SubtractTuple(cluster):
    left = cluster[0]
    right = cluster[1]

    LeftArr = []
    RightArr = []
    
    if isinstance(left, tuple):
        LeftArr+=SubtractTuple(left)[0]+SubtractTuple(left)[1]
    if not isinstance(left, tuple):
        LeftArr.append(left)
    if isinstance(right, tuple):
        RightArr+=SubtractTuple(right)[0]+SubtractTuple(right)[1]
    if not isinstance(right, tuple):
        RightArr.append(right)
    
    return (LeftArr,RightArr)

# UPGMA algorithm
def ComputeDist(Dict1, cluster):
    c = SubtractTuple(cluster)
    left = c[0]
    right = c[1]
    
    dist = 0
    
    for i in left:
        for j in right:
            dist+=Dict1[(i,j)]
    
    return (dist/(len(left)*len(right)))
            

def NewDict(Dict1, DistDict, NewCluster):
    newDict = {}
    element1 = NewCluster[0]
    element2 = NewCluster[1]
    elements = []
    
    for i in DistDict.keys():
        if i[0] not in elements:
            elements.append(i[0])
        if i[1] not in elements:
            elements.append(i[1])
    
    elements.remove(NewCluster[0])
    elements.remove(NewCluster[1])
    
    for i in DistDict.keys():
        if (i[0] not in NewCluster) and (i[1] not in NewCluster):
            newDict[i] = DistDict[i]
    
    for i in elements:
        avg = ComputeDist(Dict1, (i,NewCluster))
        newDict[(i,NewCluster)] = avg
        newDict[(NewCluster,i)] = avg
    
    return newDict
    
def PrintCluster(cluster):
    result = ""
    left = cluster[0]
    right = cluster[1]
    
    if isinstance(left, tuple):
        result+=PrintCluster(left)+" "
    if not isinstance(left, tuple):
        result+=str(left)+" "
    if isinstance(right, tuple):
        result+=PrintCluster(right)+" "
    if not isinstance(right, tuple):
        result+=str(right)+" "
    
    print(result)
    return result.strip()

input = open("input", "r")
size = int(input.readline().strip())
DistDict = {}
for i in range(size):
    each_line = [float(s) for s in (input.readline().strip().split())]
    for j in range(size):
        DistDict[(i+1,j+1)] = each_line[j]

ClusterNumber = int(pow(len(DistDict.keys()),0.5))

Dict1 = DistDict.copy()

while (ClusterNumber > 1):
    NewCluster = FindSmallestDist(DistDict)
    DistDict= NewDict(Dict1, DistDict, NewCluster)
    #print(DistDict)
    ClusterNumber-=1
    result = SubtractTuple(NewCluster)
    resultArr = result[0]+result[1]
    '''
    for i in resultArr:
        print(i, end = ' ')
    print()
    '''

PrintCluster(NewCluster)
    