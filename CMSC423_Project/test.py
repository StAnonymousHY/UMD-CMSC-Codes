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

print(SubtractTuple(((1,2),2)))