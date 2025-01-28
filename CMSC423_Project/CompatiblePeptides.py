array1 = [129,131,147,163]
array2 = [163,276,129,147]
array3 = [129,147,131,163]  #This one is in the experimental spectrum of [0 129 131 147 163 276 278 292 294 407 423 439 441 570]
array4 = [131,147,163,276]

def comp(array):
    compatible = [0]

    for i in range(0,len(array)):
        for j in range(0,len(array)-i):
            index = i
            sum = 0
            while index < len(array)-j:
                sum+=array[index]
                index+=1
            compatible.append(sum)
    
    return sorted(compatible)

print(comp(array1))
print(comp(array2))
print(comp(array3))
print(comp(array4))