input = open("input", "r")
BWT = ""
for line in input: 
    BWT+=line.strip()
CntA = 0
CntC = 0
CntG = 0
CntT = 0
Matrix = []
for i in BWT:
    if i == "A":
        CntA += 1
        Matrix.append([i+str(CntA)])
    elif i == "C":
        CntC += 1
        Matrix.append([i+str(CntC)])
    elif i == "G":
        CntG += 1
        Matrix.append([i+str(CntG)])
    elif i == "T":
        CntT += 1
        Matrix.append([i+str(CntT)])
    else:
        Matrix.append([i])

CntA = 0
CntC = 0
CntG = 0
CntT = 0
SortedBWT = sorted(BWT)

for i in range(len(SortedBWT)):
    if SortedBWT[i] == "A":
        CntA+=1
        Matrix[i] = ["A"+str(CntA)]+Matrix[i]
    elif SortedBWT[i] == "C":
        CntC+=1
        Matrix[i] = ["C"+str(CntC)]+Matrix[i]
    elif SortedBWT[i] == "G":
        CntG+=1
        Matrix[i] = ["G"+str(CntG)]+Matrix[i]
    elif SortedBWT[i] == "T":
        CntT+=1
        Matrix[i] = ["T"+str(CntT)]+Matrix[i]
    else:
        Matrix[i] = ["$"]+Matrix[i]

def finished(matrix, BWT):
    length = len(BWT)
    for i in matrix:
        if len(i) == length:
            return i
    return None

def reverseBWT(matrix, BWT, LineToBeUpdated):
    if (arr := finished(matrix, BWT)) != None:
        #print(arr)
        ReturnString = ""
        flag = False
        for i in arr:
            if not flag:
                if i != "$":
                    ReturnString+=i[0]
                else:
                    ReturnString+=i[0]
                    flag = True
            else:
                ReturnString = i[0]+ReturnString
        return ReturnString
    else:
        UpdateStart = matrix[LineToBeUpdated][-1]
        UpdateString = []
        for i in range(len(matrix[LineToBeUpdated])-1):
            UpdateString.append(matrix[LineToBeUpdated][i])
        for i in range(len(matrix)):
            if matrix[i][0] == UpdateStart:
                matrix[i] = [matrix[i][0]]+UpdateString+[matrix[i][-1]]
                return reverseBWT(matrix, BWT, i)
        return None

print(reverseBWT(Matrix, BWT, 0))