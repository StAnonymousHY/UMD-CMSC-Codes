input = open("input", "r")
blosum62 = open("PAM250.txt", "r")

acid1 = input.readline().strip()
acid2 = input.readline().strip()

BlosumRow = blosum62.readline().strip().split()
BlosumColumn = []

BlosumDict = {}

Max = 0

for line in blosum62: 
    BlosumColumn.append(line[0:1])
    blosums = line[1:].strip().split()
    for i in range(len(BlosumRow)):
        BlosumDict[(BlosumRow[i], line[0:1])] = int(blosums[i])

MatrixRow = ["-"]
MatrixCol = ["-"]

for i in acid1:
    MatrixRow.append(i)

for i in acid2:
    MatrixCol.append(i)

Matrix = []

GapPenalty = -5
for i in range(len(MatrixCol)):
    arr = [0]
    for j in range(1, len(MatrixRow)):
        arr.append(0)
    Matrix.append(arr)

for i in range(1, len(MatrixRow)):
    Matrix[0][i]=0

RowIndex = 0
ColIndex = 0

PosDict = {}

for j in range(1, len(MatrixRow)):
    for i in range(1, len(MatrixCol)):
        GapAbove = Matrix[i][j-1]+GapPenalty
        GapLeft = Matrix[i-1][j]+GapPenalty
        MatchMismatch = Matrix[i-1][j-1]+BlosumDict[(MatrixRow[j], MatrixCol[i])]
        
        maximum = max(0,GapAbove, GapLeft, MatchMismatch)
        if maximum == MatchMismatch:
            PosDict[(i,j)] = (i-1,j-1)
        elif maximum == GapAbove:
            PosDict[(i,j)] = (i,j-1)
        elif maximum == GapLeft:
            PosDict[(i,j)] = (i-1,j)
        
        Matrix[i][j] = maximum
        
        if maximum > Max:
            Max = maximum
            RowIndex = i
            ColIndex = j
            

result1 = ""
result2 = ""

print(Matrix[RowIndex][ColIndex])
while Matrix[RowIndex][ColIndex] != 0:
    OldRow = RowIndex
    OldCol = ColIndex
    (RowIndex, ColIndex) = PosDict[(RowIndex, ColIndex)]
    if OldRow == RowIndex+1 and OldCol == ColIndex+1:
        result1 = acid1[OldCol-1]+result1
        result2 = acid2[OldRow-1]+result2
    elif OldRow == RowIndex+1:
        result1 = "-"+result1
        result2 = acid2[OldRow-1]+result2
    else:
        result1 = acid1[OldCol-1]+result1
        result2 = "-"+result2

'''
if RowIndex != 0: 
    for i in range(RowIndex):
        result1 = "-"+result1
        result2 = acid2[i]+result2
elif ColIndex != 0: 
    for i in range(ColIndex):
        result1 = acid1[i]+result1
        result2 = "-"+result2
'''
for i in Matrix:
    print(i)
print(result1)
print(result2)