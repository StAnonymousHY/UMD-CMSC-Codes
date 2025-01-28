
input = open("input", "r")
BasicInformation = input.readline().strip().split()

KLen = int(BasicInformation[0])
LineNum = int(BasicInformation[1])

WholeString = []
for line in input:
    WholeString.append(line.strip())

def profile(Matrix):
    ProfileMatrix = []
    CntA = []
    CntC = []
    CntG = []
    CntT = []
    SumMatrix = []
    for i in Matrix[0]:
        CntA.append(1)
        CntC.append(1)
        CntG.append(1)
        CntT.append(1)
        SumMatrix.append(0)
    
    for i in range(len(Matrix)):
        for j in range(len(Matrix[i])): 
            if Matrix[i][j] == "A": 
                CntA[j] = CntA[j]+1
            elif Matrix[i][j] == "C": 
                CntC[j] = CntC[j]+1
            elif Matrix[i][j] == "G": 
                CntG[j] = CntG[j]+1
            elif Matrix[i][j] == "T": 
                CntT[j] = CntT[j]+1

    ProfileA = []
    ProfileC = []
    ProfileG = []
    ProfileT = []
    
    for i in range(len(SumMatrix)):
        SumMatrix[i] = CntA[i] + CntC[i] + CntG[i] + CntT[i]
        
        ProfileA.append(CntA[i]/SumMatrix[i])
        ProfileC.append(CntC[i]/SumMatrix[i])
        ProfileG.append(CntG[i]/SumMatrix[i])
        ProfileT.append(CntT[i]/SumMatrix[i])
    
    ProfileMatrix.append(ProfileA)
    ProfileMatrix.append(ProfileC)
    ProfileMatrix.append(ProfileG)
    ProfileMatrix.append(ProfileT)
    return ProfileMatrix

BestMotifMatrix = []
for i in WholeString:
    BestMotifMatrix.append(i[0:KLen])

def Consensus(Matrix):
    consensus = ""
    Matrix = profile(Matrix)
    for i in range(len(Matrix[0])):
        MostProbable = max(Matrix[0][i], Matrix[1][i], Matrix[2][i], Matrix[3][i])
        if MostProbable == Matrix[0][i]:
            consensus = consensus+"A"
        elif MostProbable == Matrix[1][i]:
            consensus = consensus+"C"
        elif MostProbable == Matrix[2][i]:
            consensus = consensus+"G"
        elif MostProbable == Matrix[3][i]:
            consensus = consensus+"T"
    return consensus
        
    
def score(BestMotifMatrix):
    BestScore = 0
    con = Consensus(BestMotifMatrix)
    for i in range(len(BestMotifMatrix)):
        for j in range(len(BestMotifMatrix[0])):
            if con[j] != BestMotifMatrix[i][j]:
                BestScore = BestScore+1
    return BestScore

for i in range(len(WholeString[0])-KLen+1):
    consensus = WholeString[0][i:i+KLen]
    CurrentMotifMatrix = [consensus]
    print()
    for j in range(1, len(WholeString)):
        CurrentProfileMatrix = profile(CurrentMotifMatrix)
        BestProbability = 0
        BestMotif = ""
        for k in range(len(WholeString[j])-KLen+1):
            motif = WholeString[j][k:k+KLen]
            probability = 1
            for x in range(len(motif)):
                if motif[x] == "A":
                    probability = probability*CurrentProfileMatrix[0][x]
                elif motif[x] == "C":
                    probability = probability*CurrentProfileMatrix[1][x]
                elif motif[x] == "G":
                    probability = probability*CurrentProfileMatrix[2][x]
                elif motif[x] == "T":
                    probability = probability*CurrentProfileMatrix[3][x]
            if probability > BestProbability: 
                BestProbability = probability
                BestMotif = motif
        CurrentMotifMatrix.append(BestMotif)
        print(BestMotif)
    if score(BestMotifMatrix) > score(CurrentMotifMatrix):
        BestMotifMatrix = CurrentMotifMatrix

print()
for i in BestMotifMatrix:
    print(i)