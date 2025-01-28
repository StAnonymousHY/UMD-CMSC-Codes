BuildingBlock = [57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186]

def Sum(length):
    sum = 0
    for i in range(length+1):
        sum+=i
    for i in range(length-1):
        sum += i
    return sum+1

def LengthOfPeptide(length):
    if length == 1:
        return 1
    else:
        l = 3
        while Sum(l) < length:
            l+=1
        if Sum(l) == length:
            return l
        else:
            return l-1 

def GenerateSpectrum(peptide):
    spectrum = [0]

    for i in range(0,len(peptide)):
        sum = peptide[i]
        spectrum.append(sum)
        for j in range (i+1,len(peptide)):
            sum += peptide[j]
            spectrum.append(sum)

    return spectrum

def SameAcid(spectrum1, spectrum2):
    s2 = spectrum2.copy()
    SameSpectrum = []
    for i in spectrum1:
        if i in s2:
            SameSpectrum.append(i)
            s2.remove(i)
    return SameSpectrum

input = open("input","r")
ExperimentalSpectrum = []
for line in input:
    arr = line.strip().split()
    for i in arr:
        ExperimentalSpectrum.append(int(i))

PossibleAcids = SameAcid(ExperimentalSpectrum, BuildingBlock)
length = LengthOfPeptide(len(ExperimentalSpectrum))
#print(len(ExperimentalSpectrum))
#print(length)

PossiblePeptides = []
for i in PossibleAcids:
    PossiblePeptides.append([i])
Max = 0
for a in range(0,length-1):
    TempPeptides = []
    for i in PossiblePeptides:
        for j in PossibleAcids:
            TempPeptides.append(i+[j])
    for i in TempPeptides:
        Spectrum = GenerateSpectrum(i)
        Same = SameAcid(Spectrum, ExperimentalSpectrum)
        if len(Same) > Max:
            Max = len(Same)
            PossiblePeptides = []
            PossiblePeptides.append(i)
        elif len(Same) == Max:
            PossiblePeptides.append(i)

result = []
peptide = ""
for i in PossiblePeptides:
    for j in i:
        peptide = peptide+str(j)+"-"
    result.append(peptide[:-1])
    peptide = ""

for i in result:
    print(i, end = ' ')
print()