input = open("input", "r")
A_cnt = 0
T_cnt = 0
C_cnt = 0
G_cnt = 0
for line in input:
    for character in line: 
        if character == "A":
            A_cnt = A_cnt+1
        elif character == "T":
            T_cnt = T_cnt+1
        elif character == "G":
            G_cnt = G_cnt+1
        elif character == "C":
            C_cnt = C_cnt+1

output = open("output", "w")

output.write(str(A_cnt)+" "+str(C_cnt)+" "+str(G_cnt)+" "+str(T_cnt))