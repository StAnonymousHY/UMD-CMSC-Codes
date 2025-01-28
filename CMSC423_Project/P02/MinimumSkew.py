input = open("input", "r")
whole_str = ""

for line in input:
    whole_str = whole_str+line.strip()

arr = []
cnt = 0
minimum = 1

for character in whole_str:
    if character == "G":
        cnt = cnt+1
    elif character == "C":
        cnt = cnt-1
    
    arr.append(cnt)
    
    if cnt < minimum:
        minimum = cnt

result = ""

for i in range(len(arr)):
    if arr[i] == minimum:
        result = result+str(i+1)+" "

print(result.strip())