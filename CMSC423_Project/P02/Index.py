input = open("input", "r")
str_to_find = input.readline().strip()

whole_str = ""
for line in input:
    whole_str = whole_str+line.strip()

result = ""
flag = True
start_index = 0

for i in range(len(whole_str)):
    index = i
    for j in range(len(str_to_find)):
        if index>=len(whole_str) or whole_str[index] != str_to_find[j]: 
            flag = False
            break
        else:
            index = index+1
    if flag:
        result = result+str(start_index)+" "
    flag = True
    start_index = start_index+1

print(result.strip())