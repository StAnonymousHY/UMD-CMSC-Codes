input = open("input", "r")
input.readline()

whole_str = ""
for line in input:
    whole_str = whole_str+line.strip()

sp = [0]

suffix_index = 1
longest_prefix_length = 0

while suffix_index < len(whole_str):
    if whole_str[suffix_index] == whole_str[longest_prefix_length]:
        longest_prefix_length = longest_prefix_length+1
        sp.append(longest_prefix_length)
        suffix_index = suffix_index+1
    else:
        if longest_prefix_length == 0:
            sp.append(0)
            suffix_index = suffix_index+1
        else:
            #This part aims to find the current longest prefix that is the same as the suffix
            #After finding the longest prefix, then we can access the last sp value of the fitting prefix
            #If the last sp value of the current longest fitting prefix is not 0, then it means that the code can ignore the first x characters of the prefix in next iteration 
            
            #e.g. CAGCATGGTATCACAGCAGAG
            #Up until CAGCATGGTATCACAGCA, the sp array would be [0,0,0,1,2,0,0,0,0,0,0,1,2,1,2,3,4,5], and the next character would be G, where G != T, and longest_prefix_length is 5
            #In this case, we notice that CAGCATGGTATCACAGCA has the same prefix and suffix of length 5
            #In the prefix, we also notice that the prefix has the same sub-prefix and sub-suffix of length 2
            #Thus, when starting a new iteration of comparison for the whole string, we can actually skip the first 2 characters for the prefix and directly compare the 3rd character of the prefix with G
            #Where the index of the 3rd character of the prefix is 2, which is sp[5-4] = sp[4]
            longest_prefix_length = sp[longest_prefix_length-1]

result = ""

for i in sp:
    result = result + str(i) + " "

print(result.strip())


#Recursion version, but will trigger recursion error when the string is too long :(
def KMP(whole_str, sp, suffix_index, longest_prefix_length):
    if suffix_index >= len(whole_str):
        return sp
    else:
        if whole_str[suffix_index] == whole_str[longest_prefix_length]:
            suffix_index = suffix_index+1
            longest_prefix_length = longest_prefix_length+1
            sp.append(longest_prefix_length)
        else:
            if longest_prefix_length == 0:
                sp.append(0)
                suffix_index = suffix_index+1
            else:
                longest_prefix_length = sp[longest_prefix_length-1]
        return KMP(whole_str, sp, suffix_index, longest_prefix_length)