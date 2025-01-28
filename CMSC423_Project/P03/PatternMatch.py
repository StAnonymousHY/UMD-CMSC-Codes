input = open("input", "r")
pattern = input.readline().strip()

whole_str = ""
for line in input:
    whole_str = whole_str+line.strip()

def KMP(whole_str, sp, suffix_index, longest_prefix_length) -> list[int]:
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
                longest_prefix_length = sp[longest_prefix_length-1]
    return sp

M = len(pattern)
N = len(whole_str)

lps = KMP(pattern, [0], 1, 0)
result_arr = []
print(lps)
j = 0
i = 0  # index for txt[]
while (N - i) >= (M - j):
    if pattern[j] == whole_str[i]:
        i += 1
        j += 1

    if j == M:
        result_arr.append(i-j)
        j = lps[j-1]

    # mismatch after j matches
    elif i < N and pattern[j] != whole_str[i]:
        if j != 0:
            j = lps[j-1]
        else:
            i += 1

print(result_arr)