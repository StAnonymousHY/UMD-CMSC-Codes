def backpack(capacity,current_weight, weights, index,final_items):
    if index >= len(weights):
        return current_weight,final_items
    else:
        if current_weight+weights[index]>capacity:
            return backpack(capacity, current_weight, weights, index+1,final_items)
        else:
            yes = backpack(capacity, current_weight+weights[index],weights, index+1,final_items+[weights[index]])
            no = backpack(capacity, current_weight,weights, index+1,final_items)
            if yes[0]>=no[0]:
                #print(weights[index])
                #print(final_items+[weights[index]])
                return yes
            else:
                return no
'''
def backpack_2(capacity,current_weight, weights, index,final_items):
    if index < 0:
        return current_weight,final_items
    else:
        if current_weight+weights[index]>capacity:
            return backpack_2(capacity, current_weight, weights, index-1,final_items)
        else:
            yes = backpack_2(capacity, current_weight+weights[index],weights, index-1,final_items+[weights[index]])
            no = backpack_2(capacity, current_weight,weights, index-1,final_items)
            if yes[0]>no[0]:
                #print(weights[index])
                #print(final_items+[weights[index]])
                return yes
            else:
                return no
                '''
final_items = []
print(backpack(82,0,[3, 5, 9, 12, 13, 15, 20, 22, 24],0,final_items))
'''
final_items = []
print(backpack_2(82, 0, [3, 5, 9, 12, 13, 15, 20, 22, 24],len([3, 5, 9, 12, 13, 15, 20, 22, 24])-1,final_items))
'''