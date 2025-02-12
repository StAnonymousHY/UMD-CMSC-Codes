#fixed point
x = 1
#f = pow((2-pow(x,4))/3, 0.5)
f = pow(2-3*pow(x,2), 0.25)

for i in range(4):
    x = f
    print(x)
    #f = pow((2-pow(x,4))/3, 0.5)
    f = pow(2-3*pow(x,2), 0.25)
    

'''
#Secant
x0 = 0
x1 = 1
new = 1
while abs(new) > 0.000001:
    upper = (230*pow(x1,4) + 18*pow(x1, 3) + 9*pow(x1,2) - 221*x1 - 9)*(x1-x0)
    lower = 230*pow(x1,4) + 18*pow(x1, 3) + 9*pow(x1,2) - 221*x1 - 9 - (230*pow(x0,4) + 18*pow(x0, 3) + 9*pow(x0,2) - 221*x0 - 9)
    x2 = round(x1 - upper/lower, 10)
    new = 230*pow(x2,4) + 18*pow(x2, 3) + 9*pow(x2,2) - 221*x2 - 9
    print(x2)
    print(new)
    print()
    x0 = x1
    x1 = x2
'''
'''
# Newton
x = -0.5
new = 1

while (abs(new) > 0.000001):

    upper = 230*pow(x,4) + 18*pow(x, 3) + 9*pow(x,2) - 221*x - 9

    lower = 920*pow(x,3) + 54*pow(x,2) + 18*x - 221

    result = round(x - (upper/lower), 10)

    new = round(230*pow(result,4) + 18*pow(result, 3) + 9*pow(result,2) - 221*result - 9, 20)

    print(result)
    print(new)
    print()

    x = result
'''