# p01
def p01(L):
    return L[-1]
    
def p02(L):
    return L[-2]
    
def p03(k,L):
    return L[k]

# Length of a list, recursive version
def p04(L):
    if(L==[]):
        return 0
    return 1+p04(L[1:])

# Reverse a list 
def p05(L):
    return reverse(L,[])

def reverse(L,L2):
    if(L == []):
        return L2
    L2.append(L[0])
    return reverse(L[1:], L2)

# Determine if a list is a palindrome
def p06(L):
    return L == p05(L)


# Flatten a list
def p07(L):
    if(L==[]):
        return []
    if(type(L[0]) == list):
        return p07(L[0])+p07(L[1:])
    return [L[0]]+p07(L[1:])





