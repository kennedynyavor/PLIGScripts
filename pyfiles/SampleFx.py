
def fact(num):
    ans = 1
    n = int(num)+1
    if num < 0:
        ans = None
    elif num == 0:
        ans = 1
    elif num >=1 and num < 2:
        ans = 1
    else:
        while n > 1:
            ans = ans * (n-1)
            n = n-1
    return ans
