import math

a = []
def primes_sieve2(limit):
    global a
    a = [True] * limit
    a[0] = a[1] = False

    for (i, isprime) in enumerate(a):
        if isprime:
            yield i
            for n in range(i*i, limit, i):     # Mark factors non-prime
                a[n] = False

def isTruncatable(p):
    # import pdb; pdb.set_trace()
    if int(p / 10) == 0:
        return False

    left_trunc = right_trunc = int(p)
    while right_trunc > 0:
        if a[left_trunc] and a[right_trunc]:
            right_trunc = int(right_trunc / 10)
            left_trunc = left_trunc % pow(10, int(math.log(left_trunc, 10)))
        else:
            return False

    return True

