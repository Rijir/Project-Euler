isPrimeList = []
def primes_sieve2(limit):
    global isPrimeList
    isPrimeList = [True] * limit
    isPrimeList[0] = isPrimeList[1] = False

    for (i, isprime) in enumerate(isPrimeList):
        if isprime:
            yield i
            for n in range(i*i, limit, i):     # Mark factors non-prime
                isPrimeList[n] = False
