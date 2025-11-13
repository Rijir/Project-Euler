#!/usr/bin/python
import sys, os
sys.path.append(os.path.abspath(".."))
import primes

prime_list = list(primes.primes_sieve2(100))

def primeFactorize(n):
    global prime_list

    if len(primes.isPrimeList) <= n:
        prime_list = list(primes.primes_sieve2(n*2))

    if primes.isPrimeList[n]:
        return [n]
    else:
        for p in prime_list:
            if n % p == 0:
                return [p] + primeFactorize(int(n / p))

        assert(False, "There should be a prime factor if n is not prime")

cur = 2
while(True):
    found = True
    for i in range(4):
        num_factors = len(set(primeFactorize(cur + i)))
        if num_factors != 4:
            cur = (cur + i + 1)
            found = False
            break

    if found:
        print(cur)
        break
