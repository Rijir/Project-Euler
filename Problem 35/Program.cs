using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Problem_35
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Starting...");
            Console.WriteLine("Finding all primes...");
            HashSet<int> primes = getPrimes(1000000);
            Console.WriteLine("Found " + primes.Count + " primes");
            Console.WriteLine("Searching primes for circular primes...");
            int numCircularPrimes = 0;
            foreach (int prime in primes)
            {
                bool isCircular = true;
                int[] digits = getDigits(prime);

                for (int i = 0; i < digits.Length - 1; i++)
                {
                    rotate(digits);
                    isCircular &= primes.Contains(fromDigits(digits));
                }

                if (isCircular)
                {
                    Console.WriteLine("Found: " + prime);
                    numCircularPrimes++;
                }
            }
            Console.WriteLine("Circular primes below 1 million: " + numCircularPrimes);
            Console.ReadLine();
        }

        static void rotate(int[] array)
        {
            int temp = array.Last();
            for (int i = array.Length - 1; i > 0; i--)
            {
                array[i] = array[i - 1];
            }
            array[0] = temp;
        }

        static int[] getDigits(int n)
        {
            int numDigits = (int)Math.Log10(n) + 1;
            int[] res = new int[numDigits];
            for (int i = 0; i < numDigits; i++)
            {
                res[i] = n % 10;
                n /= 10;
            }
            return res;
        }

        static int fromDigits(int[] digits)
        {
            int res = 0;
            for (int i = 0; i < digits.Length; i++)
            {
                res += digits[i] * (int)Math.Pow(10, i);
            }
            return res;
        }

        static HashSet<int> getPrimes(int below)
        {
            HashSet<int> primes = new HashSet<int>();
            for (int i = 2; i < below; i++)
            {
                bool divided = false;
                foreach (int prime in primes)
                {
                    if (i % prime == 0)
                    {
                        divided = true;
                        break;
                    }
                }
                if (!divided)
                    primes.Add(i);
            }
            return primes;
        }
    }
}
