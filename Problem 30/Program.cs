using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Problem_30
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Starting...");
            int sum = 0;
            for (int i = 2; i < 1000000; i++)
            {
                int[] digits = getDigits(i);
                int digitSum = 0;
                foreach (int digit in digits)
                {
                    digitSum += (int)Math.Pow(digit, 5);
                }
                if (digitSum == i)
                    sum += i;
            }
            Console.WriteLine("Answer: " + sum);
            Console.ReadLine();
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
    }
}
