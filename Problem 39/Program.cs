using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Problem_39
{
    class Program
    {
        static void Main(string[] args)
        {
            int maxCount = 0;
            int maxP = -1;
            for (int p = 12; p <= 1000; p++)
            {
                Console.WriteLine("p = " + p);
                int count = 0;
                for (int a = 2; a < p / 3; a++)
                {
                    for (int b = 1; b < a; b++)
                    {
                        int c = (int)Math.Sqrt(Math.Pow(a, 2) + Math.Pow(b, 2));
                        if (IsPerfectSquare(c) && p == a + b + c) {
                            count++;
                        }
                    }
                }
                if (count > maxCount)
                {
                    maxCount = count;
                    maxP = p;
                }
            }

            Console.WriteLine("Maximizing P is " + maxP);
            Console.WriteLine("With count " + maxCount);
            Console.ReadLine();
        }

        static bool IsPerfectSquare(int c)
        {
             return Math.Pow((int)Math.Sqrt(c), 2) == c;
        }
    }
} 