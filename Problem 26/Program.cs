using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Problem_26
{
    class Program
    {
        static void Main(string[] args)
        {
            int max = 0;
            int ans = 0;
            for (int i = 1; i < 1000; i++)
            {
                int cLength = measureCycle(i);
                if (cLength > max)
                {
                    max = cLength;
                    ans = i;
                }
            }
            Console.WriteLine(ans);
            Console.ReadLine();
        }

        static int measureCycle(int d)
        {
            List<int> remainders = new List<int>();
            int n = 1;
            while (n != 0)
            {
                remainders.Add(n);
                while (n < d)
                {
                    n *= 10;
                }
                n %= d;
                if (n != 0)
                {
                    int lastOccurence = remainders.FindIndex(x => Math.Log10((double)x / (double)n) % 1 == 0);
                    if (lastOccurence != -1)
                        return remainders.Count - lastOccurence;
                }
            }
            return 0;
        }
    }
}
