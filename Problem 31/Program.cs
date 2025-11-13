using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Project_Euler
{
    class Program
    {
        const int TARGET = 200;
        static int[] coins = new int[] { 1, 2, 5, 10, 20, 50, 100, 200 };

        static void Main(string[] args)
        {
            Console.WriteLine("Starting computation...");
            Console.WriteLine("Answer is: " + chooseCoins(coins.Length - 1, 0));
            Console.ReadLine();
        }

        static int chooseCoins(int current, int sum)
        {
            if (sum == TARGET)
                return 1;
            else if (sum > TARGET)
                return 0;
            if (current < 0)
                return 0;

            int combinations = 0;
            while (sum <= TARGET)
            {
                combinations += chooseCoins(current - 1, sum);
                sum += coins[current];
            }
            return combinations;
        }
    }
}
