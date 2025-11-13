using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Problem_67
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] lines = System.IO.File.ReadAllLines(@"C:\Users\Tim\Documents\Visual Studio 2015\Projects\Project Euler\Problem 67\triangle.txt");

            int[] previousLine = new int[lines.Length];
            for (int i = lines.Length - 1; i >= 0; i--)
            {
                string[] numberStrings = lines[i].Split(new Char[]{ ' ' });
                int[] numbers = new int[numberStrings.Length];

                for (int j = 0; j < numberStrings.Length; j++)
                {
                    numbers[j] = int.Parse(numberStrings[j]);
                }

                if (i != lines.Length - 1)
                {
                    for (int j = 0; j < numbers.Length; j++)
                    {
                        numbers[j] += Math.Max(previousLine[j], previousLine[j + 1]);
                    }
                }
                previousLine = numbers;
            }

            Console.WriteLine(previousLine[0]);
            Console.ReadLine();
        }
    }
}
