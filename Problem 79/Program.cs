using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Problem_79
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] lines = System.IO.File.ReadAllLines(@"C:\Users\Tim\Documents\Visual Studio 2015\Projects\Project Euler\Problem 79\keylog.txt");
            DigitPosition[] positions = new DigitPosition[10];
            for (int i = 0; i < positions.Length; i++)
            {
                positions[i] = new DigitPosition();
            }

            for (int i = 0; i < lines.Length; i++)
            {
                int digit1 = (int)Char.GetNumericValue(lines[i][0]);
                int digit2 = (int)Char.GetNumericValue(lines[i][1]);
                int digit3 = (int)Char.GetNumericValue(lines[i][2]);

                if (positions[digit1].before.Contains(digit2) || positions[digit1].before.Contains(digit3)
                    || positions[digit2].after.Contains(digit1) || positions[digit2].before.Contains(digit3)
                    || positions[digit3].after.Contains(digit1) || positions[digit3].after.Contains(digit2))
                {
                    Console.WriteLine("Welp this problem is more complicated");
                    return;
                }
                positions[digit1].after.Add(digit2);
                positions[digit1].after.Add(digit3);
                positions[digit1].toBeAdded = true;

                positions[digit2].before.Add(digit1);
                positions[digit2].after.Add(digit3);
                positions[digit2].toBeAdded = true;

                positions[digit3].before.Add(digit1);
                positions[digit3].before.Add(digit2);
                positions[digit3].toBeAdded = true;
            }

            bool moreToAdd = false;
            for (int i = 0; i < positions.Length; i++)
            {
                moreToAdd |= positions[i].toBeAdded;
                if (positions[i].before.Count == 0 && positions[i].toBeAdded)
                {
                    Console.Write(i);
                    for (int j = 0; j < positions.Length; j++)
                    {
                        positions[j].before.Remove(i);
                    }
                    positions[i].toBeAdded = false;
                    // start at the beginning
                    moreToAdd = false;
                    i = -1;
                    continue;
                }
            }

            Console.WriteLine();
            if (moreToAdd)
            {
                Console.WriteLine("Something went wrong. There were still more digits to place");
            }
            Console.ReadLine();
        }
    }
} 