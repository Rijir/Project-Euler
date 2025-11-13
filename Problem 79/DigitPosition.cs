using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Problem_79
{
    class DigitPosition
    {
        public DigitPosition()
        {
            before = new HashSet<int>();
            after = new HashSet<int>();
            toBeAdded = false;
        }

        public HashSet<int> before;
        public HashSet<int> after;
        public bool toBeAdded;
    }
}
