using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TestAppCsharp
{
    class Program
    {
        static void Main(string[] args)
        {
            double a = 2;
            double b = 3;
            double x = 0.4;
            double answer = ClassLibrary1.Class1.InverseIncompleteBeta(a, b, x);

            double mean = 70;
            double std = 4;
            double quantile = 0.90;
            double answer2 = ClassLibrary1.Class1.NormalDistribution(mean, std, quantile);

            Console.WriteLine(answer.ToString()+" "+answer2.ToString());
            Console.ReadLine();
        }
    }
}
