using System.Reflection;
using System;

namespace FSharpCoreResolver
{
    class Program
    {
        static int Main(string[] args)
        {
            foreach (string version in args)
            {
                try {
                    var fullName = "FSharp.Core, Version=" + version + ", Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a";
                    var assembly = Assembly.ReflectionOnlyLoad(fullName);
                    if (assembly.GlobalAssemblyCache) {
                        Console.WriteLine(assembly.Location);
                        return 0;
                    }
                }
                catch {
                }
            }
            return 1;
        }
    }
}
