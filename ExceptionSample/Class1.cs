using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ExceptionSample
{
    public class MyException : Exception
    {
        public MyException(string message, Exception innerException) : base(message, innerException)
        {
        }
    }

    public class Class1
    {
        public void JustDoIt()
        {
            throw new MyException();
        }

        public void Log<T>(T exception) where T:Exception
        {
            
        }

        public void Call()
        {
            try
            {
                JustDoIt();
            }
            catch (MyException ex)
            {
                
            }
            catch (Exception ex)
            {
                Log(ex);
                throw;
            }
        }

    }
}
