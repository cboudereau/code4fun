using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace CSharp
{
    public class AsyncPerfTest : IDisposable
    {
        private readonly string workingDir;

        public AsyncPerfTest()
        {
            workingDir = Path.Combine(@"d:\asynccharp", Path.GetRandomFileName());

            Clean();
            Directory.CreateDirectory(workingDir);
        }

        private void Clean()
        {
            if (Directory.Exists(workingDir))
            {
                Directory.Delete(workingDir, true);
            }
        }

        private string GetFile()
        {
            return Path.Combine(workingDir, Path.GetRandomFileName());
        }

        [Fact]
        public void Bench_async_copy()
        {
            var stopwatch = Stopwatch.StartNew();
            var tasks = GetWriteTasks(30000, Content()).ToArray();
            Assert.Equal(0, stopwatch.ElapsedMilliseconds);
        }

        private static string Content()
        {
            return "Hello\r\n";
        }

        [Fact]
        public void Bench_sync_copy()
        {
            var stopwatch = Stopwatch.StartNew();
            var content = Content();
            for (var i = 0; i < 30000; i++)
            {
                WriteSync(content);
            }
            Assert.Equal(0, stopwatch.ElapsedMilliseconds);
        }

        private void WriteSync(string content)
        {
            var encodedText = Encoding.Unicode.GetBytes(content);

            using (var sourceStream = new FileStream(GetFile(), FileMode.Append, FileAccess.Write, FileShare.None, 4096, true))
            {
                sourceStream.Write(encodedText, 0, encodedText.Length);
            }
            ;
        }

        private IEnumerable<Task> GetWriteTasks(int number, string content)
        {
            for (var i = 0; i < number; i++)
            {
                yield return ProcessWrite(GetFile(), content);
            }
        }

        static async Task ProcessWrite(string filePath, string text)
        {
            await WriteTextAsync(filePath, text);
        }

        static async Task WriteTextAsync(string filePath, string text)
        {
            var encodedText = Encoding.Unicode.GetBytes(text);

            using (var sourceStream = new FileStream(filePath, FileMode.Append, FileAccess.Write, FileShare.None, 4096, true))
            {
                await sourceStream.WriteAsync(encodedText, 0, encodedText.Length);
            };
        }

        static async Task ProcessRead()
        {
            const string filePath = @"c:\temp2\temp2.txt";

            if (File.Exists(filePath) == false)
            {
                Console.WriteLine("file not found: " + filePath);
            }
            else
            {
                try
                {
                    var text = await ReadTextAsync(filePath);
                    Console.WriteLine(text);
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }
        }

        static async Task<string> ReadTextAsync(string filePath)
        {
            using (var sourceStream = new FileStream(filePath,
                FileMode.Open, FileAccess.Read, FileShare.Read,
                bufferSize: 4096, useAsync: true))
            {
                var sb = new StringBuilder();

                var buffer = new byte[0x1000];
                int numRead;
                while ((numRead = await sourceStream.ReadAsync(buffer, 0, buffer.Length)) != 0)
                {
                    var text = Encoding.Unicode.GetString(buffer, 0, numRead);
                    sb.Append(text);
                }

                return sb.ToString();
            }
        }

        public void Dispose()
        {
            Clean();
        }
    }
}
