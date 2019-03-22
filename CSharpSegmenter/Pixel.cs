using System;
using System.Collections.Generic;
using System.Text;

namespace CSharpSegmenter
{
    public class Pixel
    {
        private (int, int) coordinate;
        private byte[] colour;
        private static Dictionary<(int, int), Pixel> PixelList = new Dictionary<(int, int), Pixel>();
        
        public (int, int) Coordinate { get => coordinate; set => coordinate = value; }
        public byte[] Colour { get => colour; set => colour = value; }

        public Pixel((int, int) coordinate, byte[] colour)
        {
            Coordinate = coordinate;
            Colour = colour;
            PixelList.Add(coordinate, this);
        }

        public static Pixel FindPixel((int, int) coordinate)
        {
            return PixelList[coordinate];
        }

        public List<Pixel> Neighbours(int N)
        {
            List<Pixel> result = new List<Pixel>();

            (int, int)[] coordinates = {
                (Coordinate.Item1 - 1, Coordinate.Item2),
                (Coordinate.Item1 + 1, Coordinate.Item2),
                (Coordinate.Item1, Coordinate.Item2 - 1),
                (Coordinate.Item1, Coordinate.Item2 + 1)
            };

            foreach ((int x, int y) in coordinates)
            {
                if (x >= 0 && y >= 0 && x < 1 << N && y < 1 << N)
                    result.Add(PixelList[(x, y)]);
            }

            return result;
        }
    }
}
