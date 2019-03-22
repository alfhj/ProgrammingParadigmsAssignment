using System;
using System.Collections.Generic;

namespace CSharpSegmenter
{
    public class Program
    {
        static void Main(string[] args)
        {
            int N = 5;
            double threshold = 800.0;
            Segmentation s = new Segmentation();
            TiffImage image = new TiffImage("..\\TestImages\\L15-3792E-1717N-Q4.tif");
            s.Populate(image, N);
            
            bool changeMade;
            do
            {
                changeMade = false;
                foreach ((int, int) coordinate in Dither.coordinates(N))
                {
                    Segment segment = s.FindSegment(Pixel.FindPixel(coordinate));
                    changeMade |= s.GrowSegment(segment, threshold, N);
                }
            } while (changeMade);

            image.overlaySegmentation("segmented.tif", N, s);
        }
    }
}
