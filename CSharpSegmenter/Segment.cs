using System;
using System.Collections.Generic;
using System.Text;

namespace CSharpSegmenter
{
    public class Segment
    {
        public Pixel[] Pixels { get; set; }

        public Segment(Pixel[] p)
        {
            Pixels = p;
        }

        public double[] Stddev()
        {
            int numColours = Pixels[0].Colour.Length;
            int numPixels = Pixels.Length;
            double[,] colourBands = new double[numColours, numPixels];
            double[] sums = new double[numColours];

            for (int i = 0; i < numPixels; i++)
            {
                byte[] colours = Pixels[i].Colour;
                for (int j = 0; j < numColours; j++)
                {
                    colourBands[j, i] = (double) colours[j];
                    sums[j] += (double) colours[j];
                }
            }

            double[] stddevs = new double[numColours];
            for (int i = 0; i < numColours; i++)
            {
                double mean = sums[i] / numPixels;
                double sqDiffSum = 0.0;

                for (int j = 0; j < numPixels; j++)
                {
                    sqDiffSum += Math.Pow(colourBands[i, j] - mean, 2.0);
                }
                stddevs[i] = Math.Sqrt(sqDiffSum / numPixels);
            }
            return stddevs;
        }

        public Segment Merge(Segment segment)
        {
            Pixel[] newArray = new Pixel[Pixels.Length + segment.Pixels.Length];
            Array.Copy(Pixels, newArray, Pixels.Length);
            Array.Copy(segment.Pixels, 0, newArray, Pixels.Length, segment.Pixels.Length);
            return new Segment(newArray);
        }

        public double MergeCost(Segment segment)
        {
            Segment newSegment = Merge(segment);
            double[] stddev1 = Stddev();
            double[] stddev2 = segment.Stddev();
            double[] stddev3 = newSegment.Stddev();
            double result = 0;

            for (int i = 0; i < stddev1.Length; i++)
            {
                result += stddev3[i] * newSegment.Pixels.Length;
                result -= stddev2[i] * segment.Pixels.Length;
                result -= stddev1[i] * Pixels.Length;
            }
            
            return result;
        }
    }
}
