using System;
using System.Collections.Generic;
using System.Text;

namespace CSharpSegmenter
{
    public class Segmentation
    {
        // a segmentation is a list of segments
        public List<Segment> Segments;

        public Segmentation()
        {
            Segments = new List<Segment>();
        }

        // adds a new segment to the segmentation while removing the two being merged
        public void MergeSegments(Segment s1, Segment s2)
        {
            Segment s3 = s1.Merge(s2);
            Segments.Remove(s1);
            Segments.Remove(s2);
            Segments.Add(s3);
        }

        public void Populate(TiffImage image, int N)
        {
            for (int x = 0; x < 1 << N; x++)
            {
                for (int y = 0; y < 1 << N; y++)
                {
                    Pixel pixel = new Pixel((x, y), image.getColourBands(x, y));
                    Segment segment = new Segment(new Pixel[] { pixel });
                    Segments.Add(segment);
                }
            }
        }

        // finds the segment of which pixel is a part of
        public Segment FindSegment(Pixel pixel)
        {
            foreach (Segment s in Segments)
            {
                foreach (Pixel p in s.Pixels)
                {
                    if (pixel == p) return s;
                }

            }

            return null;
        }

        public HashSet<Segment> FindNeighbours(Segment s, int N)
        {
            HashSet<Segment> result = new HashSet<Segment>();

            foreach (Pixel p in s.Pixels)
            {
                foreach (Pixel n in p.Neighbours(N))
                {
                    result.Add(FindSegment(n));
                }
            }

            result.Remove(s);
            return result;
        }

        public HashSet<Segment> FindBestNeighbours(Segment segment, double threshold, int N)
        {
            HashSet<Segment> neighbours = FindNeighbours(segment, N);

            bool isAboveThreshold(Segment n)
            {
                return segment.MergeCost(n) > threshold;
            }

            neighbours.RemoveWhere(isAboveThreshold);
            return neighbours;
        }

        public HashSet<Segment> FindMutualBestNeighbours(Segment segment, HashSet<Segment> bestNeighbours, double threshold, int N)
        {
            bool isNotMutual(Segment n)
            {
                return ! FindBestNeighbours(n, threshold, N).Contains(segment);
            }

            bestNeighbours.RemoveWhere(isNotMutual);
            return bestNeighbours;
        }

        // returns true if a change was made
        public bool GrowSegment(Segment segment, double threshold, int N)
        {
            HashSet<Segment> bestNeighbours = FindBestNeighbours(segment, threshold, N);
            if (bestNeighbours.Count == 0) return false;

            HashSet<Segment> mutualNeighbours = FindMutualBestNeighbours(segment, bestNeighbours, threshold, N);
            if (mutualNeighbours.Count == 0)
            {
                Segment[] neighbours = new Segment[bestNeighbours.Count];
                bestNeighbours.CopyTo(neighbours);
                Segment firstNeighbour = neighbours[0];
                return GrowSegment(firstNeighbour, threshold, N);
            }
            else
            {
                Segment[] neighbours = new Segment[mutualNeighbours.Count];
                mutualNeighbours.CopyTo(neighbours);
                Segment firstNeighbour = neighbours[0];
                MergeSegments(segment, firstNeighbour);
                return true;
            }
        }
    }
}
