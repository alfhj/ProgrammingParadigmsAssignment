module SegmentationModule

open SegmentModule

// Maps segments to their immediate parent segment that they are contained within (if any) 
type Segmentation = Map<Segment, Segment>

// Find the largest/top level segment that the given segment is a part of (based on the current segmentation)
let rec findRoot (segmentation: Segmentation) segment : Segment =
     match (segmentation.TryFind segment) with
     | Some parent -> findRoot segmentation parent
     | None -> segment

let isSameSegment segmentation segment1 segment2 =
    // two segments that share the same root segment belong to the same segment
    (findRoot segmentation segment1) = (findRoot segmentation segment2)

// Initially, every pixel/coordinate in the image is a separate Segment
// Note: this is a higher order function which given an image, 
// returns a function which maps each coordinate to its corresponding (initial) Segment (of kind Pixel)
let createPixelMap (image:TiffModule.Image) : (Coordinate -> Segment) =
    fun coordinate -> Pixel(coordinate, TiffModule.getColourBands image coordinate)
    
// Find the neighbouring segments of the given segment (assuming we are only segmenting the top corner of the image of size 2^N x 2^N)
// Note: this is a higher order function which given a pixelMap function and a size N, 
// returns a function which given a current segmentation, returns the set of Segments which are neighbours of a given segment
let createNeighboursFunction (pixelMap:Coordinate->Segment) (N:int) : (Segmentation -> Segment -> Set<Segment>) =
    let rec result (segmentation: Segmentation) (segment: Segment) : Set<Segment> =
        match segment with
        | Pixel((a, b), colour) ->
            // coordinates of the potential four neighbours
            let testNeighbours = [(a - 1, b); (a, b - 1); (a + 1, b); (a, b + 1)]
            // coordinates of neighbours that are within the area specified by N
            let okNeighbours = testNeighbours |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < (1 <<< N) && y < (1 <<< N))
            // (root) segments of valid coordinates
            let segmentList = okNeighbours |> List.map (fun coord -> pixelMap coord |> findRoot segmentation)
            // filtering out the neighbours that are part of the same segment as the original pixel
            let okSegments = segmentList|> List.filter (fun segment ->  segment |> isSameSegment segmentation (Pixel((a, b), colour)) |> not)
            Set.ofList okSegments
        | Parent(seg1, seg2) -> Set.union (result segmentation seg1) (result segmentation seg2)
    result

// The following are also higher order functions, which given some inputs, return a function which ...

 // Find the neighbour(s) of the given segment that has the (equal) best merge cost
 // (exclude neighbours if their merge cost is greater than the threshold)
let createBestNeighbourFunction (neighbours:Segmentation->Segment->Set<Segment>) (threshold:float) : (Segmentation->Segment->Set<Segment>) =
    let result (segmentation: Segmentation) (segment: Segment) : Set<Segment> =
        let root = segment
        let set = neighbours segmentation segment
        // finds the minimum merge cost
        let minimumCost =
            if (Set.isEmpty set) then 0.0
            else set |> Set.map (fun neighbour -> mergeCost segment neighbour) |> Set.minElement
        // keeps neighbour(s) with the least cost if the merge cost is less than the threshold
        set |> Set.filter (fun neighbour -> (mergeCost root neighbour) = minimumCost && (mergeCost root neighbour) <= threshold)
    result

// Try to find a neighbouring segmentB such that:
//     1) segmentB is one of the best neighbours of segment A, and 
//     2) segmentA is one of the best neighbours of segment B
// if such a mutally optimal neighbour exists then merge them,
// otherwise, choose one of segmentA's best neighbours (if any) and try to grow it instead (gradient descent)
let createTryGrowOneSegmentFunction (bestNeighbours:Segmentation->Segment->Set<Segment>) (pixelMap:Coordinate->Segment) : (Segmentation->Coordinate->Segmentation) =
    let rec result (segmentation: Segmentation) (coordinate: Coordinate) : Segmentation =
        let segment = pixelMap coordinate |> findRoot segmentation
        let neighbours = bestNeighbours segmentation segment
        // if no neighbours satisfy the criterium, return the unchanged segmentation
        if Set.isEmpty neighbours then segmentation
        else
            // keep the neighbours that are mutual
            let mutualNeighbours = neighbours |> Set.filter (fun neighbour -> bestNeighbours segmentation neighbour |> Set.contains segment)
            // if no such pair exists, do gradient descent
            if Set.isEmpty mutualNeighbours then
                let firstNeighbour = Set.minElement neighbours
                // finds the coordinate of any pixel within a segment
                let rec findCoord seg1 =
                    match seg1 with
                    | Pixel(coord, _) -> coord
                    | Parent(seg2, _) -> findCoord seg2
                let coord = findCoord firstNeighbour
                result segmentation coord
            else
                // if a mutual neighbour is found, merge it
                let firstNeighbour = Set.minElement neighbours
                let parent = Parent(segment, firstNeighbour)
                segmentation.Add(segment, parent).Add(firstNeighbour, parent)
    result

// Try to grow the segments corresponding to every pixel on the image in turn 
// (considering pixel coordinates in special dither order)
let createTryGrowAllCoordinatesFunction (tryGrowPixel:Segmentation->Coordinate->Segmentation) (N:int) : (Segmentation->Segmentation) =
    // applies tryGrowPixel to each dither coordinate, resulting in a final segmentation
    fun segmentation -> DitherModule.coordinates N |> Seq.fold tryGrowPixel segmentation

// Keep growing segments as above until no further merging is possible
let createGrowUntilNoChangeFunction (tryGrowAllCoordinates:Segmentation->Segmentation) : (Segmentation->Segmentation) =
    let rec result (segmentation: Segmentation) : Segmentation =
        let newSegmentation = tryGrowAllCoordinates segmentation
        // only return when no change has happened
        if newSegmentation = segmentation then segmentation
        else result newSegmentation
    result

// Segment the given image based on the given merge cost threshold, but only for the top left corner of the image of size (2^N x 2^N)
let segment (image:TiffModule.Image) (N: int) (threshold:float) : (Coordinate -> Segment) =
    let segmentation = Map.empty
    let pixelMap = createPixelMap image
    let neighbours = createNeighboursFunction pixelMap N
    let bestNeighbours = createBestNeighbourFunction neighbours threshold
    let tryGrowSegment = createTryGrowOneSegmentFunction bestNeighbours pixelMap
    let growSegments = createTryGrowAllCoordinatesFunction tryGrowSegment N
    let grow = createGrowUntilNoChangeFunction growSegments
    let finalSegmentation = grow segmentation
    fun coord -> pixelMap coord |> findRoot finalSegmentation