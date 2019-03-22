module SegmentModule

type Coordinate = (int * int) // x, y coordinate of a pixel
type Colour = byte list       // one entry for each colour band, typically: [red, green and blue]

type Segment = 
    | Pixel of Coordinate * Colour
    | Parent of Segment * Segment

// get all the pixels of a given parent segment
let rec getChildren (segment: Segment) : Segment list =
    match segment with
    | Pixel(coord, colour) -> [Pixel(coord, colour)]
    | Parent(seg1, seg2) -> (getChildren seg1) @ (getChildren seg2)
    
let transpose (rows:list<list<'T>>) : list<list<'T>> =
    let n = List.length (List.head rows)
    List.init n (fun i -> (List.map (List.item i) rows))

// calculate standard deviation of a list of numbers
let numStdDev nums =
    let mean = nums |> Seq.average
    let sumSqDiff = nums |> Seq.sumBy (fun x -> (x - mean) ** 2.0)
    match (Seq.length nums) with
    | 1 -> 0.0
    | _ -> sumSqDiff / float (Seq.length nums) |> sqrt

// return a list of the cs of the pixel colours in the given segment
// the list contains one entry for each colour band, typically: [red, green and blue]
let stddev (segment: Segment) : float list =
    // extract the colours
    let colours = getChildren segment |> List.map (fun (Pixel(_, colour)) -> colour)
    // separate into colour bands
    let bands = transpose colours
    // convert each byte into floats, then calculate stddev of each colour band
    bands |> List.map (List.map (fun x -> float x)) |> List.map numStdDev

// determine the cost of merging the given segments: 
// equal to the standard deviation of the combined the segments minus the sum of the standard deviations of the individual segments, 
// weighted by their respective sizes and summed over all colour bands
let mergeCost segment1 segment2 : float = 
    let combined = Parent(segment1, segment2)
    let length1 = getChildren segment1 |> List.length |> float
    let length2 = getChildren segment2 |> List.length |> float
    let lengthCombined = length1 + length2

    let stddev1 = stddev segment1 |> List.sum
    let stddev2 = stddev segment2 |> List.sum
    let stddevCombined = stddev combined |> List.sum

    stddevCombined * lengthCombined - (stddev1 * length1 + stddev2 * length2)