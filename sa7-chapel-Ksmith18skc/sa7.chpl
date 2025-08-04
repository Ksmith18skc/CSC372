use Image;
use FileSystem, BlockDist;
use IO;
use List;

// Time Spent: ~5 hours
// Collaborators: Chapel documentation, ChatGPT, StackOverflow, youtube, and the course materials

/* Some example code to get a feel for how this works
config const dir = "ImageDir";              
var fList = findFiles(dir);

// read in the first image in the directory and write out the
// color values for the first 10x10 pixel region for debugging.
var f = fList[0];
var imageArray = readImage(f, imageType.png);
const fmt = (rgbColor.red, rgbColor.green, rgbColor.blue);
// var colors = pixelToColor(imageArray, format=fmt);
//writeln("colors = ", colors);

writeImage(f+".bmp", imageType.bmp, imageArray);
*/

/***** Problem A *****/
// Stubbed out so you can see how testing works.

proc getArrayOfFiles(dirPath: string): [] string {
    var filenames = new list(string);

    for file in findFiles(dirPath) {
        // writeln("Found file: ", file);
        filenames.pushBack(file);
    }

    return filenames.toArray();
}

/***** Problem B *****/
// Stubbed out so you can see how testing works.

proc endsWith(s: string, suffix: string): bool {
    // Check if the string ends with the given suffix
    if s.size < suffix.size {
        return false;
    }
    for i in 0..suffix.size-1 {
        if s[s.size - suffix.size + i] != suffix[i] {
            return false;
        }
    }
    return true;
}

/***** Problem C *****/
// Stubbed out so you can see how testing works.

proc convertPngToBmpInDir(dirPath: string): void {
    // Get the list of files in the directory
    var filenames = getArrayOfFiles(dirPath);

    // Loop through each file and convert .png to .bmp
    for file in filenames {
        if endsWith(file, ".png") && !endsWith(file, ".png.bmp") {
            try {
                // Read the image
                var imageArray = readImage(file, imageType.png);

                // Replace the ".png" extension with ".bmp"
                var bmpFilename = file[0..#(file.size - 4)] + ".bmp";

                // Write the image as .bmp
                writeImage(bmpFilename, imageType.bmp, imageArray);
            } catch e: Error {
                writeln("Error converting ", file, ": ", e.message());
            }
        }
    }
}


/***** Problem D *****/
// Stubbed out so you can see how testing works.

proc imageSize(imageArray : [] pixelType): int {

    // debugging output
    // writeln("imageArray = ", imageArray);

    // Calculate the size of the image
    var numRows = imageArray.domain.dim(0).size;
    var numCols = imageArray.domain.dim(1).size;
    return numRows * numCols;
}

/***** Problem E *****/
// Stubbed out so you can see how testing works.
proc imageSizeHistogram(dirPath: string, fileExtension: string, numBuckets: int): [0..#numBuckets] int {
    use List;

    var histogram: [0..#numBuckets] int = 0;
    var filenames = getArrayOfFiles(dirPath);
    var imageSizes = new list(int);

    // Determine image type based on string
    var imgType: imageType;
    if fileExtension == "png" then
        imgType = imageType.png;
    else if fileExtension == "bmp" then
        imgType = imageType.bmp;
    else {
        writeln("Unsupported image type: ", fileExtension);
        return histogram;
    }

    // Read files and record image sizes
    for file in filenames {
        if endsWith(file, "." + fileExtension) {
            try {
                var imageArray = readImage(file, imgType);
                var size = imageSize(imageArray);
                imageSizes.pushBack(size);
            } catch e: Error {
                writeln("Error reading ", file, ": ", e.message());
            }
        }
    }

    if imageSizes.size == 0 {
        return histogram;
    }

    var maxSize = max reduce imageSizes.toArray();
    var bucketSize = ceil(maxSize:real / numBuckets):int;

    if bucketSize == 0 {
        histogram[0] = imageSizes.size;
        return histogram;
    }

    for size in imageSizes {
        var bucketIndex = size / bucketSize;
        if bucketIndex >= numBuckets then
            bucketIndex = numBuckets - 1;

        histogram[bucketIndex] += 1;
    }

    return histogram;
}



/***** Problem F *****/
// Stubbed out so you can see how testing works.
proc rgbToGrayscale(rgbImage : [?d] pixelType) : [d] pixelType
  where d.isRectangular() && d.rank == 2 {

  use Image;

  const fmt = (rgbColor.red, rgbColor.green, rgbColor.blue);

  // Convert to (r,g,b) tuples, preserving 2D domain
  var colors = pixelToColor(rgbImage, format=fmt);

  // Convert each pixel to grayscale
  forall (i, j) in d {
    const (r, g, b) = colors[i, j];

    const gray = (0.299 * r:real + 0.587 * g:real + 0.114 * b:real):int;

    colors[i, j] = (gray, gray, gray);
  }

  // Convert back to grayscale pixel format
  var grayImage = colorToPixel(colors, format=fmt);
  return grayImage;
}

/***** Problem G *****/
// Stubbed out so you can see how testing works.

proc sobelEdgeDetection(grayScale : [?d] int) : [d] int
  where d.isRectangular() && d.rank == 2 {

  // Sobel kernels
  const Gx = [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]];
  const Gy = [[-1, -2, -1], [ 0,  0,  0], [ 1,  2,  1]];

  const numRows = d.dim(0).size;
  const numCols = d.dim(1).size;

  // Create output array with same domain as input, initialized to 0
  var edgeImage: [d] int;

  // Loop only over inner pixels to avoid boundary issues
  forall i in 1..numRows-2 do
    forall j in 1..numCols-2 {
      var sumX = 0, sumY = 0;
      for di in -1..1 do
        for dj in -1..1 {
          const pixel = grayScale[i+di, j+dj];
          sumX += pixel * Gx[di+1][dj+1];
          sumY += pixel * Gy[di+1][dj+1];
        }
      const value = sqrt(sumX**2 + sumY**2) : int;
      edgeImage[i, j] = value;
    }

  return edgeImage;
}
