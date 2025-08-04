
config const dir = "ImageDir";

proc assertEqual(actual, expected, testName: string) {
  if actual != expected {
    writeln("FAIL: ", testName, " — got ", actual, ", expected ", expected);
  } else {
    writeln("PASS: ", testName);
  }
}

proc main() {
    testGetArrayOfFiles();
    testEndsWith();
    testImageSize();
    testImageSizeHistogram();
    testRgbToGrayscale();
    testSobelEdgeDetection();
    testConvertPngToBmpInDir();
    writeln("All tests completed.");
}


proc testGetArrayOfFiles() {
    use sa7, BlockDist, Set;


    var expectedFileSet: set(string);
    expectedFileSet.add(dir+"/white-flowers.bmp");
    expectedFileSet.add(dir+"/white-flowers.png");
    expectedFileSet.add(dir+"/flower.png");
    expectedFileSet.add(dir+"/alligator.png");

    var files = getArrayOfFiles(dir);
    var fileSet: set(string);
    for file in files {
        fileSet.add(file);
    }

    assertEqual(fileSet.size, expectedFileSet.size,
                "getArrayOfFiles test - set size");
    
    for expectedFile in expectedFileSet {
        assertEqual(fileSet.contains(expectedFile), true,
                    "getArrayOfFiles test - contains " + expectedFile);
    }
}

proc testEndsWith() {
    use sa7;

    assertEqual(endsWith("image.png", ".jpg"), false, "endsWith test 1");
    assertEqual(endsWith("document.pdf", ".pdf"), true, "endsWith test 2");
    assertEqual(endsWith("archive.tar.gz", ".gz"), true, "endsWith test 3");
    assertEqual(endsWith("noextension", ".txt"), false, "endsWith test 4");
}

proc testConvertPngToBmpInDir() {
    use sa7;

    // Count the number of .png files in the directory before conversion
    var filesBefore = getArrayOfFiles(dir);

    var pngCountBefore = 0;
    for file in filesBefore {
        if endsWith(file, ".png") {
            pngCountBefore += 1;
        }
    }

    // Perform the conversion
    convertPngToBmpInDir(dir);

    // Count the number of .bmp files in the directory after conversion
    var filesAfter = getArrayOfFiles(dir);
    var bmpCountAfter = 0;
    for file in filesAfter {
        if endsWith(file, ".bmp") {
            bmpCountAfter += 1;
        }
    }

    // Assert that the number of .bmp files matches the number of .png files before conversion
    const expectedBmpCountAfter = 4;
    assertEqual(bmpCountAfter, pngCountBefore, "convertPngToBmpInDir test");
}

proc testImageSize() {
    use sa7, Image;

    var imageArray = readImage(dir+"/white-flowers.png", imageType.png);
    var size = imageSize(imageArray);
    assertEqual(size, 120400, "imageSize test");
}

proc testImageSizeHistogram() {
    use sa7;

    var histogram = imageSizeHistogram(dir,"png",10);
    var expectedHistogram = [0, 0, 0, 0, 0, 1, 0, 0, 1, 1];
    writeln("Histogram: ", histogram);
    writeln("Expected Histogram: ", expectedHistogram);

    // Assert the zeroth element of the histogram
    assertEqual(histogram[0], expectedHistogram[0], "imageSizeHistogram test - zeroth element");

    // Assert a middle value of the histogram
    const middleIndex = histogram.size / 2;
    assertEqual(histogram[middleIndex], expectedHistogram[middleIndex], "imageSizeHistogram test - middle element");
}

//test case for rgbToGrayscale from sa7.chpl
proc testRgbToGrayscale() {
    use sa7, Image;

    var imageArray = readImage(dir+"/white-flowers.png", imageType.png);
    var grayscaleImage = rgbToGrayscale(imageArray);
    writeImage("grayscale_output.png", imageType.png, grayscaleImage);

    // Check the first pixel value (example)
    assertEqual(grayscaleImage[0, 0], 9079434, "rgbToGrayscale test - pixel value");
}

// test case for the sobel edge detector from sa7.chpl
proc testSobelEdgeDetection() {
    use sa7, Image;

    var imageArray = readImage(dir+"/white-flowers.png", imageType.png);
    var grayScale = rgbToGrayscale(imageArray);
    var sobelImage = sobelEdgeDetection(grayScale);
    writeImage("sobel_output.png", imageType.png, sobelImage);

    // Check the first pixel value (example)
    assertEqual(sobelImage[0, 0], 0, "sobelEdgeDetection test - pixel value");
}
