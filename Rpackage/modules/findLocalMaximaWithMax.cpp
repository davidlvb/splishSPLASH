#include <Rcpp.h>
using namespace Rcpp;

  //////////////////////////////////////
 //  generateInteractionMatrix_cpp   //
//////////////////////////////////////
//
// 10 August 2019
// DLVB
//
// This function generates an interaction matrix from a dataframe of reads

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]

IntegerMatrix findLocalMaximaWithMax_cpp(IntegerMatrix ixnMat, int baseline=0, int search = 40, double dilate = 1.0) {
  
  // Set up output matrix to hold maxima
  IntegerMatrix maximaMat( ixnMat.nrow() , ixnMat.ncol()  );
  
  // determine dimensions of ixnMat
  int matNrow = ixnMat.nrow();
  int matNcol = ixnMat.ncol();
  
  // Loop through each element of the vector
  for (int x = 0; x < matNrow; x++) {
    for (int y = 0; y < matNcol; y++) {
      // Only carry out local maxima search if pixel is above baseline
      if (ixnMat(x,y) > baseline){
        // determine search window, ensuring we do not go beyond bounds of ixnMat
        int xStart = std::max(x-search, 0);
        int xEnd = std::min(x+search,matNrow-1);
        int yStart = std::max(y-search, 0);
        int yEnd = std::min(y+search,matNcol-1);
        // get submatrix 
        IntegerMatrix sub = ixnMat ( Range(xStart, xEnd), Range(yStart, yEnd) );
        int maxVal = max(sub);
        if (ixnMat(x,y) >= maxVal/dilate){
          maximaMat(x,y) = maxVal;
        }
      } 
    }
  }
  return(maximaMat);
}



