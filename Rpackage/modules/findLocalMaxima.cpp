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

LogicalMatrix findLocalMaxima_cpp(IntegerMatrix ixnMat, int baseline=0, int search = 40, double dilate = 1.0) {
  
  // Set up output matrix to hold maxima
  LogicalMatrix maximaMat( ixnMat.nrow() , ixnMat.ncol()  );
  
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
        if (ixnMat(x,y) >= max(sub)/dilate){
          maximaMat(x,y) = TRUE;
        }
      } 
    }
  }
  return(maximaMat);
}



