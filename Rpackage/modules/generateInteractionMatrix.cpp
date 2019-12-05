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
IntegerMatrix generateInteractionMatrix_cpp(DataFrame readTable, IntegerVector dim) {
  // Set up interaction matrix
  IntegerMatrix ixnMat( dim[0] , dim[1] );
  
  // Extract vectors from the input dataframe
  IntegerVector start1 = readTable["start1"];
  IntegerVector end1 = readTable["end1"];
  IntegerVector start2 = readTable["start2"];
  IntegerVector end2 = readTable["end2"];

  // Adjust for zero-indexing in C++
  start1 = start1 - 1;
  end1 = end1 - 1;
  start2 = start2 - 1;
  end2 = end2 - 1;

  // Loop through each row of the data frame
  int numRows = readTable.nrows();
  for (int i = 0; i < numRows; i++) {
    int start1_i = start1[i];
    int end1_i = end1[i];
    int start2_i = start2[i];
    int end2_i = end2[i];
    
    // Extract the submatrix corresponding to the read
    // Loop through each start and end position of the matrix and increment it by 1
    for (int x = start1_i; x <= end1_i; x++) {
      for (int y = start2_i; y <= end2_i; y++) {
        ixnMat(x,y)++;
      }
    }
    // end of submatrix loop
  }
  return(ixnMat);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
# Test code
df <- data.frame(start1=c(1,4), end1=c(5,6), start2=c(1,4), end2=c(4,6))
tm <- generateInteractionMatrix_cpp(df, c(8,8))
tm
*/
