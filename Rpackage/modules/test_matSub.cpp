#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
IntegerMatrix generateInteractionMatrix_cpp(DataFrame readTable, IntegerVector dim) {
  double src[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25};
  IntegerMatrix mat(5, 5, src);
  Rcout << "Matrix:\n";
  Rf_PrintValue(mat);
  
  // Submatrix from (2, 2) to (3, 4)
  IntegerMatrix sub = mat(Range(1, 2), Range(1, 4));
  Rcout << "\n(2, 2) to (3, 4) block:\n";
  Rf_PrintValue(sub);
  
  
  return mat;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
a <- generateInteractionMatrix_cpp(data.frame(), c(1,2))
  */
