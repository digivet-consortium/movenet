#include <Rcpp.h>

double test(double a, double b)
{
  return a+b;
}

RCPP_MODULE(movenet_module){

	using namespace Rcpp;
	
	function("Rcpp_test", &test);
}
