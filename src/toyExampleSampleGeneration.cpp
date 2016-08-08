#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
List generateSamples(NumericVector thetas, int numberOfReplicates, int seed) {
  std::mt19937 engine(seed);
  std::normal_distribution<> normal(0.0, 1.0);

  List result(thetas.length());
  for (int particleNumber = 0; particleNumber < thetas.length(); ++particleNumber) {
    List replicates(numberOfReplicates);
    for (int replicate = 0; replicate < numberOfReplicates; ++replicate) {
      replicates[replicate] = normal(engine);
    }
    result[particleNumber] = replicates;
  };
  return result;
}
