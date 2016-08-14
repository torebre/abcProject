#include <Rcpp.h>

using namespace Rcpp;


std::mt19937 createEngine(int seed) {
  if (seed != -1) {
    std::mt19937 engine(seed);
    return engine;
  } else {
    std::random_device rd;
    std::mt19937 engine(rd());
    return engine;
  }
}

// [[Rcpp::export]]
List generateSamples(NumericVector thetas, int numberOfReplicates, int seed) {
  std::mt19937 engine = createEngine(seed);
  std::normal_distribution<> normal(0.0, 1.0);
  std::uniform_real_distribution<> uniform(0.0, 1.0);

  List result(thetas.length());
  for (int particleNumber = 0; particleNumber < thetas.length(); ++particleNumber) {
    List replicates(numberOfReplicates);
    for (int replicate = 0; replicate < numberOfReplicates; ++replicate) {
      if (uniform(engine) < 0.5) {
        replicates[replicate] = normal(engine) + thetas[particleNumber];
      } else {
        // TODO Check that the standard deviation is correct
        replicates[replicate] = normal(engine) / 10 + thetas[particleNumber];
      }
    }
    result[particleNumber] = replicates;
  };
  return result;
}
