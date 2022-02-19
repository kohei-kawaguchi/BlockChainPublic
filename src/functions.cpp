// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppEigen.h which pulls Rcpp.h in for us
#include <RcppEigen.h>

// [[Rcpp::export]]
double compute_loglikelihood_arrival_rate_rcpp(
    Eigen::VectorXd theta,
    Eigen::VectorXd winning_rate,
    Eigen::VectorXd epoch_time,
    Eigen::VectorXd generated,
    Eigen::MatrixXd x_variable
) {
  Eigen::VectorXd term = winning_rate.array().log().matrix() + x_variable * theta;
  Eigen::VectorXd loglikelihood = - epoch_time.array() * term.array().exp() + generated.array() * term.array();
  double output = loglikelihood.sum();
  return output;
}

// [[Rcpp::export]]
Eigen::VectorXd compute_score_arrival_rate_rcpp(
    Eigen::VectorXd theta,
    Eigen::VectorXd winning_rate,
    Eigen::VectorXd epoch_time,
    Eigen::VectorXd generated,
    Eigen::MatrixXd x_variable
) {
  Eigen::VectorXd term = winning_rate.array().log().matrix() + x_variable * theta;
  Eigen::MatrixXd multiplier_1 = - epoch_time.array() * term.array().exp();
  Eigen::MatrixXd multiplier_2 = generated;
  
  Eigen::MatrixXd score = x_variable;
  Eigen::MatrixXd ones = Eigen::MatrixXd::Constant(1, score.cols(), 1);
  multiplier_1 = multiplier_1 * ones;
  multiplier_2 = multiplier_2 * ones;
  score = (score.array() * multiplier_1.array() + score.array() * multiplier_2.array()).matrix();
  Eigen::VectorXd output = score.colwise().sum();
  // Eigen::VectorXd output;
  return output;
}
