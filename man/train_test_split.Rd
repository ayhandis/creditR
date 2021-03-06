% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train_test_split.R
\name{train_test_split}
\alias{train_test_split}
\title{Train Test Split}
\usage{
train_test_split(data, seed_value = 1, ratio)
}
\arguments{
\item{data}{The data set which is intended to be seperated into train and test should be specified.}

\item{seed_value}{In order to ensure repeatability, random number must be assigned in a particular order. The seed value must be specified in order to perform the repeatability process.}

\item{ratio}{The percentage of train data should be specified.}
}
\description{
This function separates the given data set as train and test dataaccording to the determined seed value and ratio. .
}
\examples{
random_column <- data.frame(runif(100, min = 0, max = 1000))
colnames(random_column) = "random_column"
datasets <- train_test_split(random_column, seed_value = 1, ratio = 0.70)
train = datasets$train
test = datasets$test
}
\keyword{creditR}
