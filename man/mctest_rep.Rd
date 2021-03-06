% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basicmc.R
\name{mctest_rep}
\alias{mctest_rep}
\title{Perform multiple simulations of a simple multiple choice test}
\usage{
mctest_rep(questions, options, n)
}
\arguments{
\item{questions}{The number of questions in the quiz}

\item{options}{The number of options each question has}

\item{n}{The number of repetitions of the test}
}
\value{
A data frame showing whether each question was
  answered correctly, and the total score achieved
  for the test (out of \code{questions})
  for each test, along with the mean score across all tests
}
\description{
Perform multiple simulations of a simple multiple choice test
}
\examples{
mctest_rep(10,4,1000)
mctest_rep(5, 5, 32)
}
\seealso{
\code{\link{vectortest_rep}} to customise
  the number of options for each question
}
