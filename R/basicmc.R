
#' Simulate a basic multiple choice test
#'
#' @param questions The number of questions in the quiz
#' @param options The number of options each question has
#' @returns A data frame showing whether each question was
#'   answered correctly, and the total score achieved
#'   for the test (out of \code{questions})
#' @examples
#' mctest(10, 4)
#' mctest(50, 3)
#' @seealso \code{\link{mctest_rep}} for multiple runs of a test
#' @seealso \code{\link{vectortest}} to customise the
#'  number of options for each question
#' @export
mctest <- function(questions, options) {
  success <- 1 / options
  scores <- frame(questions)
  for (i in 1:questions) {
    if (pracma::rand() <= success)
      scores[i] <- 1
    else
      scores[i] <- 0
  }
  scores$Total <- sum(scores)
  colnames(scores) <- c(paste0("Q", 1:questions), "Total")
  rownames(scores) <- "Score"
  return(scores)
}

#' Perform multiple simulations of a simple multiple choice test
#'
#' @inheritParams mctest
#' @param n The number of repetitions of the test
#' @returns A data frame showing whether each question was
#'   answered correctly, and the total score achieved
#'   for the test (out of \code{questions})
#'   for each test, along with the mean score across all tests
#' @examples
#' mctest_rep(10,4,1000)
#' mctest_rep(5, 5, 32)
#' @seealso \code{\link{vectortest_rep}} to customise
#'   the number of options for each question
#' @export
mctest_rep <- function(questions, options, n) {
  scores <- frame_multi(n, questions + 1)
  for (i in 1:n)
    scores[i, ] <- mctest(questions, options)
  scores[n + 1, ] <- colMeans(scores)
  scores[n + 2, ] <- scores[n + 1, ]
  scores[n + 1, ] <- "-"
  colnames(scores) <- c(paste0("Q", 1:questions), "Total")
  rownames(scores) <- c(paste0("Run ", 1:n), "-", "Means")
  return(scores)
}
