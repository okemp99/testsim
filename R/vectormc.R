#' Simulate a basic multiple choice test with varied option numbers
#'
#' @param quiz A vector containing the number of options for each question
#' @returns A data frame showing whether each question was
#'   answered correctly, and the total score achieved for the test
#' @examples
#' vectortest(c(4,5,6,4,3))
#' vectortest(rep(4, 10))
#' @seealso \code{\link{vectortest_rep}} for multiple runs of such tests
#' @export
vectortest <- function(quiz) {
  questions <- length(quiz)
  scores <- frame(questions)
  for (i in 1:questions) {
    if (pracma::rand() <= 1 / quiz[i])
      scores[i] <- 1
    else
      scores[i] <- 0
  }
  scores$Total <- sum(scores)
  colnames(scores) <- c(paste0("Q", 1:questions), "Total")
  rownames(scores) <- "Score"
  return(scores)
}

#' Perform multiple simulations of a multiple
#'  choice test with varied option numbers
#'
#' @inheritParams vectortest
#' @param n The number of repetitions of the test
#' @returns A data frame showing whether each question was
#'   answered correctly, and the total score achieved for each test,
#'   along with the mean score across all tests
#' @examples
#' vectortest_rep(c(4,5,6,4,3), 1000)
#' vectortest_rep(rep(4, 10), 30)
#' @export
vectortest_rep <- function(quiz, n) {
  questions <- length(quiz)
  scores <- frame_multi(n, questions + 1)
  for (i in 1:n)
    scores[i, ] <- vectortest(quiz)
  scores[n + 1, ] <- colMeans(scores)
  scores[n + 2, ] <- scores[n + 1, ]
  scores[n + 1, ] <- "-"
  colnames(scores) <- c(paste0("Q", 1:questions), "Total")
  rownames(scores) <- c(paste0("Run ", 1:n), "-", "Means")
  return(scores)
}
