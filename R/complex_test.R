#' Simulate a complex customisable test
#'
#' Simulates runs of a test with each question in a list. Each question has its
#'  corresponding parameters from \code{\link{mc}}, \code{\link{mc_all}},
#'   \code{\link{matching}} and \code{\link{matching_all}}
#'  in the list entry and takes one of these four forms. An example test is:
#'  \code{test <- list(q1, q2)} where \code{q1 <- c(type = "mc", options = 5,
#'   correct = 3, right = 3, wrong = 1, negative = TRUE)} and
#'  \code{q2 <- c(type = "matching", options = 6,
#'   right = 2, wrong = 1, negative = FALSE)}
#'
#' @param test A list of each question of the test.
#' @param n The number of repetitions of the test
#' @param negative If \code{TRUE} a negative overall score for the
#'   test is allowed, if \code{FALSE} the minimum total score for
#'    the test is 0. (However individual questions
#'     can still have negative scores).
#'    Defaults to \code{FALSE}
#' @returns A data frame showing the score achieved for each question
#'  and the test overall on each run, and corresponding means
#'  across all runs
#' @examples
#' complex_test(list(c(type = "mc", options = 5, correct = 3, right = 1,
#'  wrong = 5, negative = TRUE), c(type = "matching", options = 6, right = 4,
#'   wrong = 1, negative = FALSE)), 1000, FALSE)
#' complex_test(list(c(type = "mc", options = 5, correct = 3, right = 1,
#'  wrong = 5, negative = TRUE), c(type = "matching", options = 6, right = 4,
#'   wrong = 1, negative = FALSE)), 1000, TRUE)
#' @export
complex_test <- function(test, n, negative) {
  if (missing(negative)) {
    negative <- FALSE
  }
  questions <- length(test)
  results <- frame_multi(n, questions)
  for (j in 1:n) {
    for (i in 1:questions) {
      if (test[[i]][1] == "mc")
        results[j, i] <- mc(as.numeric(test[[i]][2]),
                            as.numeric(test[[i]][3]),
                            as.numeric(test[[i]][4]),
                            as.numeric(test[[i]][5]), test[[i]][6])
      else if (test[[i]][1] == "mc_all")
        results[j, i] <- mc_all(as.numeric(test[[i]][2]),
                                as.numeric(test[[i]][3]))
      else if ((test[[i]][1] == "matching"))
        results[j, i] <- matching(as.numeric(test[[i]][2]),
                                  as.numeric(test[[i]][3]),
                                  as.numeric(test[[i]][4]), test[[i]][5])
      else
        results[j, i] <- matching_all(as.numeric(test[[i]][2]))
    }

  }
  results[, questions + 1] <- rowSums(results)
  for (i in 1:n) {
    if ((negative == FALSE) & (results[i, questions + 1] < 0))
      results[i, questions + 1] <- 0
  }
  results[n + 1, ] <- colMeans(results)
  results[n + 2, ] <- results[n + 1, ]
  results[n + 1, ] <- "-"
  colnames(results) <- c(paste0("Q", 1:questions), "Total")
  rownames(results) <- c(paste0("Run ", 1:n), "-", "Means")
  return(results)
}
