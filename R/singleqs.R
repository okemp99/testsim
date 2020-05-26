#' Simulate a multiple choice question with
#'  multiple answers, all needed to score
#'
#' @param options The number of possible options the question has
#' @param correct The number of correct answers the question has
#' @returns 1 if all correct answers are selected, 0 otherwise
#' @examples
#' mc_all(5, 3)
#' mc_all(10, 4)
#' @seealso \code{\link{mc}} for more customisable multiple choice questions
#' @export
mc_all <- function(options, correct) {
  mark <- 1
  incorrect <- options - correct
  choices <- sample(rep(c(0, 1), c(incorrect, correct)), correct)
  for (i in 1:correct) {
    if (choices[i] == 0) {
      mark <- 0
    }
  }
  return(mark)
}

#' Simulate a customisable multiple choice question
#'
#' @inheritParams mc_all
#' @param right The score gained for each correct answer selected
#' @param wrong The score lost for each incorrect answer selected. Defaults to 0
#' @param negative If \code{TRUE} a negative overall score for the
#'   question is allowed, if \code{FALSE} the minimum score for
#'    the question is 0. Defaults to \code{FALSE}
#' @returns The overall mark for the question
#' @examples
#' mc(5, 3, 2, 1, TRUE)
#' mc(10, 5, 1/5)
#' mc(10, 5, 1/5, 1/5)
#' @export
mc <- function(options, correct, right, wrong, negative) {
  if (missing(wrong)) {
    wrong <- 0
  }
  if (missing(negative)) {
    negative <- FALSE
  }
  mark <- 0
  incorrect <- options - correct
  choices <- sample(rep(c(0, 1), c(incorrect, correct)), correct)
  for (i in 1:correct) {
    if (choices[i] == 1)
      mark <- mark + right
    else
      mark <- mark - wrong
  }
  if ((negative == FALSE) & (mark < 0)) {
    mark <- 0
  }
  return(mark)
}


#' Simulate a matching question with 1 mark for fully correct, 0 otherwise
#'
#' @param options The number of pairs that need matching
#' @returns 1 if all pairs were correctly matched, 0 otherwise
#' @examples
#' matching_all(6)
#' matching_all(10)
#' @seealso \code{\link{matching}} for more customisable matching questions
#' @export
matching_all <- function(options) {
  grid <- frame_multi(options, 2)
  grid[, 1] <- sample(1:options)
  grid[, 2] <- c(1:options)
  mark <- 1
  for  (i in 1:options)
    if (grid[i, 1] != grid[i, 2])
      mark <- 0
  return(mark)
}


#' Simulate a customisable matching question
#'
#' @inheritParams matching_all
#' @param right The score gained for each correctly matched pair
#' @param wrong The score lost for each incorrectly matched pair. Defaults to 0
#' @param negative If \code{TRUE} a negative overall score for the
#'   question is allowed, if \code{FALSE} the minimum score
#'    for the question is 0. Defaults to \code{FALSE}
#' @returns The overall mark for the question
#' @examples
#' matching(5, 2, 1, negative = TRUE)
#' matching(8, 1/8)
#' @export
matching <- function(options, right, wrong, negative) {
  if (missing(wrong)) {
    wrong <- 0
  }
  if (missing(negative)) {
    negative <- FALSE
  }
  mark <- 0
  grid <- frame_multi(options, 2)
  grid[, 1] <- c(sample(1:options))
  grid[, 2] <- c(1:options)
  for (i in 1:options) {
    if (grid[i, 1] == grid[i, 2])
      mark <- mark + right
    else
      mark <- mark - wrong
  }
  if ((negative == FALSE) & (mark < 0)) {
    mark <- 0
  }
  return(mark)
}
