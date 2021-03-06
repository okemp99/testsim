#' testsim: A package to simulate multiple choice and matching test questions
#'
#' Functions to simulate customisable multiple choice and matching questions and tests.
#'
#' @section Simple tests:
#' The function \code{mctest} allows simulation of a multiple choice test with all questions
#' having the same number of possible options, and one correct option. \code{mctest_rep}
#' simulates multiple runs of such a test. \code{vectortest} allows the number of
#' options for each question to be customised by inputting the option numbers
#' for each question as a vector, \code{vectortest_rep} allows multiple runs of
#' these tests.
#'
#' @section Individual questions:
#' The functions \code{mc} and \code{matching} allow for the simulation of fully
#' customisable multiple choice and matching questions. Each allows a choice of number
#' of options, number of correct answers (for \code{mc}), the score added for each correct
#' choice or match, the score subtracted for each incorrect  score or match, and whether or
#' not a negative overall score is allowed. The functions \code{mc_all} and \code{matching_all}
#' allow simulation of questions where it is required to get the question fully correct to
#' score the one mark.
#'
#' @section Complex Test:
#' The \code{complex_test} function allows simulation of a test constructed via a
#' list of questions, with each taking the form used in the \code{mc}, \code{mc_all}, \code{matching} and \code{matching_all}
#' function inputs.
#'
#' @docType package
#' @name testsim
NULL
