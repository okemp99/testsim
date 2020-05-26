#' Create an empty 1 x n data frame
#'
#' @noRd
#' @param n A number
#' @return An empty 1 x \code{n} data frame
#' @examples
#' \dontrun{frame(5)}
#' \dontrun{frame(20)}
#' @seealso \code{\link{frame_multi}} for an m x n data frame
frame <- function(n) {
  scorevector <- data.frame(matrix(nrow = 1, ncol = n))
  return(scorevector)
}



#' Create and empty m x n data frame
#'
#' @noRd
#' @param m A number
#' @param n A number
#' @return An empty \code{m} x \code{n} data frame
#' @examples
#' \dontrun{frame_multi(100, 10)}
#' \dontrun{frame_multi(5, 6)}
frame_multi <- function(m, n) {
  scorevector2 <- data.frame(matrix(nrow = m, ncol = n))
  return(scorevector2)
}
