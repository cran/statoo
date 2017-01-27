#' @title
#' Most frequent value(s)
#'
#' @description
#' The function \code{mfv} returns the most frequent value(s) (or mode(s))
#' found in a vector.
#' The function \code{mfv1} returns the first of these values, so that \code{mfv1(x)} 
#' is identical to \code{mfv(x)[[1L]]}. 
#' 
#' @details 
#' Argument \code{x} is to come from a discrete distribution. 
#' \code{mfv} calls the function \code{\link{tabulate}}. 
#' 
#' @references 
#' \itemize{ 
#'   \item Dutta S. and Goswami A. (2010). 
#'   Mode estimation for discrete distributions. 
#'   \emph{Mathematical Methods of Statistics}, \bold{19}(4):374--384.
#' }
#' 
#' @param x
#' Vector of observations (of type numeric, character, factor, or logical).
#'
#' @param ...
#' Additional parameters that are currently ignored.
#'
#' @return
#' The function \code{mfv} returns a vector of the same type as \code{x}.
#' One should be aware that this vector can be of length \code{> 1}, in case of multiple modes.
#' \code{mfv1} is safer in the sense that it always 
#' returns a vector of length \code{1} (the first of the modes found). 
#'
#' @export
#'
#' @examples
#' # Basic examples
#' mfv(c(3, 4, 3, 2, 2, 1, 2))      # 2
#' mfv(c(TRUE, FALSE, TRUE))        # TRUE
#' mfv(c("a", "a", "b", "a", "d"))  # "a"
#'
#' mfv(c("a", "a", "b", "b", "d"))  # c("a", "b")
#' mfv1(c("a", "a", "b", "b", "d")) # "a"
#'
mfv <-
function (x,
          ...)
{
  f  <- factor(x)
  tf <- tabulate(f)
  lf <- levels(f)[tf == max(tf)]
  as.vector(lf, mode = class(x)[[1L]])
}


#' @export
#' @rdname mfv
#'
mfv1 <-
function(x, 
         ...)
{
  mfv(x, ...)[[1L]]
}
