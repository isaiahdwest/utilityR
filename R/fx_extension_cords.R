# TODO - add this doc to split documentation

#' @title split
#' @param nm names to assign the output of \code{split}
#' @examples \dontrun{
#' split2(mtcars, 1:nrow(mtcars), nm = rownames(mtcars))
#' }
#' @export
split2 <- function(x, f, drop = FALSE, nm = NULL, ...) {
  out <- split(x, f, drop = FALSE, ...)
  if (!is.null(nm)) {
    names(out) <- nm
  }
  out
}
