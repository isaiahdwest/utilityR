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

#' @title remove objects from environment
#' @description Remove all objects from environment EXCEPT
#' those that are specified.
#' @param ... what objects to retain in environment
#' @param envir Which environment to remove objects from, default is the global environment
#' @examples \dontrun {
#' a <- "TEST"
#' b <- "TEST2"
#'
#' rm_ex(a)
#' }
#' @export
rm_ex <- function(..., envir = globalenv()) {

  objs <- match.call(expand.dots = FALSE)$...

  enVars <- ls(envir = envir)

  rm(list = setdiff(enVars, unlist(objs)), envir = envir)
}
