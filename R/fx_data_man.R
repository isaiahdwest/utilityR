
jc_check <- function(args) {
  if (length(unique(args[["suffix"]])) != 2) {
    stop("suffix entered was not a character vector with 2 unique elements")
  }
  if (!args$join[length(args$join)] %in% lsf.str("package:dplyr")) {
    stop("joinCoalesce currently only supports join functions exported by dplyr (e.g. dplyr::left_join, dplyr::full_join, etc.)")
  }
  if (!endsWith(args$join[length(args$join)], "join")) {
    stop("function passed to join is not a recognzed joining function from dplyr.")
  }
}

#' @title Joining Data
#' @description Join together two data sets and coalesce together columns with
#' the same name that aren't used as joining columns.
#' @param x a \code{data.frame} object
#' @param y a \code{data.frame} object
#' @param join a \code{dplyr} join function to apply to \code{x} and \code{y}.
#' @param suffix suffixes to apply to column names found in both \code{x} and \code{y}. Columns with these suffixes will be coalesced together.
#' @param ignore character vector of column names that exist in both \code{x} and \code{y} that shouldn't be coalesced.
#' @examples \dontrun{
#' one <- tribble(
#'       ~x,~y,~z,
#'       "a", 1,1,
#'       "b", 9,1,
#'       "c", 4,1
#'       )
#'
#'two <- tribble(
#'      ~x, ~y,~z,
#'      "a", NA,2,
#'      "b",2,2,
#'      "d", 9,2,
#'      "c", NA,2
#'       )
#'joinCoalesce(one, two, dplyr::left_join, by = "x", ignore = "z")
#' }
#' @export

joinCoalesce <- function(x, y, join, by = NULL, suffix = c(".x", ".y"), ignore = NULL) {

  checks <- mget(ls())
  checks$join <- as.character(substitute(join))
  jc_check(checks)

  joinedData <- join(x, y, by = by, suffix = suffix)
  cols <- union(names(x), names(y))

  if (!is.null(ignore)) {

    ignore <- c(paste0(ignore, suffix[[1]]), paste0(ignore, suffix[[2]]))
  }

  toCoal <- names(joinedData)[!(names(joinedData) %in% c(cols, ignore))]

  if (length(toCoal) == 0) {
    return(joinedData)
  }

  oldName <- purrr::map_chr(toCoal,
                     ~substr(.x,
                             start = 1,
                             stop = nchar(.x) - nchar(suffix[endsWith(.x, suffix)]))) %>%
    unique()

  outCols <- purrr::map(oldName, ~ dplyr::coalesce(
    joinedData[[paste0(.x, suffix[[1]])]],
    joinedData[[paste0(.x, suffix[[2]])]]
  )) %>%
    purrr::set_names(oldName) %>%
    dplyr::bind_cols()

  joinedData %>%
    dplyr::select(-dplyr::any_of(toCoal)) %>%
    dplyr::bind_cols(outCols)
}

#'@title Pipe Alt. - Mapping Functions
#'@description Applies a list of functions to an object in sequential order. Usefule in cases where you don't want to use a lot of \code{%>%} operators. Usefule in conjunction with \code{%=%} and \code{g}.
#'@param .x An object to apply functions to
#'@param .fns A \code{list} of functions to apply in sequence to \code{.x}. E.g. list(toupper, function(x) {str_remove(x, " ")})
#'@param error_handling How errors in code should be handled. Options are \code{"do-nothing"} and \code{"stop"}. \code{"do-nothing"} skips the function in the sequence that produced the error and continues through the list. \code{"stop"} results in the code stopping with standard error messaging.
#'@examples \dontrun{
#'map_fns("THe Starting price is $25,000 to $35,000", list(function(x) {str_extract_all(x, "\\$[0-9,]+")}, unlist, function(x){str_remove_all(x, "[^0-9]")}, as.numeric, function(x){mean(x, na.rm = TRUE)}))
#'
#'# Using with %=% and g
#'
#'# cleaning steps
#'
#'cln_lst <- list(toupper, str_squish, function(x) {str_remove_all(x, "[^A-Z]")})
#'make <- "Chevro0lT"
#'model <- "odYssey5"
#'g(make, model) %=% purrr::map(c(make, model), ~ map_fns(.x, cln_lst))
#'}
#'@export
map_fns <- function(.x, .fns, error_handling = "do-nothing") {

  stopifnot(error_handling %in% c("do-nothing", "stop"))

  result <- .x
  for (i in 1:length(.fns)) {
    tryCatch({
      result <- .fns[[i]](result)
    },
    error = function(e) {
      if (error_handling == "do-nothing") {
        result <- result
      } else if (error_handling == "stop") {
        stop(e)
      }
    })
  }

  result
}

# Generic form
#'@export
'%=%' = function(l, r, ...) UseMethod('%=%')

# Binary Operator
#'@export
'%=%.lbunch' = function(l, r, ...) {
  Envir = as.environment(-1)

  if (length(r) > length(l))
    warning("RHS has more args than LHS. Only first", length(l), "used.")

  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extendToMatch(r, l)
  }

  for (II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
  }
}
# Used if LHS is larger than RHS
extendToMatch <- function(source, destin) {
  s <- length(source)
  d <- length(destin)

  # Assume that destin is a length when it is a single number and source is not
  if(d==1 && s>1 && !is.null(as.numeric(destin)))
    d <- destin

  dif <- d - s
  if (dif > 0) {
    source <- rep(source, ceiling(d/s))[1:d]
  }
  return (source)
}

# Grouping the left hand side
#'@export
g = function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  return(List)
}

