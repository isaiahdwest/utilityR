
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
#'@title assign multiple results to multiple variables
#'@name Multiple-Assignment
#'@rdname pyop
#'@export
"%=%" <-  function(l, r, ...) {
  UseMethod('`%=%`')
}

#'@rdname pyop
#'@export
#'
`%=%.lbunch` <-  function(l, r, ...) {
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
setClass("lbunch")
setGeneric("%=%")
setMethod("%=%", "lbunch", function(l, r, ...) {
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
})

# Used if LHS is larger than RHS
#'@export
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

#' @title Na if extension
#' @description Extends \code{na_if}, convert data to \code{NA} if behavior is
#' detected. Useful in mutates and long pipes. Equivalent to \code{x[f(x)]}.
#' @param x object that contains values to be switched to NA
#' @param .f a function that returns a boolean, such as \code{stringr::str_detect}
#' @examples \dontrun{
#' mutate(mtcars, mpg2 = na_if_func(mpg, function(x) x > 30))
#' }
#' @export
na_if_func <- function(x, .f, ...) {

  x[.f(x, ...)] <- NA
  x

}

#' @title Data Ingest
#' @description Find the correct row to use as column names without needing
#' to specify \code{read_excel(..., skip = 3)}. Simply read in data and pipe
#' here.
#' @param .data A \code{data.frame} that needs clean names
#' @param sensitivity A numeric indicating how many name repaired column names to allow before picking column headers
#' @param clean_names A boolean, whether to return the \code{data.frame} with cleaned names from \code{janitor} package
#'
#'
#'@export

guess_names <- function(.data, sensitivity = 1, clean_names =TRUE) {

  result <- .data
  valid_names <- sensitivity > sum(stringr::str_detect(names(result), "V[0-9]+|X[0-9]+")) + sum(names(result) == "")

  while (!valid_names) {
    tryCatch({
      suppressWarnings(result <- result %>%
                         janitor::row_to_names(1))
      valid_names <- sensitivity > sum(stringr::str_detect(names(result), "V[0-9]+|X[0-9]+")) + sum(names(result) == "")
      row.names(result) <- 1:nrow(result)
    },
    error = function(e) {
      print(e)
    })
  }

  if (clean_names) {
    result %>% janitor::clean_names()
  } else {
    result
  }
}

#' @title Base Stringr
#' @description Stringr functions rewritten to use only base functions
#' @param text Text string to have matched
#' @param pattern Text string to match in text
#' @details \code{grext} is the equivalent of \code{stringr::str_extract}
#' @export
grext <- function(text, pattern) {
  regmatches(text, regexpr(pattern, text))
}


#' @title Base Stringr
#' @description Stringr functions rewritten to use only base functions
#' @param text Text string to have matched
#' @param pattern Text string to match in text
#' @param simplify Whether to return an unlisted character vector, default
#' is \code{FALSE} and returns list
#' @details \code{grextall} is the equivalent of \code{stringr::str_extract_all}
#' @export
grextall <- function(text, pattern, simplify = FALSE) {
  out <- regmatches(text, gregexpr(pattern, text))
  if (simplify) {
    out[lengths(out) == 0] <- NA_character_
    unlist(out)
  } else {
    out
  }
}


#' @title Package-Dependencies
#' @rdname pkg-dep
#' @description Ever run into cases where package installation fails repeatedly,
#' with a string of error messages for one package requiring different versions
#' of different packages? Get a list of all dependencies with \code{package.dependencies}
#' and install them all in one go with \code{install.dependencies}
#' @param package Name of the package to look for dependencies for, as a character string
#' @param level Character vector of the levels of dependencies to install, options are
#' \code{c("Depends", "Imports", "Suggests")}.
#' @param record Whether to recrd the newly installed package in your \code{renv.lock} file
#' @export
package.dependencies <- function(package, level = c("Depends", "Imports", "Suggests")) {
  pkgs <- as.data.frame(available.packages(), stringsAsFactors = FALSE)
  if (nrow(pkgs[pkgs$Package == package,]) == 0) {stop("package is not available, check your repos")}
  pkgs[package,level]
}

#' @rdname pkg-dep
install.dependencies <- function(package, level = c("Depends", "Imports", "Suggests"), record = FALSE) {
  deps <- as.character(package.dependencies(package = package, level = install))
  deps_lst <- unlist(strsplit(gsub("\n", " ", deps), ", "))
  deps_lst2 <- gsub( " \\(>= ","@", deps_lst[!grepl("R (.+)",deps_lst)])
  deps_lst3 <- gsub("\\)$", "", deps_lst2)

  # Check if already installed
  deps_bar <- gsub("@[0-9\\.]+$", "", deps_lst3)
  vers <- lapply(deps_bar, function(x) tryCatch({packageVersion(x)}, error = function(e) {NA}))
  names(vers) <- deps_bar

  names(deps_lst3) <- deps_bar

  deps_ver <- grextall(text = deps_lst3, "\\d\\.\\d\\.\\d", simplify = T)
  names(deps_ver) <- deps_bar
  to_update <- names(which(deps_ver[names(deps_ver)[!is.na(deps_ver)]] >= vers[names(deps_ver)[!is.na(deps_ver)]]))

  to_install <- append(deps_lst3[names(deps_ver[is.na(deps_ver)])], deps_lst3[to_update])

  renv::install(to_install)
  if (record) {renv::record(deps_lst3)}
}

#' @title Save Time Stamped File
#' @description Save a file and a timestamped copy of that file in a subfolder
#' @param obj Object to save out
#' @param dir Directory to save to
#' @param name File name
#' @param ext File extension to use
#' @param cache Boolean of whether to create a dedicated time stamp folder to
#' save timestampped files to
#' @param ... Additional parameters to pass along
save_ts_file <- function(
    obj,
    dir,
    name,
    ext = ".csv",
    cache = TRUE,
    ...) {

  if (cache && !dir.exists(file.path(dir, paste0(name, "_timestampped")))) {
    dir.create(file.path(dir, paste0(name, "_timestampped")))
  }

  base_fp0 <- file.path(dir, name)
  time <- format(Sys.time(), "%Y-%m-%d_%H_%M_%S")
  base_fp <- paste0(base_fp0, ext)
  ts_fp <- if (cache) {
    paste0(base_fp0, "_timestampped/", name, "_", time, ext)
  } else {
    paste0(base_fp0, "_", time, ext)
  }

  if (ext == ".csv") {
    write.csv(obj, base_fp, ...)
    write.csv(obj, ts_fp, ...)
  } else if (ext == ".xlsx") {
    openxlsx::write.xlsx(obj, file = base_fp, ...)
    openxlsx::write.xlsx(obj, file = ts_fp, ...)
  } else if (ext == ".png") {
    ggplot2::ggsave(plot = obj, filename = base_fp, ...)
    ggplot2::ggsave(plot = obj, filename = ts_fp, ...)
  } else if (ext == ".rds") {
    saveRDS(obj, file = base_fp, ...)
    saveRDS(obj, file = ts_fp, ...)
  }

}

