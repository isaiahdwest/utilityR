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

#' @title Pivot Longer - Easy Parallel Pivot
#' @description Easiliy pivot multiple \code{data.frame} columns into longer format in
#' parallel
#' @param .data A \code{data.frame} with columns to pivot into longer format
#' @param cols A character vector of columns to pivot into longer format, these will be used to
#' group the parallel columns
#' @param parallel A list of column groups to pivot into longer format in parallel with \code{cols}.
#' Each group should be the same length as \code{cols}. When pivoted, the
#' first column in \code{cols} matches to the first column in each group in \code{parallel}. Default is
#' \code{NULL} and results in behavior equivalent to \code{tidyr::pivot_longer}.
#' @param val_names Names to give the value columns of the resulting dataframe
#' @param id_names Names to give the id columns of the resulting dataframe
#' @examples \dontrun{
#' parallel_longer(mtcars, c("mpg", "cyl"), list(c("cyl", "hp")), c("VALONE", "VALTWO"), c("COL1", "COL2"))
#' }
#' @export
parallel_longer <- function(.data, cols, parallel = NULL, val_names = NULL, id_names = NULL) {

  if (!all(row.names(.data) == 1:nrow(.data))) {
    warning("Preserving original `row.names` as new `row.names` will need to be unique")
    .data$rowNames <- row.names(.data)
  }

  # Check for unique names
  if (any(grepl("name[0-9]+|value[0-9]+", names(.data)))) {
    warning("Duplicate colummns found, creating unique")
    names(.data)[grepl("name[0-9]+|value[0-9]+", names(.data))] <- paste0("..", names(.data)[grepl("name[0-9]+|value[0-9]+", names(.data))])
  }

  nms <- rep(cols, nrow(.data))
  .data[["value1"]] <- .data[[cols[[1]]]]
  num <- length(cols)
  rnms <- rep(row.names(.data), num)
  long.data <- do.call(rbind.data.frame, rep(split(.data, 1:nrow(.data)),num))
  long.data[["rnms"]] <- rnms
  long.data <- long.data[order(rnms),]
  long.data <- do.call(
    rbind.data.frame,
    lapply(split(long.data, long.data$rnms), function(x) {
      x[["value1"]] <- as.vector(t(x[1,cols]))
      x
    })
  )
  long.data$name1 <- factor(nms, levels = cols)

  if (!is.null(parallel)) {

    if (!unique(lengths(parallel)) == length(cols)) {
      stop("Each group of columns to pivot parallel to `cols` must have the same length as `cols`")
    }

    prl.nmcols <- data.frame(lapply(parallel, function(x) rep(x, nrow(.data))))
    names(prl.nmcols) <- paste0("name", 2:(length(parallel)+1))
    long.data[,names(prl.nmcols)] <- prl.nmcols
    prl.nms <- sapply(1:length(parallel), function(x) paste0("value", x + 1))
    long.data[,c(prl.nms)] <- long.data[,sapply(parallel, function(x) x[[1]])]
    long.data <- do.call(
      rbind.data.frame,
      lapply(split(long.data, long.data$name1), function(x) {
        ind <- match(unique(x$name1), cols)
        y <- sapply(parallel, function(z) z[[ind]])
        x[,prl.nms] <- x[, y]
        x
      })
    )
    long.data <- long.data[order(rnms, long.data$name1),]
  }
  long.data$name1 <- as.character(long.data$name1)

  vec <- names(long.data)[grepl("^name[0-9]+|^value[0-9]+", names(long.data))]
  order_nms <- paste0(c("name", "value"), sort(rep(1:(length(vec) / 2),2)))
  long.data <- long.data[names(long.data)[!names(long.data) %in% c(cols, "rnms", unlist(parallel))]]
  long.data <- long.data[c(names(long.data)[!grepl("^name[0-9]+|^value[0-9]+", names(long.data))], order_nms)]

  if (!is.null(val_names)) {
    if (length(val_names) != (1 + length(parallel))) {
      stop("val_names should have the same length as all column groups, i.e. `1 + length(parallel)")
    }
    names(long.data)[grepl("^value[0-9]+", names(long.data))] <- val_names
  }

  if (!is.null(id_names)) {
    if (length(id_names) != (1 + length(parallel))) {
      stop("id_names should have the same length as all column groups, i.e. `1 + length(parallel)")
    }
    names(long.data)[grepl("^name[0-9]+", names(long.data))] <- id_names
  }
  row.names(long.data) <- NULL
  long.data
}
