#' Parse command line args
#'
#' Converts command line arguments into a named list.
#'
#' Attempts to convert the sometimes clunky results from
#' \code{\link{commandArgs}()} to a more useful named \code{list} containing
#' command line parameters.
#'
#' If passed to \code{commandArgsList()}, \code{args} is expected to mimic the
#' results of a call to \code{\link{commandArgs}()}; e.g., a character vector of
#' arguments, by convention, in the form \code{"--<arg-name>"} or
#' \code{"--<arg-name>=<value>"}. However, \code{commandArgsList()} attempts to
#' parse the names and values of any arguments in args, regardless of format, so
#' long as the name and value are separated by "=".
#'
#' Command line flags (arguments without values) are returned in the resulting
#' list with a value of TRUE.  Values assigned to arguments are returned within
#' the list as strings.
#'
#' @param args Typlically NULL (the default), in which case \code{args} will be
#'   derived from a call to \code{\link{commandArgs}()}.  Otherwise, a character
#'   string of arguments that simulates the result of a call to
#'   \code{\link{commandArgs}}; useful for debugging in interactive mode.
#' @returns A named list representing the parsed results of args or, if args is
#'   NULL, a named list representing a call to \code{\link{commandArgs}()}.
#' @export
commandArgsList <- function(args = NULL) {
  if(is.null(args)) {
    args <- commandArgs(TRUE)
    # add a name to the first argument, which is the executable.
    args[1] <- paste0("--executable=", args[[1]])
  } else {
    if(!is.character(args)) stop("Parameter `args` must be of type character.")
    # this is a more robust way of getting as.character(args) -- if any element
    # of args can not be converted to a character, NA is returned with no
    # warning or error.
    args <- sapply(args, \(x) tryCatch(as.character(x), error = \(e) NA))
    args <- as.character(na.omit(args))
  }

  if(length(args) == 0) return(list())

  args <- trimws(args)

  # replace the first '=' with new line character
  args <- sub("=", "\n", args)
  # split on the '\n', trim white space and remove leading "--"
  split_args <-
    strsplit(args, "\n") |>
    lapply(trimws) |>
    lapply(\(x) gsub("^-{0,2}", "", x))

  # make a list with first elements of split as names and second elements
  # as members
  args_list <-
    structure(
      lapply(split_args, "[", 2),
      names = sapply(split_args, "[", 1)
    )

  # args_list <- lapply(
  #   args_list,
  #   \(x) tryCatch(jsonlite::fromJSON(x), error = \(e) x)
  # )

  args_list
}

# @param name_replace a data.frame with two columns (\code{pattern} and
#   \code{replacement}). \code{\link{gsub}} will be called on all names in the
#   return list using the values in each row of \code{name_replace}.  By
#   default, underscores will replace hyphens in the names of the returned list
#   or sublists.

replace_names <- function(x, name_replace) {

# make any name modifications included in name_replace
  if(nrow(name_replace) > 0) {
    for(i in 1:nrow(name_replace)) {
      x <-
        recursive_rename(
          x,
          gsub,
          pattern = name_replace$pattern[i],
          replacement = name_replace$replacement[i])
    }
  }
  x
}



# renames all list elements of x, recursively, using 'fun'.  'fun' is a function
# that accepts the names of a list and returns modified names as a character
# vector.  Called with signature "fun(names(x), ...)"
recursive_rename <- function(x, fun, ...) {
  if(class(x) != "list") stop("'Recursive rename' only operates on lists (without s3 classes).")
  x <-
    lapply(
    x,
    \(.x) {
      if(class(.x) != "list") return(.x)
      recursive_rename(.x, fun, ...)
    }
  )
  names(x) <- fun(names(x), ...)
  x
}
