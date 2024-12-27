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
#' @param pattern,replace See \code{\link{gsub}()}. Used to modify command line
#'   argument names because names on the command line sometimes don't make very
#'   good list names.  If either \code{pattern} or \code{replace} is NULL, list
#'   names will be unmodified.
#' @returns A named list representing the parsed results of args or, if args is
#'   NULL, a named list representing a call to \code{\link{commandArgs}()}.
#' @export
commandArgsList <- function(args = NULL, pattern = NULL, replacement = NULL) {
  if(is.null(args)) {
    args <- commandArgs(TRUE)
  } else {
    if(!is.character(args)) stop("Parameter `args` must be of type character.")
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
    ) |>
    # convert any NA to TRUE
    lapply(
      \(x) {
        if(is.na(x)) return(TRUE)
        x
      })

  # if there is a `json` member, convert and add to non-json args
  args_list <- convert_json_args(args_list)

  # set pattern and replacement defaults
  if("pattern" %in% names(args_list)) {
    pattern <- args_list$pattern
  } else {
    pattern <- "-"}
  if("replacement" %in% names(args_list)) {
    replacement <- args_list$replacement
  } else {
    replacement <- "_"}

  # replace pattern with replacement, recursively, in the names of the arg list.
  gsub_names(args_list, pattern = pattern, replacement = replacement) |>
    # Remove all but the first member of any that have duplicate names.
    remove_duplicate_args()
}

# renames all list elements of x, recursively, using gsub()"
gsub_names <- function(x, pattern, replacement) {
  if(is.null(pattern)|is.null(replacement)) return(x)
  if(class(x) != "list") stop("'Recursive rename' only operates on objects with class(x) == 'list'.")
  x <-
    lapply(
    x,
    \(.x) {
      if(class(.x) != "list") return(.x)
      gsub_names(.x, pattern, replacement)
    }
  )
  names(x) <- gsub(pattern, replacement, names(x))
  x
}

convert_json_args <- function(x) {
  is_json_param <- grepl("^json$", names(x), ignore.case = T)
#  is_json_arg <- sapply(x, grepl, pattern = "^[[{].*[]}]") & !is_json_param

  if(any(!is_json_param)) {
    for(i in which(!is_json_param)) {
      x[[i]] <-
        tryCatch(
          jsonlite::fromJSON(x[[i]]),
          error = \(e) x[[i]]
        )
    }
  }

  if(any(is_json_param)) {
    if(sum(is_json_param) > 1) stop("Only one JSON object can be specified")
    x <- c(
      x[-which(is_json_param)],
      jsonlite::fromJSON(x[[which(is_json_param)]]))
  }
  x
}

