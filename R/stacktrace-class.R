#' Add a stack trace to an error or warning.
#'
#' Creates a \code{stacktrace} object, typically from a \code{warning} or
#' \code{error} or other \code{\link{condition}} object by appending the current
#' stack trace or a subsection thereof as a member called \code{$stacktrace}.
#'
#' The class \code{stacktrace} can be built atop and S3 class of
#' \code{\link{mode} list}, but is anticipated to inherit from the
#' \code{\link{condition}} class. \code{stacktrace} adds an additional member
#' (itself a list) called \code{$stacktrace} to the underlying list of the
#' \code{condition} argument.  The \code{$stacktrace} member contains results
#' of \code{\link{sys.calls}()}, as executed from within \code{as.stacktrace()}
#' and converted to \code{character}. \code{start_key} and \code{end_key}
#' parameters can be used to extract a relevant section from the middle of a
#' full stack trace.
#' @param condition Any \code{list} or S3 object of \code{\link{mode} list} to
#'   which a stack trace should be appended; anticipated to be a \code{warning}
#'   or \code{error} object (or anything else that inherits from the
#'   \code{\link{condition}} class).
#' @param start_key,end_key a regex string that matches the text of a call in
#'   the stack trace and marking the start (\code{start_key}) and end
#'   (\code{end_key}) of a relevant portion of the stack call to be extracted.
#'   The calls between the last call matching \code{start_key} and the first
#'   call matching \code{end_key} (non_inclusive) will be included in
#'   \code{$stacktrace}.  If \code{start_key} is NULL or doesn't match, calls
#'   starting with the first call in the stack trace will be included  If
#'   \code{end_key} is NULL or doesn't match, calls through the last call will
#'   be included.  Also, the entire stack trace will be included if the last
#'   call matching \code{start_key} occurs after the first call matching
#'   \code{end_key}.
#' @param x A \code{stacktrace} object
#' @param ... Additional parameters passed from the generic
#'   \code{\link{print}()} function that calls \code{print.stacktrace()} (see
#'   \code{\link{genericFunction-class}})
#' @returns An object of class \code{stacktrace} that inherits from the class of
#'   x.  If x also inherits from \code{error} or \code{warning}, result will
#'   have additional class of \code{stacktraceError} or \code{stacktraceWarning},
#'   respectively. Result will also have an additional member in the underlying
#'   list, called \code{$stacktrace} containing the stack trace (as a
#'   \code{character}) and as excerpted by \code{start_key} and \code{end_key}.
#' @export
as.stacktrace <- function(condition, start_key = NULL, end_key = NULL) {
  orig_class <- class(condition)
  class(condition) <- c("stacktrace", class(condition))
  if(inherits(condition, "error")) {
    class(condition) <- c("stacktraceError", class(condition))
  } else if(inherits(condition, "warning")) {
    class(condition) <- c("stacktraceWarning", class(condition))
  }

  condition$stacktrace <-
    sys.calls() |>
    lapply(\(.x) capture.output(print(.x))) |>
    extract_calls(start_key, end_key) |>
    # get rid of comment lines
    lapply(\(.x) .x[!grepl("^[[:space:]]*#", .x)])
  condition
}

#' @rdname as.stacktrace
#' @export
print.stacktrace <- function(x, ...) {
  NextMethod(generic = NULL, object = NULL, ...)
  cat("  Stack trace:\n")
  for(i in 1:length(x$stacktrace)) {
    cat("  [", i, "] ",
        paste(x$stacktrace[[i]], collapse = "\n      "),
        "\n",
        sep = "")
  }
}

#' @rdname as.stacktrace
#' @export
as.character.stacktrace <- function(x) {
  capture.output(print(x)) #|>
    # strsplit("\n") |>
    # unlist() |>
    # unname()
}

# extracts a subset of a list of vectors between the last vector matching
# start_key and the first vector matching end_key, excluding the matches.
extract_calls <- function(x, start_key, end_key) {
  s <- find_key(x, start_key, F) + 1
  e <- find_key(x, end_key, T) - 1
  # check to see if request start is after requested end
  if(s > e) return(x)
  x[s:e]
}

# finds the first or last index of list of vectors where an element
# of the vector matches the regex in key.  If first = T, returns the
# index of the first element.  If first = F, returns the index of the
# last element.  If not match, returns 0 or length(x) + 1, depending on
# value of first.
find_key <- function(x, key, first) {
  if(is.null(key)) return(ifelse(first, length(x)+1, 0))
  matched <- sapply(x, \(.x) any(grepl(key, .x)))
  if(!any(matched)) return(ifelse(first, length(x)+1, 0))
  which(matched)[ifelse(first, 1, sum(matched))]
}
