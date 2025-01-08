#' Evaluate an expression with formal capturing and logging or webhook reporting
#' of the result, including warnings and errors with a stack trace.
#'
#' \code{tryCapture()} allows any R expression to be run from the command line
#' in a production environment so that result can be logged or reported as part
#' of an automated workflow.
#'
#' \code{tryCapture()} is based on \code{\link{tryCatch}()} except that error
#' handlers are predefined to report results, including errors and warnings, to
#' one of several destination types.  \code{tryCapture()} builds a list (the
#' "\emph{result list}", see Value section below) that contains the command line
#' parameter strings, the interpretation of those parameters, any warnings or
#' errors generated, and the results of \code{expr}.  The \emph{result list} can
#' be:
#' \itemize{
#' \item{saved as an .rdata file;}
#' \item{printed to stdout or stderr; or}
#' \item{converted to JSON and}
#' \itemize{
#'   \item{reported to a webhook or}
#'   \item{logged} to a JSON (text) file; optionally gzipped.}}
#'
#' The following parameters, when included on the command line, will be used by
#' \code{tryCapture()}, and will not available in the execution environment for
#' \code{expr} (more on that environment below).  Instead, they control the
#' behavior of \code{tryCapture()} and are referred to, collectively, as
#' \emph{report parameters}.
#'
#' \itemize{
#' \item{\code{--report-to=<destination>} determines how results are reported.
#' Values of \code{<destination>} are one of:
#' \itemize{
#' \item{the value "stdout" for reporting to stdout (the defualt);}
#' \item{the value "stderr" for reporting to stderr;}
#' \item{the URL of a webhook (staring with "http://" or "https://");}
#' \item{a file name (including path, if desired) for a local file; or}
#' \item{the name of an environment variable associated with any of the above.}
#' }
#' In the case of a file name, the result list will be saved as an `Rdata` file
#' using \code{\link{saveRDS}()} unless the file extension of the provided file
#' name is `.json` or `.json.gz`.  For `.json` and `.json.gz` files, the result
#' list will be converted to JSON and saved as a text file or gzipped text
#' file.}
#' \item{\code{--json=<JSON_string>}. Provides parameters using JSON.  Can
#' include parameters needed by \code{expr} or other parameters used by
#' tryCapture (e.g., \code{report-to}).  Parameters specified using JSON will
#' be appended to the end of the parameter list, following any other parameters
#' not included in the JSON parameters.  <JSON_String> must be a valid JSON
#' object containing key-value pairs.  To provide a parameter "foo" with a
#' value, use \code{"foo":"string_value"} or, for a vector,
#' \code{"foo":[1,2,3]}.  For a parameter switch (parameter without a value),
#' use \code{"foo":true}}.
#' \item{\code{--simulate-error=<type>}. Simulates errors for testing and
#' debugging. <type> is one of "args", "function", "report", or
#' "function/report". "args" will simulate an error in the processing of command
#' line arguments, "expr" an error in the expression passed to \code{expr},
#' "report" an error in reporting results, and "expr/report" an error in both
#' the expression and reporting. "expr" errors will be reported to location
#' specified by \code{--report-to}.  If any other error type is requested,
#' reporting will be to stderr (because reporting to \code{--report-to} is
#' precluded if an error occurs in argument processing or reporting).}
#' \item{\code{--simulate-warning=<type>}. Simulates warnings for testing and
#' debugging.  For <type> definition, see `--simulate-error`.  Unless an error
#' occurs in reporting, all warnings will be reported to the \code{report-to}
#' destination}
#' \item{\code{--overwrite-file}. Affects reporting to files; ignored for other
#' reporting destinations. When \code{--report-to} is a file name ending in
#' `.json` or `.json.gz`, \code{--overwrite_file} causes an existing file to be
#' overwritten rather than appended (the default).  When `report-to` doesn't end
#' in `.json` or `.json.gz`, \code{--overwrite_file} allows an existing file to
#' be overwritten rather than raising an error (the default), because .Rdata
#' files can not be appended.}
#' \item{\code{--pattern, --replacement}. When converting argument names to list
#' names, \code{\link{gsub}()} is performed on argument names using
#' \code{--pattern} and \code{--replacement}.  Defaults to \code{--pattern="-"}
#' and \code{replacement="_"} because hyphens are common in command line
#' argument names but R does not deal gracefully with hyphens in list names.}}
#'
#' Any argument can be associated with a stand-alone parameter on the command
#' line (e.g., \code{--report-to=stdout}) OR wrapped within the \code{--json}
#' value (e.g., \code{--json=\{"report-to":"stdout"\}} Further, the
#' \code{--json=<JSON_string>} is not exclusive.  It can be mixed with other
#' stand-alone parameters on the command line. If the same parameter name is
#' provided as a stand-alone parameter and in the \code{--json=<JSON_string>},
#' the value of the stand-alone parameter will take precedence.
#'
#' Any parameter included on the command line \emph{other than} \emph{report
#' parameters} are termed \emph{expression parameters}.  Each \emph{expression
#' parameters} will be converted to R \code{\link{name}} associated with value
#' of parameter's argument.  Any argument associated with an \emph{expression
#' parameter} that contains valid JSON will be processed as such.
#'
#' Any R \code{\link{name}} arising from an \emph{expression parameter} will be
#' contained in an execution environment where \code{expr} is evaluated.
#' Therefore, any expression parameters will be available for use as a
#' \code{\link{name}} in `expr`. See Examples, below.
#'
#' Because only the expression parameters are included in the execution
#' environment for <expr>, \code{mget(ls())} can be used within \code{`expr`} to
#' create a \code{\link{list}} of all expression parameters.  See last example
#' in Examples, below.
#'
#' To run R code from the command line directly, use
#'
#' \code{Rscript -e "commandR::tryCapture(<expr>)" --<arg>=<value> --<arg>=...}
#'
#' where \code{<expr>} is the code to be executed.  Alternatively, use
#' \code{tryCapture()} in an R script (e.g., a script file named `foo.R`) and
#' call the script using:
#'
#' \code{Rscript foo.R --<arg>=<value> --<arg>=...}
#'
#' In order to be included in the Examples section (below), examples must use
#' direct calls to tryCapture() (using the \code{args} parameter) from within an
#' active R session.  Calls from the command line can not be included in
#' Examples.  However, command-line-equivalents for many examples are list
#' \emph{in the comments} above several of the examples below.
#'
#' A note on parallel processing... The safest approach is to embed
#' \code{mclapply} in a script called from the command prompt using
#' \code{Rscript} (Google "Rscript command line" for more.) Parallel processing
#' using \code{mclapply} to call \code{batch} from within Rstudio often works,
#' but can cause Rstudio to freeze (seeminly when the screen saver engages??).
#' In this case, batch processing usually proceeds successfully even after
#' RStudio is frozen, but RStudio must be terminated manually, potentially
#' leading to loss of code.
#'
#' @returns A numeric vector with a single value: 0 if \code{fun} terminates
#'   successfully, 1 if \code{fun} terminates successfully with warnings, 2 if
#'   \code{fun} throw an error, and 3 if an error occurs in argument processing
#'   or reporting and no report was sent to `--report-to` (in this case a report
#'   will go to stderr).
#'
#'   In addition, as a side effect, \code{batch()} creates a list with the
#'   following format:
#'
#'   \verb{list(
#'     time = <time of report>,
#'     resultType = c("Success", "Warning", "Error"),
#'     messages = list of warning messages,
#'     commandLineArgs = either `args` parameter or result from commandArgs(TRUE),
#'     reportArgs = processed commandLineArgs controlling tryCapture() reporting,
#'     expressionArgs = list of remaining processed commandLineArgs (needed by `expr`),
#'     result = result from <expr>)}
#'
#'   This list, or a JSON representation thereof, will be sent to --report-to
#'   destination, as described in Details.
#'
#' @param expr An expression for evaluation.  Any variables required by the
#'   expression can be passed as command line arguments.  If reporting to a
#'   .json file or webhook is requested, \code{expr} must return an object that
#'   can be converted to JSON using \code{\link[jsonlite]{toJSON}()}
#' @param args Generally set to NULL (the default), in which case \code{batch()}
#'   will call \code{\link{commandArgs}(TRUE)} to retrieve and then process
#'   command line parameters. Alternatively, a character vector that mimics the
#'   results from \code{\link{commandArgs}(TRUE)} can be passed for interactive
#'   use or debugging. If present, values should be in a character vector
#'   containing strings in the form `--<parameter>=<argument>` (or
#'   `--<parameter>` for boolean switch).  If the character vector is named, the
#'   names are ignored.
#' @example /man/examples/tryCapture.R
#' @export
tryCapture <- function(expr, args = NULL) {

  # Within `tryCapture()`, these variables act as globals
  report_arg_names <- c("report_to", "simulate_error", "simulate_warning", "overwrite_file")
  r <-
    list(
      time = NULL,
      resultType = NULL,
      messages = NULL,
      commandLineArgs = args,
      reportArgs = NULL,
      expressionArgs = NULL,
      result = NULL)

  if(!is_something(r$commandLineArgs)) {
    r$commandLineArgs <- commandArgs(TRUE)
  }

  # ###############################
  # Helper functions.  Must be defined in this context because these functions
  # require access to the reporting variables above!
  # ###############################

  store_warning_with_trace <- function(w) {
    w <- as.stacktrace(w, "^(eval\\(expr\\))|((do)?[tT]ryCatch)", "(^.handle)|(^withRestarts)")
    #    w <- as.stacktrace(w)
    r$messages <<- append_message(w)
    invokeRestart("muffleWarning")
  }

  store_error_with_trace <- function(e) {
    e <- as.stacktrace(
      e,
      start_key = "^(eval\\(expr\\))|((do)?[tT]ryCatch)",
      end_key = "(^.handle)|(^withRestarts)|(^h\\()" )
    #    e <- as.stacktrace(e)
    r$messages <<- append_message(e)
    #report()
  }

  print_error_with_trace <- function(e) {
    # don't trim the stacktrace here to avoid potential errors
    # occurring before printing.
    e <- as.stacktrace(e)
    r$messages <<- append_message(e)
    sink(stderr())
    r$resultType <<- "Error **reporting failed**"
    print(r)
    sink(NULL)
    return(3)
  }

  append_message <- function(x) {
    c(r$messages,
      list(
        list(message_id = uuid::UUIDgenerate(),
             message = x)))
  }

  simulate_conditions <- function(location) {
    types <- c("warning", "error")
    # get "simulate_warning" and "simulate_error" arguements if present.
    requests <- r$reportArgs[paste0("simulate_", types)]
    funs <- list(warning, stop)

    for(i in 1:2) {
      if(is_something(requests[i]))
        # if `location` was requested by the user as a simulated condition...
        if(grepl(location, requests[i])) {
          # to simulate error in arg processing, remove
          # any successfully processed args...
          if(types[i] == "error" && location == "args") {
            r["reportArgs"] <<- list(NULL)
            r["expressionArgs"] <<- list(NULL)
          }
          funs[[i]](
            "**Simulated** ", types[i], ". `--simulate-", types[i], "=",
            location, "` was included on command line.")
        }
    }
    TRUE
  }

  check_report_to <- function() {
    if(!is_something(r$reportArgs$report_to))
      stop("Required `--report-to=<destination>` parameter not detected on command line.")
    if(!is.character(r$reportArgs$report_to))
      stop("Required `--report-to=<destination>` parameter must be a character")
    if(length(r$reportArgs$report_to) != 1)
      stop("Required `--report-to=<destination>` must have one and only one character string.")
    if(r$reportArgs$report_to == "")
      stop("Required `--report-to=<destination>` can't be an empty string.")
    TRUE
  }

  # test to see if existing .rdata file can be overwritten.
  check_overwrite_status <- function() {
    # see if the overwrite flag is set
    if(!is_something(r$reportArgs$overwrite_file)) {
      # see if the output is an rdata file.  This is the only case
      # where overwriting is a concern.  For json output, file will
      # be appended.
      is_rdata_output <-
        !grepl(
          "(^stderr$)|(^stdout$)|(^http)|(.json(.gz)?$)",
          r$reportArgs$report_to)
      # If output is rdata and file exists and overwrite flag is not set,
      # abort with error to stderr.
      if(is_rdata_output) {
        if(file.exists(r$reportArgs$report_to)) {
          message <-
            paste0(
              "File `", r$reportArgs$report_to,
              "` exists; `--overwrite-file` must be specified to overwrite. ",
              "NOTE: To preserve file, error reporting redirected to stderr.")
          r$reportArgs$report_to <<- "stderr"
          stop(message)
        }
      }
    }
  }



  ####################################################################
  # REPORT the expression
  ####################################################################

  # Report will report results to `report-to` and return a status that is then
  # returned to the OS. status = c(NormalExit = 0, Warning=2, Error=2,
  # ReportingError=3). ReportingError is broken out from Error because it is
  # more severe that Error.  If there is no ReportingError, we can assume that
  # Errors have been reported successfully to `report-to`. In contrast,
  # ReportingErrors can only be sent to stderr because, well, reporting to
  # `report-to` generated an error!  Thus, a ReportingError means that no report
  # was sent to `report-to` and `report-to` will never get a response, which
  # could have knock-on effect for any pipeline that is listening to
  # `report-to`.
  report <- function(expr) {
    r$time <<- paste(as.POSIXct(Sys.time(), tz="UTC"), "UTC")

    simulate_conditions("report")
    # result_type is reported.  status is returned to operating system as
    # result of command line call..
    if(length(r$messages) == 0) {
      r$resultType <<- "Success"
      status = 0
    } else if (any(sapply(r$messages, \(x) inherits(x$message, "error")))) {
      r$resultType <<- "Error"
      status = 2
    } else {
      r$resultType <<- "Warning"
      status = 1
    }

    report_to <- r$reportArgs$report_to

    r$time <<- paste(as.POSIXct(Sys.time(), tz="UTC"), "UTC")

    # if report args weren't processed property, report to stderr
    if(!is_something(report_to)) {
      report_to <- "stderr"
      # error can't be reported to `report-to` arg so status is 3
      r$resultType <<- "Error **reporting failed**"
      status = 3
    }

    if(report_to == "stdout") {
      print(r)
    } else if(report_to == "stderr") {
      sink(stderr())
      print(r)
      sink()
    } else {
      # we're outputting JSON, so convert message list to text
      r$messages <<-
        lapply(
          r$messages,
          \(x) {
            x$message <- capture.output(print(x$message))
            x})
      if(grepl("^http", report_to)) {
        req <-
          httr2::request(report_to) |>
          httr2::req_method("POST") |>
          httr2::req_body_json(r)
        resp <- httr2::req_perform(req)
      } else {
        # only overwrite if parameter is found and is explicitly TRUE!
        append <- !is_something(r$reportArgs$overwrite_file)
        # if we are going json...
        if(grepl(".json(\\.gz)?$", report_to)) {
          # convert the message conditions to text
          readr::write_lines(
            jsonlite::toJSON(
              r, auto_unbox = TRUE),
            report_to,
            append = append)
        } else {
          saveRDS(r, r$reportArgs$report_to)
        }
      }
    }
    status
  }


  ####################################################################
  # PROCESS the expression
  ####################################################################
  process <- function(expr) {

    r[c("reportArgs", "expressionArgs")] <<-
      # convert command line args to a list
      commandArgsList(r$commandLineArgs) |>
      # split the list into reporting args and function args.
      (\(x){
        reportArgs_idx <- names(x) %in% report_arg_names
        list(reportArgs = x[reportArgs_idx],
             expressionArgs = x[!reportArgs_idx])})()

    # throw error if `report_to` argument is missing or malformed.
    check_report_to()

    # if report_to contains a environment variable, make the substitution
    if(Sys.getenv(r$reportArgs$report_to) != "")
      r$reportArgs$report_to <- Sys.getenv(report_to)

    check_overwrite_status()
    # throw simulated error or warning if requested; is dependent upon args_list
    # so can't be called before args_list is processed.

    simulate_conditions("args")

    simulate_conditions("expr")
    # evaluate the expression to call `fun`
    r$result <<- eval(expr, envir = r$expressionArgs)
    TRUE
  }

  # NOTE: TRUE returned by process() and FALSE returned by error handler are
  # currently not saved to a variable.  `report()` determines the status of
  # 'Success', 'Warning', or 'Error' by investigating `r$messages` list, which
  # are updated by calling handlers `report_error_with_trace` or
  # `store_warning_with_trace` whenever an error occurs.

  # process() returns TRUE is processing completes (with or without warning).
  # warning handler stores warnings for reporting.  First error handler
  # reports errors but doesn't trap them.  Second error handler traps the error
  # and returns FALSE.

  tryCatch(
    withCallingHandlers(
      process(substitute(expr)),
      warning = store_warning_with_trace,
      error = store_error_with_trace),
    error = \(e) return(FALSE))

  # report() will return 0, 1, or 2 depending on whether the report was for: 0 =
  # successful processing, 1 = warning in processing, or 2 = error during
  # processing. warning handler stores the warnings for reporting.
  # The first error handler prints any reporting error information to
  # stdout but doesn't catch error.  Second error handler catches error and
  # returns 3 to signify error during reporting

  tryCatch(
    withCallingHandlers(
      report(),
      warning = store_warning_with_trace,
      error = print_error_with_trace),
    error = \(e) return(3)
  )

}

# removes any items with duplicated names from a list.
remove_duplicate_args <- function(x) {
  dups <- duplicated(names(x))
  if(any(dups)) {
    dup_names <- unique(names(x)[dups])
    warning(
      "Repeat instances of these command line arguments were ignored: ",
      paste(dup_names, collapse = ", "))
    x <- x[!dups]
  }
  x
}

#' Is a value something other than NULL or FALSE?
#'
#' Useful for investigating if a parameter in a command line list, which can be
#' missing or FALSE to represent FALSE.
#' @returns If x is NULL or FALSE, returns FALSE, otherwise return TRUE
#' @param x value to be tested.
#' @examples
#' is_something(NULL)
#' is_something(FALSE)
#' is_something("Hello")
#' is_something(45)
#' @export
is_something <- function(x) {
  if(is.null(x)) return(FALSE)
  # if a logical of length 1, return the value.
  if(is.logical(x))
    if(length(x) == 1)
      return(x)
  # otherwise, it's not just FALSE, so it's set!
  TRUE
}

#' Wrapper for base::log function.
#'
#' A wrapper for \code{\link{log}} ensures that x and base are passed to log as
#' numeric values and includes `...` in parameter list.
#'
#' Used as a function to test and debug the \code{\link{batch}} function.
#'
#' Depending of the format of the command line arguem
#'
#' @param x,base See \code{\link{log}}.
#' @param ... Ignored. See Details.
#' @export
batch_log <- function(x, base = exp(1)) {
  if(is.character(x)) x <- strsplit(x, ",")[[1]]
  if(is.character(base)) base <- strsplit(base, ",")[[1]]
  log(as.numeric(x), as.numeric(base))
}
