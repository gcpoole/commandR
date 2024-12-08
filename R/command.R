#' Execute a function from the command line in production, capturing warnings
#' and errors.
#'
#' \code{commandR()} allows any function to be used in a production environment.
#' It is intended to be used like \code{\link{do.call}()}, except that results
#' of the function are either:
#' \itemize{
#' \item{converted to JSON and written to a file or sent to a webhook;}
#' \item{printed to stdout or stderr; or}
#' \item{saved as an .rdata file using \code{\link{saveRDS}()}}
#' }
#'
#' The result of \code{fun} is incorporated into a list that contains a
#' timestamp, a result type, the command line arguments, the parsed
#' interpretation of the command line arguments, and a list of any warnings or
#' errors (complete with stack trace) (see Value, below).  The list can be
#' printed to stdout or stderr, saved as an .rdata file, posted to a webhook or
#' written to a file as JSON.
#'
#' To operate \code{commandR()} from the command line, place a call to
#' \code{commandR()} into a R script.  Then, call the script from the command
#' line (e.g., using Rscript).
#'
#' In addition to the parameters to be passed to `fun`, \link{batch()}
#' recognizes the following command line parameters:
#'
#' \itemize{
#' \item{\code{--report-to=<location>} determines how results are reported.
#' Value values of \code{report-to} are one of:
#' \itemize{
#' \item{the value "stdout" for reporting to stdout (the defualt);}
#' \item{the value "stderr" for reporting to stderr, or;}
#' \item{a fully specified file name for a local file;}
#' \item{a fully specified file name in an S3: bucket (starting with "S3://");}
#' \item{the URL of a webhook (staring with "http://" or "https://");}
#' \item{the name of an environment variable containing any of the above.}
#' }
#' Note that the results will be gzipped if .gz is at the end of a local or S3
#' file name.}
#' \item{\code{--simulate-error=<type>}. Simulates errors for testing and
#' debugging. <type> is one of "args", "function", "report", or
#' "function/report". "args" will simulate an error in the processing of command
#' line arguments, "function" an error in the function passed to `fun`, "report"
#' an error in reporting results, and "function/report" an error in both the
#' `fun` and reporting. `fun` errors will be reported to location specified by
#' `report-to`.  If any other error type is requested, reporting will be to
#' stderr.}
#' \item{\code{--simulate-warning=<type>}. Simulates warnings for testing and
#' debugging.  For <type> definition, see `--simulate-error`}
#' \item{\code{--overwrite}}. By default, when `--report-to` is a file name, the
#' file is appended. If `--overwrite` flag is included, the file will be
#' overwritten (if it exists). `--overwrite` has no effect if `--report-to` is
#' not a local disk file. }
#'
#' As an example, consider the \code{\link{log}} function in R.
#' \code{\link{log}} takes two arguments, \code{x} and \code{base}. An example
#' script called "logarithm.R" might be used to calculate and return the log of
#' values passed from the command line and report the result to a file, webhook,
#' or stdout.
#'
#' A simple (but not recommended) call from the command line might look like
#' this:
#'
#' \code{Rscript /path/to/logarithm.R --report-to=https://my.webhook.com
#' --x=12,14,30 --base = 10}
#'
#' and logarithm.R would look like this:
#'
#' \code{# -- file: logarithm.R -----------------------------} \cr
#'
#' \code{# wrapper that returns the result of log as a named list} \cr
#' \code{batch_log <- function(x, base = exp(1), ...) \{} \cr
#' \code{\verb{   if(is.character(x)) x <- strsplit(x, ",")[[1]]}}
#' \code{\verb{   log(as.numeric(x), as.numeric(base))}} \cr
#' \code{\}} \cr
#'
#' \code{batch(batch_log)}
#'
#' \code{#--------------------------------------------------}
#'
#' In this case, \code{batch()} will pass all parameters to
#' \code{\link{batch_log}()} and all parameters will be passed as character
#' strings.  There is also no mechanism to control to the data types of
#' arguments. "12,14,30" (the value of x) is passed to \code{\link{batch_log}()}
#' as a character. This means that the \code{\link{log}()} function can't be
#' called directly from within `logarithm.R`.  Instead,
#' \code{\link{batch_log}()} must be created to do some pre-processing before
#' \code{\link{log}()} can be called. Also, because `--report-to` and any other
#' parameters used by \code{batch()} will be passed to
#' \code{\link{batch_log}()}, `...` must be in the signature for
#' \code{\link{batch_log}()}, as shown above.
#'
#' All of these issues can be avoided by passing command line arguments as two
#' two JSON objects (the recommended approach), using the parameter names
#' `--batch-args` and `--fun-args`:
#'
#' \code{Rscript /path/to/logarithm.R --batch-args='{"report-to":"stdout"}'
#' --fun-args='{"x":[45,10,20],"base":10}'}
#'
#' In this case, only the arguments in `--batch-args` are passed to `fun` and
#' the arguments have data types (numeric, logical, string, etc.) Therefore, the
#' script can be much simpler (and less error prone):
#'
#' \code{# -- file: logarithm.R -----------------------------} \cr
#'
#' \code{batch(log)}
#'
#' \code{#--------------------------------------------------}
#'
#' A note on parallel processing... \code{batch} has been tested extensively
#' with \code{mclapply} for parallel processing. The safest approach is to embed
#' \code{mclapply} in a script called from the command prompt using
#' \code{Rscript} (Google "Rscript command line" for more.) Parallel processing
#' using \code{mclapply} to call \code{batch} from within Rstudio often works,
#' but can cause Rstudio to freeze (perhaps only if screen saver engages??).  In
#' this case, batch processing usually proceeds successfully even after RStudio
#' is frozen, but RStudio must be terminated manually, potentially leading to
#' loss of code.
#'
#' @returns A numeric vector with a single value: 0 if \code{fun} terminates
#'   successfully, 1 if \code{fun} terminates successfully with warnings,
#'   2 if \code{fun} throw an error, and 3 if an error occurs in reporting and
#'   no report was sent to `--report-to` (in this case a report will go to
#'   stderr). As a side effect, \code{batch()} creates a JSON object with the
#'   following format:
#'
#'   \{
#'     "resultType": c("Success", "Warning", "Error)
#'     "warningAndErrorMessages": [(warningMsg1, warningMsg2, ..., warningMsgN) (, error)],
#'     "time": "2024-09-07 00:30:37.577178 UTC",
#'     "commandLineArgs": <args>,
#'     "result": <result from `fun` (as JSON)>
#'   \}
#'
#'   Depending on the command line values contained in \code{args}, the JSON
#'   object will be written to location specified by `--report-to` command line
#'   parameter. See Details. If fun returns a value successfully, but an error
#'   prevents reporting to a file or webhook (e.g., the file can't be opened or
#'   the URL for the webhook is invalid), the error and results from `fun` will
#'   be reported to stderr. Therefore, even if reporting to a webhook or file is
#'   requested, piping stderr to a log file is recommended.
#'
#' @param fun A function that returns an object that can be converted to JSON
#'   using \code{\link[jsonlite]{toJSON}()}
#' @param args Generally set to NULL (the default), in which case \code{batch()}
#'   will call \code{\link{commandArgs}()} to retrieve and then process command
#'   line parameters (see Details, below, for required command line arguments).
#'   Alternatively, a character vector that mimics the results from
#'   \code{\link{commandArgs}()} can be passed for interactive use or debugging.
#'   Values must be in the form `--<parameter>=<argument>` (or `--<parameter>`
#'   for boolean switch).
#' @export
commandR <- function(fun, args = NULL) {

  fun_name = substitute(fun) |> as.character()
  if(fun_name[1] == "::") fun_name <- paste(fun_name[c(2,1,3)], collapse = "")

  # Within `batch()`, these variables act as globals
  report_arg_names <- c("report_to", "simulate_error", "simulate_warning", "overwrite_file")
  r <-
    list(
      time = NULL,
      resultType = NULL,
      messages = NULL,
      commandLineArgs = args,
      reportArgs = NULL,
      functionArgs = NULL,
      result = NULL)

  if(!is_set(r$commandLineArgs)) {
    r$commandLineArgs <- commandArgs(TRUE)
  }


  # ###############################
  # Helper functions.  Must be defined in this context because these functions
  # require access to the reporting variables above!
  # ###############################

  store_warning_with_trace <- function(w) {
    w <- as.stacktrace(w, "^(eval\\(expr\\))|((do)?[tT]ryCatch)", "(^.handle)|(^withRestarts)")
    #    w <- as.stacktrace(w)
    r$messages <<- c(r$messages, list(w))
    invokeRestart("muffleWarning")
  }

  store_error_with_trace <- function(e) {
    e <- as.stacktrace(e, "^(eval\\(expr\\))|((do)?[tT]ryCatch)", "(^.handle)|(^withRestarts)|(^h\\()")
    #    e <- as.stacktrace(e)
    r$messages <<- c(r$messages, list(e))
    #report()
  }

  print_error_with_trace <- function(e) {
    # don't trim the stacktrace here to avoid potential errors
    # occurring before printing.
    e <- as.stacktrace(e)
    r$messages <<- c(r$messages, list(e))
    sink(stderr())
    r$resultType <<- "Error **reporting failed**"
    print(r)
    sink(NULL)
    return(3)
  }

  simulate_conditions <- function(location) {
    types <- c("warning", "error")
    requests <- r$reportArgs[paste0("simulate_", types)]
    funs <- list(warning, stop)

    for(i in 1:2) {
      if(is_set(requests[i]))
        if(grepl(location, requests[i])) {
          # to simulate error in arg processing, remove
          # any successfully processed args...
          if(types[i] == "error" && location == "args") {
            r["reportArgs"] <<- list(NULL)
            r["functionArgs"] <<- list(NULL)
          }
          funs[[i]](
            "**Simulated** ", types[i], ". `--simulate-", types[i], "=",
            location, "` was included on command line.")
        }
    }
    TRUE
  }

  check_report_to <- function() {
    if(!is_set(r$reportArgs$report_to))
      stop("Required `--report-to=<destination>` parameter not detected on command line.")
    if(!is.character(r$reportArgs$report_to))
      stop("Required `--report-to=<destination>` parameter must be a character")
    if(length(r$reportArgs$report_to) != 1)
      stop("Required `--report-to=<destination>` must have one and only one character string.")
    if(r$reportArgs$report_to == "")
      stop("Required `--report-to=<destination>` can't be an empty string.")
    TRUE
  }


  # Report will report results to `report-to` and return a status that is then
  # returned to the OS as the result of the command line call to R. status =
  # c(NormalExit = 0, Warning=2, Error=2, ReportingError=3). ReportingError is
  # broken out from Error because it is more severe that Error.  If there is no
  # ReportingError, we can assume that Errors have been reported successfully to
  # `report-to`. In contrast, ReportingErrors and can only be sent to stderr
  # because, well, reporting to `report-to` generated an error!  Thus, a
  # ReportingError means that no report was sent to `report-to` and `report-to`
  # will never get a response, which could have knock-on effect for any pipeline
  # that is listening to `report-to`.
  report <- function() {

    r$time <<- paste(as.POSIXct(Sys.time(), tz="UTC"), "UTC")

    simulate_conditions("report")
    # result_type is reported.  status is returned to operating system as
    # result of command line call..
    if(length(r$messages) == 0) {
      r$resultType <<- "Success"
      status = 0
    } else if (any(sapply(r$messages, inherits, "error"))) {
      r$resultType <<- "Error"
      status = 2
    } else {
      r$resultType <<- "Warning"
      status = 1
    }

    report_to <- r$reportArgs$report_to

    r$time <<- paste(as.POSIXct(Sys.time(), tz="UTC"), "UTC")

    # if report args weren't processed property, report to stderr
    if(!is_set(report_to)) {
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
    } else if(grepl("^http", report_to)) {
      req <-
        httr2::request(report_to) |>
        httr2::req_method("POST") |>
        httr2::req_body_json(r)
      resp <- httr2::req_perform(req)
    } else {
      # only overwrite if parameter is found and is explicitly TRUE!
      append <- !is_set(r$reportArgs$overwrite_file)
      # if we are going json...
      if(grepl(".json(\\.gz)?$", report_to)) {
        # convert the message conditions to text
        r$messages <<- lapply(r$messages, \(x) capture.output(print(x)))
        readr::write_lines(
          jsonlite::toJSON(
            r, auto_unbox = TRUE),
          report_to,
          append = append)
      } else {
        saveRDS(r, r$reportArgs$report_to)
      }
    }
    status
  }

  process <- function()     {
    ###### Make sure `fun` is a character representation of what was passed
    ###### to `fun`

    if(!class(fun) %in% c("character", "function")) {
      stop("`fun` must be a function or function name as a character.")
    }

    ####### PROCESS ARGS
    #######

    args_list <-
      commandArgsList(r$commandLineArgs) |>
      convert_json_member() |>
      replace_names(name_replace = data.frame(pattern = "-", replacement = "_")) |>
      #      add_missing_arg_values(report_arg_names) |>
      remove_duplicate_args()

    report_arg_idx <- names(args_list) %in% report_arg_names
    r$reportArgs <<- args_list[report_arg_idx]
    r$functionArgs <<- args_list[!report_arg_idx]

    # throw error is report_to is missing or malformed.  if report_to has a
    # environment variable, make the substitution
    check_report_to()

    # if report_to contains a environment variable, make the substitution
    if(Sys.getenv(r$reportArgs$report_to) != "")
      r$reportArgs$report_to <- Sys.getenv(report_to)

    # test to see if existing .rdata file can be overwritten.
    if(!is_set(r$reportArgs$overwrite_file)) {
      is_rdata_output <-
        !grepl(
          "(^stderr$)|(^stdout$)|(^http)|(.json(.gz)?$)",
          r$reportArgs$report_to)
      # check we're not allowed to overwrite
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

    # throw simulated error or warning if requested; is dependent upon args_list
    # so can't be called before args_list is processed.
    simulate_conditions("args")

    ###### CALL fun WITH r$functionArgs #############################
    ######

    simulate_conditions("function")
    browser()
    # to improve the stack trace, build an expression that calls fun
    # with r$functionArgs.  (Using do.call makes for a horrendous stack trace...)
    args_as_char <-
      paste0(names(r$functionArgs), " = r$functionArgs[['", names(r$functionArgs), "']]") |>
      paste(collapse = ", ")
    expr <-
      paste0(fun_name, "(", args_as_char, ")") |>
      parse(text = _)

    # evaluate the expression to call `fun`
    r$result <<- eval(expr)
    TRUE
  }

  #########
  # Begin "batch" logic...
  #########

  # NOTE: TRUE returned by process() and FALSE returned by error handler are
  # currently not saved to a variable.  `report()` determines the status of
  # 'Success', 'Warning', or 'Error' by investigating `r$messages` list, which
  # are updated by calling handlers `report_error_with_trace` or
  # `store_warning_with_trace` whenever an error occurs.

  # process() returns TRUE is processing completes (with or without warning).
  # warning handler stores warnings for reporting.  First error handler
  # reports errors but doesn't trap them.  Second error handler traps the error
  # and returns FALSE.

  processed <-
    tryCatch(
      withCallingHandlers(
        process(),
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

convert_json_member <- function(x) {
  i <- grepl("^json$", names(x), ignore.case = T)
  if(any(i)) {
    if(sum(i) > 1) stop("Only one JSON object can be specified")
    x <- c(
      x[-which(i)],
      jsonlite::fromJSON(x[[which(i)]]))
  }
  x
}

add_missing_arg_values <- function(x, names) {
  missing <- !names %in% names(x)
  if(any(missing)) {
    x <- c(
      x,
      structure(
        as.list(rep(FALSE, sum(missing))),
        names = names[missing]
      )
    )
  }
  x
}

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

is_set <- function(x) {
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
batch_log <- function(x, base = exp(1), ...) {
  if(is.character(x)) x <- strsplit(x, ",")[[1]]
  log(as.numeric(x), as.numeric(base))
}
