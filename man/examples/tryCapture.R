# Simulate following command line:
#
# > Rscript -e "commandR::tryCapture(log(x,base))" --json='{"x":[1,5,10], "base":[10], "report-to":"stdout"}'
#
# This is the most reliable way to use tryCapture because all values are typed
# and JSON arrays and objects can be used to create parameters that become R
# vectors and lists.
tryCapture(log(x, base), args = '--json={"x":[1,5,10], "base":[10], "report-to":"stdout"}')

# Simulate following command line (yields exact same result as above but mixes
# stand-alone paramter with --json):
#
# > Rscript -e "commandR::tryCapture(log(x,base))" --report-to=stdout --json='{"x":[1,5,10], "base":[10]}'
#
tryCapture(log(x, base), args = c('--report-to=stdout', '--json={"x":[1,5,10], "base":[10]}'))

# Use only direct parameters:
#
# > Rscript -e "commandR::tryCapture(log(x,base))" --x=1,5,10 --base=10 --report-to=stdout
#
# This yields an error because direct command line parameters are always passed
# as strings and `log()` can not accept strings.  Notice, however, the error
# is capture and reported with a stack trace as part of the result list.
tryCapture(log(x, base), args = c("--x=1,5,10", "--base=10", "--report-to=stdout"))

# Direct command line parameters *can* be used, however,  writing a wrapper for
# `log()` See commandArgs::batch_log() as an example of such a wrapper:
body(commandR::batch_log)

# Using batch_log() rather than log(), the command line with only direct
# parameters works just fine.
#
# > Rscript -e "commandR::tryCapture(batch_log(x,base))" --x=1,5,10 --base=10 --report-to=stdout
#
tryCapture(batch_log(x, base), args = c("--x=1,5,10", "--base=10", "--report-to=stdout"))

# As demonstrated above, errors that occur in the expression passed to
# tryCapture will be reported to location specified by `--report-to`. To test
# error recovery in any larger pipeline that uses tryCapture, you can simulate
# an error or warning in an expression using the `--simulate-warning` and
# `--simulate-error` flags.  These conditions will be included in the "messages"
# member of the results list and sent to the `report-to` destination.  Error
# level 1 is returned for warnings, 2 for errors.
#
# > Rscript -e "commandR::tryCapture(log(x,base))" --json='{"x":[1,5,10], "base":[10], "report-to":"stdout", "simulate-warning":"expr"}'
# > Rscript -e "commandR::tryCapture(log(x,base))" --json='{"x":[1,5,10], "base":[10], "report-to":"stdout", "simulate-error":"expr"}'
#
tryCapture(
  log(x, base),
  args = '--json={"x":[1,5,10], "base":[10], "report-to":"stdout", "simulate-warning":"expr"}')

tryCapture(
  log(x, base),
  args = '--json={"x":[1,5,10], "base":[10], "report-to":"stdout", "simulate-error":"expr"}')

# However, unrecoverable errors can occur during argument parsing (meaning the
# report-to destination is not known) or during reporting (so the result list is
# not sent to the report-to destination). In either case, the result list,
# including warning and error reports, will be redirected to stderr and an error
# level of 3 is returned.  These types of condition can be simulated with
# `--simulate_error=args` or `--simulate_error=report` (If you click "Run
# examples" in the help file to execute the examples below, anything reported to
# stderr appears in the **console** and the stack trace is quite large...  These
# two examples might be best run by pasting into the console.)
tryCapture(
  log(x, base),
  args = '--json={"x":[1,5,10], "base":[10], "report-to":"stdout", "simulate-error":"args"}')

tryCapture(
  log(x, base),
  args = '--json={"x":[1,5,10], "base":[10], "report-to":"stdout", "simulate-error":"report"}')

# Finally, `mget(ls()` can be used in `expr` to create list of all expression
# parameters.
tryCapture(
  as.data.frame( mget(ls()) ),
  args = '--json={"x":[1,5,10], "y":[2,4,6], "z":[2,3,4], "report-to":"stdout"}')
