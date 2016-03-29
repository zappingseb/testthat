.active_tests <- character()

start_test <- function(test) {
  .active_tests <<- c(.active_tests, test)
  get_reporter()$start_test(context = get_reporter()$.context, test = test)
}

end_test <- function(test) {
  .active_tests <<- head(.active_tests, n = -1)
  get_reporter()$end_test(context = get_reporter()$.context, test = test)
}

generate_handlers <- function(status) {

  frame <- sys.nframe() - 1
  status$ok <- TRUE

  register_expectation <- function(e, start_frame, end_frame) {
    calls <- sys.calls()[start_frame:end_frame]
    srcref <- find_first_srcref(calls)

    test <- tail(.active_tests, n = -1)
    e <- as.expectation(e, srcref = srcref)
    e$call <- calls
    e$test <- test %||% "(unknown)"
    status$ok <- status$ok && expectation_ok(e)

    get_reporter()$add_result(context = get_reporter()$.context, test = test, result = e)
  }

  handle_error <- function(e) {
    # Capture call stack, removing last two calls from end (added by
    # withCallingHandlers), and first frame + 7 calls from start (added by
    # tryCatch etc)
    e$call <- sys.calls()[(frame + 11):(sys.nframe() - 2)]

    register_expectation(e, frame + 11, sys.nframe() - 2)
    signalCondition(e)
  }

  handle_expectation <- function(e) {
    register_expectation(e, frame + 11, sys.nframe() - 6)
    invokeRestart("continue_test")
  }

  handle_warning <- function(e) {
    register_expectation(e, frame + 11, sys.nframe() - 6)
    invokeRestart("muffleWarning")
  }

  handle_message <- function(e) {
    invokeRestart("muffleMessage")
  }

  handle_skip <- function(e) {
    register_expectation(e, frame + 11, sys.nframe() - 2)
    signalCondition(e)
  }

  list(error       = handle_error,
       expectation = handle_expectation,
       warning     = handle_warning,
       message     = handle_message,
       skip        = handle_skip)

}
