.run_verbose <- function(task_function, ...) {
  # Plan to use multisession for asynchronous processing
  future::plan(future::multisession)

  # Run the task in a separate future with the provided arguments
  task_future <- future::future({
    suppressWarnings({
      task_function(...)
    })
  })

  # Show dots while waiting for the task to complete
  while (!future::resolved(task_future)) {
    for (i in 1:5) {
      if (future::resolved(task_future)) break
      cat(".")
      flush.console()
      Sys.sleep(0.1)  # Adjust the sleep time as needed
    }

    if (!future::resolved(task_future)) {
      cat("\r     \r")  # Clear the dots
      flush.console()
    }
  }

  cat("\r     \r")
  flush.console()
  cat("\n")

  # Retrieve the result or handle the error
  suppressWarnings({
    task_result <- future::value(task_future)
  })

  if (is.null(task_result)) {
    message("The task returned NULL. An error might have occurred.")
    stop("Future task encountered an error. Check the logs for details.")
  }

  return(task_result)
}