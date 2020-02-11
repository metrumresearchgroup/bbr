# Helper functions for manipulating results objects

# get exit status of process
get_exit_status <- function (.res, ...) {
  UseMethod("get_exit_status", .res)
}

get_exit_status.babylon_result <- function(.res, .check = FALSE) {
  if (.res$process$is_alive()) {
    warning(paste0("Process ", .res$process$get_pid(), " is still running. You cannot check the exit status until it is finished."))
    invisible()
  } else {
    exit_status <- .res$process$get_exit_status()

    if (.check) {
      check_status_code(exit_status, .res$output, .res$cmd_args)
    }
    return(exit_status)
  }
}


# fetch output (stdout and stderr) of process
get_stdout <- function(.res, ...) {
  UseMethod("get_stdout", .res)
}

get_stdout.babylon_result <- function(.res) {
  # check if process is still alive (as of now, can only get output from finished process)
  if (.res$process$is_alive()) {
    warning(paste0("Process ", .res$process$get_pid(), " is still running. You cannot read the output until it is finished."))
    invisible()
  } else {
    # if output has not been read from buffer, read it
    if (is.null(.res$output)) {
      .res$output <- .res$process$read_all_output_lines()
    }
    # return output
    return(.res)
    # I don't like this buuuut...
    # https://stackoverflow.com/questions/60062105/replacement-functions-in-r-that-dont-take-input
  }
}


# parse NONMEM output to list
parse_model_output <- function(.res, ...) {
  UseMethod("parse_model_output", .res)
}

parse_model_output.babylon_result <- function(.res) {
  res_list <- nonmem_output(.res$output_dir)
  return(res_list)
}

parse_model_output.character <- function(.res) {
  res_list <- nonmem_output(.res)
  return(res_list)
}

