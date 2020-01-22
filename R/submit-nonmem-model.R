submit_nonmem_model <- function(.path,
                                type = c("sge", "local"),
                                ...,
                                clean_lvl = 2,
                                .config_path) {
  type <- match.arg(type)
  cmd_args <- c("run", "nonmem", type)
  if (!is.null(.config_path)) {
   cmd_args <- c(cmd_args, sprintf("--config=%s", .config_path))
  }
   bbi_exec(.path, cmd_args, ...)
}
