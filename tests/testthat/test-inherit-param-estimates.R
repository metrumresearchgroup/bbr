


describe("inherit_param_estimates: inherit thetas", {

  it("base case", {

    # starting record
    theta_rec <- get_example_record("base case", .record_name = "theta")

    # replacement values
    new_thetas <- example_rec_lengths(theta_rec, "theta")

    # Copy Thetas
    copy_thetas(
      .mod_lines = theta_rec[[1]], .new_thetas = new_thetas,
      .bounds_opts = "maintain_bounds"
    )

    # Inspect record
    recs <- nmrec::select_records(theta_rec[[1]], "theta")

    #expect_equal(recs[[1]]$format(), expected_string)
  })

  it("Second block for fixing theta value", {

    # starting record
    theta_rec <- get_example_record("fix theta block", .record_name = "theta")

    # replacement values
    new_thetas <- example_rec_lengths(theta_rec, "theta")
    new_thetas <- new_thetas[[1]] # second block isn't used

    # Copy Thetas
    copy_thetas(
      .mod_lines = theta_rec[[1]], .new_thetas = new_thetas,
      .bounds_opts = "maintain_bounds"
    )

    # Inspect record
    recs <- nmrec::select_records(theta_rec[[1]], "theta")
  })

})


#
#
#
# parse_ctl <- function(x){
#
#   x0 <- strsplit(x,'\n')[[1]]
#   x0 <- paste0(x0[!grepl('^\\s*;',x0)],collapse = '\n')
#
#   x1 <- gsub('(\n\n|\n|\n;|\n\n;)$','',strsplit(x0,'\\$')[[1]])
#   x2 <- sub('\n',' ',x1)
#   x3 <- as.list(gsub('^(.*?)\\s+','',x2))
#   names(x3) <- gsub('\\s+(.*?)$','',x2)
#
#   # Ensure names are in the correct order
#   block_names <- unique(names(x3)[names(x3) != ""])
#
#   x3 <- x3[sapply(x3,nzchar)]
#
#   x3 <- split(x3,names(x3))
#
#   x3 <- lapply(x3,purrr::set_names,nm=NULL)
#
#   x4 <- lapply(x3,function(x){
#     if(is.list(x)){
#       out <- lapply(x,function(xx){
#         out <- gsub('^\\s+|\\s+$','',strsplit(xx,'\n')[[1]])
#         out[nzchar(out)]
#       })
#       list(out)
#     }else{
#       out <- gsub('^\\s+|\\s+$','',strsplit(x,'\n')[[1]])
#       out[nzchar(out)]
#     }
#
#   })
#
#   nc <- block_names[(!nzchar(gsub('[A-Z]','',block_names)))]
#
#   x4 <- x4[nc[nchar(nc)>1]]
#
#   unlist(x4,recursive = FALSE)
# }
#
# generate_ctl <- function(parsed_ctl) {
#   ctl_text <- character(0)
#
#   for (block_name in names(parsed_ctl)) {
#     block_text <- paste0("$", block_name, "\n", paste(parsed_ctl[[block_name]], collapse = "\n"), "\n")
#     ctl_text <- c(ctl_text, block_text)
#   }
#
#   return(ctl_text)
# }
#
#
#
#
# readLines(template_mod)
#
# parsed_ctl <- parse_ctl(readLines(template_mod) %>% paste0(collapse = "\n"))
# generate_ctl(parsed_ctl) %>% cat()

create_test_model <- function(
    .new_thetas = NULL,
    .new_sigmas = NULL,
    .new_omegas = NULL,
    .new_mod_path = file.path(tempdir(), "fake")
){
  model_dir <- system.file("model", "nonmem", "basic",   package = "bbr")
  template_mod <- file.path(model_dir, "1.ctl")

  mod_lines <- nmrec::read_ctl(template_mod)

  # Replace thetas
  if(!is.null(.new_thetas)){
    theta_recs <- nmrec::select_records(mod_lines, "theta")
    theta_recs <- NULL
  }

  # Replace sigmas
  if(!is.null(.new_sigmas)){
    sigma_recs <- nmrec::select_records(mod_lines, "sigma")
    sigma_recs <- .new_sigmas
  }

  # Replace omegas
  if(!is.null(.new_omegas)){
    omega_recs <- nmrec::select_records(mod_lines, "omega")
    omega_recs <- .new_omegas
  }

  # Write new model
  nmrec::write_ctl(mod_lines, ctl_ext(.new_mod_path))

  # Make yaml
  write_lines("model_type: nonmem", yaml_ext(.new_mod_path))

  # Return model object
  read_model(.new_mod_path)
}


modify_ctl_file <- function(blocks_to_overwrite = c("THETA", "OMEGA", "SIGMA")) {
  # Read the control file into a character vector
  model_dir <- system.file("model", "nonmem", "basic",   package = "bbr")
  template_mod <- file.path(model_dir, "1.ctl")
  ctl_lines <- readLines(template_mod, warn = FALSE)

  # Initialize variables to track the current block and modified content
  current_block <- ""
  modified_content <- character(0)

  # Iterate through each line in the control file
  for (line in ctl_lines) {
    # Check if the line starts with "$" indicating a block header
    if (startsWith(line, "$")) {
      # Extract the block name (e.g., THETA, OMEGA, SIGMA)
      current_block <- sub("^\\$([A-Z]+).*", "\\1", line)

      # Check if the current block should be deleted
      if (current_block %in% blocks_to_overwrite) {
        current_block <- ""  # Reset the current block
        next  # Skip this block
      }
    }

    # Append the line to the modified content (with a newline separator)
    modified_content <- c(modified_content, line)
  }

  # Collapse the modified content into a single string
  modified_content <- paste(modified_content, collapse = "\n")

  return(modified_content)
}
