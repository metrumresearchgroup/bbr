# Install development version of package
if (packageVersion("bbr") != package_version("1.5.0.8003")) {
  remotes::install_github("metrumresearchgroup/bbr", ref = "1.5.0.8003")
}

library(bbr)
packageVersion("bbr")
# this should say ‘1.5.0.8003’
# if not, try restarting R and trying again, or try the install again

# If you dont have bbi installed, install before proceeding
if (!nzchar(bbi_version())) {
  path_to_bbi <- '/data/apps/bbi'
  bbr::use_bbi(path_to_bbi) # install to path

  # Set bbi path
  options('bbr.bbi_exe_path' = path_to_bbi)

  # confirm bbi path is set (precautionary)
  getOption("bbr.bbi_exe_path")
}


### Test with /data/pirana_examples (installed on every metworx blueprint) ###

# Directory containing previously run PsN models
psn_dir <- normalizePath(file.path("/", "data", "pirana_examples"))

# New directory (where you want the bbr models to be)
.bbr_dir <- file.path(tempdir(), "bbr_conversion")
fs::dir_create(.bbr_dir)

# `convert_psn` is the main function for converting a PsN run to a bbr model
# `.psn_model_file` is often not required, but may be if we cant locate the original model
# This can happen if the directories have changed, or the model was executed on a different machine
# Note that `modelfit_dir2` is missing the required table files, so this example will not work

mod <- convert_psn(.modelfit_dir = file.path(psn_dir, "modelfit_dir1"),
                   .bbr_dir = .bbr_dir,
                   .overwrite = TRUE)

# Test constructed model
mod_sum <- mod %>% model_summary()
print(mod_sum)
mod_sum %>% param_estimates()
mod_sum %>% get_omega()
mod_sum %>% get_theta()

# Note: convert_psn() will return the new model object,
#   but you can also read it in later with the following:
# mod <- bbr::read_model(file.path(.bbr_dir, "run101"))


# one of the nice things about bbr is how it integrates with other MeRGE packages
# here we show how to make simple diagnositic plots with the pmplots package
#   see more here: https://merge.metrumrg.com/expo/expo1-nonmem-foce/posts/intro-to-pmplots.html
if(!require(pmplots)){
  remotes::install_github("metrumresearchgroup/pmplots")
}

library(pmplots)

# bbr::nm_tables() will pull your input data and all of your table files into R as tibbles
mod_tables <- nm_tables(mod)
View(mod_tables)

# make some plots...
dv_pred(mod_tables$sdtab101)
dv_ipred(mod_tables$sdtab101)
res_time(mod_tables$sdtab101)


# Test that the new model can be re-submitted as a bbr model
bbi_init(.bbr_dir, "/opt/NONMEM", "nm75") # Initialize bbi in execution directory
mod %>% submit_model(.mode = "local", .overwrite = TRUE) # Submit model (locally in this example)
wait_for_nonmem(mod) # make sure the model is finished running before calling model_summary()

mod %>% model_summary()



# Convert multiple models at once -----------------------------------------

# Test all the PsN examples at once (minus 2 cases)
psn_fit_dirs <- list.dirs(psn_dir, recursive = FALSE)
psn_fit_dirs <- psn_fit_dirs[grepl("modelfit", psn_fit_dirs)]
psn_fit_dirs <- psn_fit_dirs[!grepl("modelfit_dir2", psn_fit_dirs)] # remove modelfit_dir2 (table files missing)
psn_fit_dirs <- psn_fit_dirs[!grepl("modelfit_dir9", psn_fit_dirs)] # modelfit_dir9 had 3 models executed at the same time, see below
print(psn_fit_dirs)

# map over dirs and call convert_psn() on each
list_of_new_mods <- purrr::map(psn_fit_dirs, ~{
  convert_psn(.modelfit_dir = .x,
              .bbr_dir = .bbr_dir,
              .overwrite = TRUE)
})

# log of all converted models
log_df <- summary_log(.bbr_dir)
View(log_df)
# __see note at the bottom about why there are only four models here, but 17 in mod_sums__

# load raw model summary objects
mod_sums <- list_of_new_mods %>% model_summaries()
View(mod_sums)


# PsN submission with multiple model executions (example in modelfit_dir9)
# i.e. execute -run_on_sge run101.mod run102.mod run103.mod

# convert a single one of these by specifying .psn_model_file
mod <- convert_psn(.modelfit_dir = file.path(psn_dir, "modelfit_dir9"),
                   .psn_model_file = file.path(psn_dir, "run101.mod"), # one of run101.mod, run102.mod, run103.mod
                   .bbr_dir = file.path(.bbr_dir, "multiple_executions"),
                   .overwrite = TRUE)

mod


# If you wanted to convert all three models in modelfit_dir9 at once
mods_exec <- c("run101.mod", "run102.mod", "run103.mod")
all_three_mods <- purrr::map(mods_exec, ~{
  run_x <- fs::path_ext_remove(.x)
  .bbr_dir_x <- file.path(.bbr_dir,"multiple_executions_2") # putting new models in sub-directory
  fs::dir_create(.bbr_dir_x)
  convert_psn(.modelfit_dir = file.path(psn_dir, "modelfit_dir9"),
              .psn_model_file = file.path(psn_dir, .x),
              .bbr_dir = .bbr_dir_x,
              .overwrite = TRUE)
})

log_df <- summary_log(.bbr_dir, .recurse = TRUE)
View(log_df)

all_three_sums <- all_three_mods %>% model_summaries()
View(all_three_sums)
all_three_sums[[1]]$bbi_summary %>% param_estimates()
all_three_sums[[2]]$bbi_summary %>% param_estimates()
all_three_sums[[3]]$bbi_summary %>% param_estimates()

##################################################
# Note about why there are only four models above
##################################################

# The reason you only see 4 models in log_df above (despite having converted 17 modelfit_dir's)
#   is that there are only actually 4 distinct models here, they have just each been run several
#   times. Because we passed `.overwrite = TRUE` to `convert_psn()`, it is overwriting models
#   with the same name in the destination directory, each time it finds them.
#
# You can see this by running the code below, which creates sub-directories for each model,
#   so that they won't be overwritten. Notice how the `nested_log_df` now has 17 rows, and
#   the absolute_model_path column is all unique, but there are actually only 4 different models.

nested_bbr_dir <- file.path(tempdir(), "bbr_conversion_nested")
fs::dir_create(nested_bbr_dir)
mods <- purrr::map(psn_fit_dirs, ~{
  run_x <- readr::parse_number(.x)
  .bbr_dir_x <- file.path(nested_bbr_dir, paste0("modelfit_dir", run_x))
  fs::dir_create(.bbr_dir_x)
  convert_psn(.modelfit_dir = .x,
              .bbr_dir = .bbr_dir_x,
              .overwrite = TRUE)
})

nested_log_df <- summary_log(nested_bbr_dir, .recurse = TRUE)
View(nested_log_df %>% dplyr::arrange(run))
