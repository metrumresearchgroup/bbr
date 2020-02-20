library(glue)
library(rbabylon)
# move to demo folder
#setwd(system.file(package="rbabylon", "nonmem"))
print(glue::glue("switched working directory to `{getwd()}` for demo."))

# cleanup function
cleanup_demo <- function() {
  # delete original acop output
  if (fs::dir_exists("acop")) fs::dir_delete("acop")

  # delete demo models
  demo_mods <- c(
    "acop2",
    "acop3",
    "acop4"
  )
  for (m in demo_mods) {
    if (fs::file_exists(glue("{m}.mod"))) fs::file_delete(glue("{m}.mod"))
    if (fs::file_exists(glue("{m}.yaml"))) fs::file_delete(glue("{m}.yaml"))
    if (fs::dir_exists(m)) fs::dir_delete(m)
  }
}
cleanup_demo()

# setup bbi path
#options('rbabylon.bbi_exe_path' = '/data/apps/bbi')

# create babylon.yaml
#bbi_init(".", "/opt/NONMEM")

################
# step by step
################

# create model spec
spec1 <- create_model(
  .model_path = "1.ctl",
  .yaml_path = "1.yaml",
  .description = "original acop model",
  .tags = c("acop tag", "other tag"),
  .bbi_args = list(overwrite = TRUE, threads = 4)
  )
class(spec1)
# [1] "bbi_nonmem_spec" "list"
str(spec1)

# submit model
res1 <- submit_model(spec1)
res1 <- submit_model("path/to/acop.yaml")
class(res1)
# [1] "bbi_nonmem_result" "babylon_result"    "list"
res1

sum1 <- model_summary(res1)
class(sum1)
# [1] "bbi_nonmem_summary" "list"
print(names(sum1))
# [1] "run_details"       "run_heuristics"    "parameters_data"   "parameter_names"   "ofv"               "shrinkage_details"
# [7] "covariance_theta"  "correlation_theta"
str(sum1)

  par_df1 <- param_estimates(sum1) %>% mutate(val = ifelse(param == "THETA"))
head(par_df1)

#########
# copy model spec for iteration
spec2 <- copy_model_from(spec1, "acop2", "model 2 description")
class(spec2)
# [1] "bbi_nonmem_spec" "list"
str(spec2)
cat(paste(readr::read_lines(spec2$model_path, n_max = 10), collapse = "\n" )) # notice the $PROBLEM line is changed

# submit new model
res2 <- submit_model(spec2)
class(res2)
res2

# do all the same stuff you did with the first one
sum2 <- model_summary(res2)
print(names(sum2))

par_df2 <- param_estimates(sum2)
head(par_df2)

# delete temp demo files
cleanup_demo()


########################
# composable workflow
########################

# iterate on original and run it
res2 <- copy_model_from(spec1, "acop2", "add additive error to residual error model")

# another iteration
res3 <- copy_model_from(res2, "acop3",
                        "model 2 with some changes",
                        .add_tags = c("new tag")) %>% submit_model()

# another iteration based on acop2
res4 <- copy_model_from(res1, "acop4",
                        "model 2 with different changes",
                        .add_tags = c("new tag"),
                        .based_on_additional = c("acop3")) %>% submit_model()


# construct run log and view it
log_df <- run_log(".")
View(log_df)

# compare some outputs from two of the runs
model_summary(res3) %>% param_estimates() %>% head()
model_summary(res4) %>% param_estimates() %>% head()

# delete temp demo files
cleanup_demo()


