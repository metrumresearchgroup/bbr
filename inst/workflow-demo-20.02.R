library(rbabylon)
# move to demo folder
setwd(system.file(package="rbabylon", "nonmem"))
print(glue::glue("switched working directory to `{getwd()}` for demo."))

# cleanup function
cleanup_demo <- function() {

  # delete original acop output and yaml
  if (fs::dir_exists("1")) fs::dir_delete("1")
  if (fs::file_exists("1.yaml")) fs::file_delete("1.yaml")

  # delete demo models
  demo_mods <- c(
    "2",
    "3",
    "4"
  )
  for (m in demo_mods) {
    if (fs::file_exists(glue::glue("{m}.ctl"))) fs::file_delete(glue::glue("{m}.ctl"))
    if (fs::file_exists(glue::glue("{m}.yaml"))) fs::file_delete(glue::glue("{m}.yaml"))
    if (fs::dir_exists(m)) fs::dir_delete(m)
  }
}
cleanup_demo()

# setup bbi path
options('rbabylon.bbi_exe_path' = '/data/apps/bbi')

# clear old babylon.yaml
if (fs::file_exists("babylon.yaml")) fs::file_delete("babylon.yaml")

# create new babylon.yaml
bbi_init(".", "/opt/NONMEM", "nm74gf")

################
# step by step
################

# create model spec
spec1 <- create_model(
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
class(res1)
# [1] "bbi_nonmem_result" "babylon_result"    "list"
res1

# Try to get a summary but return immediately if model isn't finished
sum1 <- model_summary(res1, .wait = NULL)

# Try to get a summary and wait if not finished.
sum1 <- model_summary(res1)
class(sum1)
# [1] "bbi_nonmem_summary" "list"
print(names(sum1))
# [1] "run_details"       "run_heuristics"    "parameters_data"   "parameter_names"   "ofv"               "shrinkage_details"
# [7] "covariance_theta"  "correlation_theta"
str(sum1)

par_df1 <- param_estimates(sum1)
head(par_df1)

#########
# copy model spec for iteration
spec2 <- copy_model_from(spec1, "2", "model 2 description")
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
create_model(.yaml_path = "1.yaml",
             .description = "original acop model",
             .tags = c("acop tag", "other tag"),
             .bbi_args = list(overwrite = TRUE, threads = 4)
           ) %>% submit_model() %>%
  copy_model_from("2",
                  "model 2 description") %>% submit_model() %>%
  copy_model_from("3",
                  "model 2 with some changes",
                  .add_tags = c("new tag")) %>% submit_model() %>%
  copy_model_from("4",
                  "model 2 with different changes",
                  .add_tags = c("new tag"),
                  .based_on_additional = c("2")) %>% submit_model()

# construct run log and view it
log_df <- run_log()
print(names(log_df))
View(log_df)

# optionally add config columns (model hash, etc.)
log_df <- run_log() %>% add_config()
print(names(log_df))
View(log_df)

# compare some outputs from two of the runs
model_summary("3") %>% param_estimates() %>% head()
model_summary("4") %>% param_estimates() %>% head()


# delete temp demo files
cleanup_demo()


########################
# more realistic workflow
########################

# iterate on original and run it
res1 <- create_model(
  .yaml_path = "1.yaml",
  .description = "original acop model",
  .tags = c("acop tag", "other tag"),
  .bbi_args = list(overwrite = TRUE, threads = 4)
) %>% submit_model()

# look at results in sum1
sum1 <- model_summary(res1)

# after it works, add some tags, etc
res1 <- res1 %>% add_tags("base model")

#
spec2 <- res1 %>% copy_model_from("2",
                                  "use proportional error model")

spec3 <- res1 %>% copy_model_from("3",
                                  "use mixed error model")

#### change the ctl files manually

# submit new models
res_list <- purrr::map(list(spec2, spec3),
                       submit_model)

# construct run log and view it
log_df <- run_log()
View(log_df)

# add some tags and notes
res_list[[1]] <- res_list[[1]] %>%
  add_tags(c("iterations round 1", "discard")) %>%
  add_decisions("Made change x in prop error model")

res_list[[2]] <- res_list[[2]] %>%
  add_tags("iterations round 1") %>%
  add_decisions(c("Added param z in mixed error model", "did some other thing"))

# look at updated run log view it
log_df <- run_log()
View(log_df)


# delete temp demo files
cleanup_demo()

# clear old babylon.yaml
if (fs::file_exists("babylon.yaml")) fs::file_delete("babylon.yaml")
