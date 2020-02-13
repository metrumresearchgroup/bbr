
# move to demo folder
setwd(system.file(package="rbabylon", "nonmem"))
print(glue::glue("switched working directory to `{getwd()}` for demo."))

# cleanup function
cleanup_demo <- function() {
  demo_files <- c(
    "acop2.mod",
    "acop2.yaml",
    "acop3.mod",
    "acop3.yaml"
    )
  for (f in demo_files) {
    if (fs::file_exists(f)) fs::file_delete(f)
  }

  demo_dirs <- c(
    "acop",
    "acop2",
    "acop3"
  )
  for (d in demo_dirs) {
    if (fs::dir_exists(d)) fs::dir_delete(d)
  }
}
cleanup_demo()

# setup bbi path
options('rbabylon.bbi_exe_path' = '/data/apps/bbi')

# create babylon.yaml
bbi_init(".", "/opt/NONMEM")

################
# step by step
################

# create model spec
spec1 <- create_model_from_yaml("acop.yaml")
class(spec1)
# [1] "bbi_nonmem_spec" "list"
str(spec1)

# submit model
res1 <- submit_model(spec1)
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

par_df1 <- param_estimates(sum1)
options("crayon.enabled" = FALSE) # turn off annoying tibble color printing
head(par_df1)

#########
# copy model spec for iteration
spec2 <- copy_model_from(spec1, "acop2", "model 2 description")
class(spec2)
# [1] "bbi_nonmem_spec" "list"
str(spec2)
cat(paste(readr::read_lines(spec2$model_path, n_max = 10), collapse = "\n" ))

# submit new model
res2 <- submit_model(spec2)
class(res2)
res2

# do all the same stuff you did with the first one
sum2 <- model_summary(res2)
print(names(sum2))

par_df2 <- param_estimates(sum2)
head(par_df2)


########################
# composable workflow
########################

cleanup_demo()

res2 <- copy_model_from(orig_mod, new_mod_path2, "model 2 description") %>%
  submit_model()

new_mod_path3 <- "somepath"
copy_model_from(orig_mod_path, new_mod_path3,
                .add_tags = c("Assdfd")) %>%
  add_tags() %>%
  submit_nonmem_model()


new_mod_path4 <- "somepath"
copy_model_from(orig_mod_path, new_mod_path2) %>%
  submit_nonmem_model()


run_log(".")


model_summary(new_mod_path2)
model_summary(res2)



# # orig idea
# copy_model_from() %>%
#   add_tags() %>%
#   modify_data_set() %>%
#   update_params() %>%
#   submit_nonmem_model() %>%
#   nonmem_output()
#
#
