
# move to demo folder
setwd(system.file(package="rbabylon", "nonmem"))
print(glue("switched working directory to `{getwd()}` for demo."))

# create babylon.yaml
bbi_init(".", "/opt/NONMEM")

# setup bbi path
options('rbabylon.bbi_exe_path' = '/data/apps/bbi')

# define models
orig_mod <- "acop"

new_mod_path2 <- "acop2"

################
# step by step
################

# create model spec
# submit model


# copy model spec
# (from other spec...) !!!!!!
spec2 <- copy_model_from(orig_mod, new_mod_path2, "model 2 description")
class(spec2)
# [1] "bbi_nonmem_spec" "list"
str(spec2)

# submit new model
res2 <- submit_model(spec2)
class(res2)
# [1] [1] "bbi_nonmem_result" "babylon_result"    "list"
str(res2)

sum2 <- model_summary(res2)
class(sum2)
# [1] "bbi_nonmem_summary" "list"
print(names(sum2))
# [1] "run_details"       "run_heuristics"    "parameters_data"   "parameter_names"   "ofv"               "shrinkage_details"
# [7] "covariance_theta"  "correlation_theta"
str(sum2)

par_df2 <- param_estimates(sum2)
options("crayon.enabled" = FALSE) # turn off annoying tibble color printing
head(par_df2)

########################
# composable workflow
########################

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
