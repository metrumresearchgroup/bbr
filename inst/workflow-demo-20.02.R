copy_model_from() %>%
  add_tags() %>%
  modify_data_set() %>%
  update_params() %>%
  submit_nonmem_model() %>%
  nonmem_output()



#setwd(system.file(package="rbabylon", "nonmem"))
# OR (like this better ^)
#house <- function() {
#  fs::file_copy(system.file(package="rbabylon", "nonmem/acop.mod"), getwd())
#}


orig_mod <- "acop"

new_mod_path2 <- "somepath"
res2 <- copy_model_from(orig_mod, new_mod_path2, "desc") %>%
  submit_nonmem_model()

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




