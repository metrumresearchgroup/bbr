test_threads <- function(.mod,
                         threads = c(2,4),
                         .mode = "sge",
                         .bbi_args = list(nm_version = "nm74"))
{

  assertthat::assert_that(is.list(.bbi_args))

  .mods <- map(threads, ~ copy_model_from(.mod, paste0(get_model_id(.mod), ".", .x, "_threads")) %>%
                  add_bbi_args(.bbi_args = c(threads = .x,
                                             .bbi_args,
                                             parallel = TRUE,
                                             overwrite = TRUE)))

  mod_paths <- lapply(.mods, function(mod.x){mod.x$absolute_model_path}) %>% unlist()

  # Cleanup
  on.exit({
    for (m in mod_paths) {
      if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
      if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
    }
  })

  proc_list <- submit_models(.mods, .mode = .mode, .wait = FALSE)

}

# Think ill have to create a new model object for each 'test'
# will need copy_model_from and tags most likely
