
DEFAULT_SUFFIXES <- c(
  '.msf',
  '.ext',
  '.tab',
  '.chn',
  'par.tab'
)

set_table_files <- function(.mod, mod_id, suffixes = DEFAULT_SUFFIXES){
  mod_str <- nmrec::read_ctl(get_model_path(.mod))

  # Replace table records with repeated identical record; length of `suffixes`
  is_table <- purrr::map_lgl(mod_str$records, function(r) r$name == "table")
  if (any(is_table)) {
    new_recs <- purrr::map(seq_along(suffixes), ~{
      mod_str$records[is_table][[1]]
    })
    mod_str$records[is_table] <- NULL

    mod_str$records <- c(mod_str$records, new_recs)
    # save out ctl to recalibrate records
    nmrec::write_ctl(mod_str, get_model_path(.mod))
  }

  # set all table records to cover all of suffixes
  mod_str <- nmrec::read_ctl(get_model_path(.mod))
  table_recs <- nmrec::select_records(mod_str, "table")
  purrr::walk2(table_recs, suffixes,
               function(tbl_rec, suffix){
                 table_file <- nmrec::get_record_option(tbl_rec, "file")
                 table_file$value <- paste0(mod_id, suffix)
               })
  nmrec::write_ctl(mod_str, get_model_path(.mod))
}


get_table_files <- function(.mod){
  mod_str <- nmrec::read_ctl(get_model_path(.mod))
  table_recs <- nmrec::select_records(mod_str, "table")

  table_files <- purrr::map_chr(table_recs, function(tbl_rec){
    nmrec::get_record_option(tbl_rec, "file")$value
  })

  return(table_files)
}

test_that("update_model_id() works with run number [BBR-CMH-001]", {
  on.exit({ cleanup() })

  # copy model and write strings with parent id to control stream
  new_mod <- copy_model_from(MOD1, basename(NEW_MOD2))

  # overwrite starting table files
  set_table_files(new_mod, mod_id = get_model_id(MOD1))

  # update model id
  update_model_id(new_mod)

  expect_equal(
    get_table_files(new_mod),
    paste0(get_model_id(new_mod), DEFAULT_SUFFIXES)
  )
})

test_that("update_model_id() works with character ID [BBR-CMH-002]", {
  on.exit({ cleanup() })

  # copy model and write strings with parent id to control stream
  parent_mod <- copy_model_from(MOD1, "Parent")
  new_mod <- copy_model_from(parent_mod, "Child")

  # overwrite starting table files
  set_table_files(new_mod, mod_id = get_model_id(parent_mod))

  # update model id
  update_model_id(new_mod)

  expect_equal(
    get_table_files(new_mod),
    paste0(get_model_id(new_mod), DEFAULT_SUFFIXES)
  )
})

test_that("update_model_id() is case-sensitive [BBR-CMH-003]", {
  on.exit({ cleanup() })
  suff_all_case <- c(toupper(DEFAULT_SUFFIXES), tolower(DEFAULT_SUFFIXES))

  # copy model and write strings with parent id to control stream
  new_mod <- copy_model_from(MOD1, basename(NEW_MOD2))

  # overwrite starting table files
  set_table_files(new_mod, mod_id = get_model_id(MOD1), suffixes = suff_all_case)

  # update model id
  update_model_id(new_mod)

  expect_equal(
    get_table_files(new_mod),
    paste0(get_model_id(new_mod), suff_all_case)
  )
})

test_that("update_model_id() .suffixes works [BBR-CMH-004]", {
  on.exit({ cleanup() })

  # copy model and write strings with parent id to control stream
  new_mod <- copy_model_from(MOD1, basename(NEW_MOD2))
  orig_suff <- paste0(get_model_id(MOD1), DEFAULT_SUFFIXES)

  # overwrite starting table files
  set_table_files(new_mod, mod_id = get_model_id(MOD1))

  # update model id
  update_model_id(new_mod, .suffixes = DEFAULT_SUFFIXES[1])

  expect_equal(
    get_table_files(new_mod),
    c(
      paste0(get_model_id(new_mod), DEFAULT_SUFFIXES[1]),
      orig_suff[2:length(orig_suff)]
    )
  )
})


test_that("update_model_id() .additional_suffixes works [BBR-CMH-005]", {
  on.exit({ cleanup() })

  # copy model and write strings with parent id to control stream
  new_mod <- copy_model_from(MOD1, basename(NEW_MOD2))
  new_suff <- ".naw"

  # overwrite starting table files
  set_table_files(new_mod, mod_id = get_model_id(MOD1),
                  suffixes = c(DEFAULT_SUFFIXES, new_suff))

  update_model_id(new_mod, .additional_suffixes = new_suff)

  expect_equal(
    get_table_files(new_mod),
    paste0(get_model_id(new_mod), c(DEFAULT_SUFFIXES, new_suff))
  )
})

test_that("update_model_id() errors with no based_on [BBR-CMH-006]", {
  expect_error(
    update_model_id(MOD1),
    regexp = "based_on is empty"
  )
})
