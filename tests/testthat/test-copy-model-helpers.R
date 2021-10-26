
DEFAULT_SUFFIXES <- c(
  '.MSF',
  '.ext',
  '.tab',
  '.chn',
  'par.tab'
)

test_that("update_model_id() works with run number [BBR-CMH-001]", {
  on.exit({ cleanup() })

  # copy model and write strings with parent id to control stream
  new_mod <- copy_model_from(MOD1, basename(NEW_MOD2))
  readr::write_lines(
    paste0(get_model_id(MOD1), DEFAULT_SUFFIXES),
    get_model_path(new_mod)
  )

  update_model_id(new_mod)
  expect_equal(
    readr::read_lines(get_model_path(new_mod)),
    paste0(get_model_id(new_mod), DEFAULT_SUFFIXES)
  )
})

test_that("update_model_id() works with character ID [BBR-CMH-002]", {
  on.exit({ cleanup() })

  # copy model and write strings with parent id to control stream
  parent_mod <- copy_model_from(MOD1, "Parent")
  new_mod <- copy_model_from(parent_mod, "Child")
  readr::write_lines(
    paste0(get_model_id(parent_mod), DEFAULT_SUFFIXES),
    get_model_path(new_mod)
  )

  update_model_id(new_mod)
  expect_equal(
    readr::read_lines(get_model_path(new_mod)),
    paste0(get_model_id(new_mod), DEFAULT_SUFFIXES)
  )
})

test_that("update_model_id() is case-sensitive [BBR-CMH-003]", {
  on.exit({ cleanup() })
  suff_all_case <- c(toupper(DEFAULT_SUFFIXES), tolower(DEFAULT_SUFFIXES))

  # copy model and write strings with parent id to control stream
  new_mod <- copy_model_from(MOD1, basename(NEW_MOD2))
  readr::write_lines(
    paste0(get_model_id(MOD1), suff_all_case),
    get_model_path(new_mod)
  )

  update_model_id(new_mod)
  expect_equal(
    readr::read_lines(get_model_path(new_mod)),
    paste0(get_model_id(new_mod), suff_all_case)
  )
})

test_that("update_model_id() .suffixes works [BBR-CMH-004]", {
  on.exit({ cleanup() })

  # copy model and write strings with parent id to control stream
  new_mod <- copy_model_from(MOD1, basename(NEW_MOD2))
  orig_suff <- paste0(get_model_id(MOD1), DEFAULT_SUFFIXES)

  readr::write_lines(
    orig_suff,
    get_model_path(new_mod)
  )

  update_model_id(new_mod, .suffixes = DEFAULT_SUFFIXES[1])
  expect_equal(
    readr::read_lines(get_model_path(new_mod)),
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

  readr::write_lines(
    paste0(get_model_id(MOD1), c(DEFAULT_SUFFIXES, new_suff)),
    get_model_path(new_mod)
  )

  update_model_id(new_mod, .additional_suffixes = new_suff)
  expect_equal(
    readr::read_lines(get_model_path(new_mod)),
    paste0(get_model_id(new_mod), c(DEFAULT_SUFFIXES, new_suff))
  )
})

test_that("update_model_id() errors with no based_on [BBR-CMH-006]", {
  expect_error(
    update_model_id(MOD1),
    regexp = "based_on is empty"
  )
})
