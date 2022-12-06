#' @title Installs most current release of bbi
#' @description Identifies system running and pulls the relevant tarball for the
#'   current release of bbi from GitHub, and then installs it at `.path` (see
#'   Details section for defaults). If used in an interactive session, will open
#'   an installation menu confirming the installed version. This function will
#'   print information about the installed version. This **printing can be
#'   suppressed** by setting `options(bbr.verbose = FALSE)`.
#'
#' @details If nothing is passed to the `.path` argument, `use_bbi()` will
#' attempt to find a valid path for installation. The following decision
#' waterfall is used:
#'
#' * First, check `getOption("bbr.bbi_exe_path")`. If this is anything _other
#' than_ `"bbi"` (the default value) then attempt to install to that path.
#'
#' * Second, check `Sys.which("bbi")` which will look for a `bbi` installation
#' in the user's `$PATH`. If one is found, ask the user if they wish to
#' overwrite it and, if they confirm, install to that path.
#'
#' * Third, attempt to install to a "default location" and add this location to
#' the user's `$PATH`. If `Sys.getenv("XDG_DATA_HOME")` is found, install to
#' `{Sys.getenv("XDG_DATA_HOME")}/bbi/bbi` (per [XGD
#' specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)).
#' Otherwise, install to OS dependent defaults:
#'
#'   * Linux: `{Sys.getenv("HOME")}/.local/share/bbi/bbi`
#'
#'   * Mac: `/usr/local/bin/bbi`
#'
#'   * Windows: `{Sys.getenv("APPDATA")}\bbi\bbi`
#'
#' If none of these are successful, the user will be prompted to set
#' `options("bbr.bbi_exe_path")` and try again.
#'
#' @importFrom glue glue_collapse
#' @importFrom cli rule
#'
#' @param .path absolute path to install bbi to. See Details section for
#'   defaults, if nothing is passed. Note that this should be the path where you
#'   would like the bbi executable to be installed, _not_ the path to the
#'   directory in which you want to install it. For example, you should pass
#'   `"/some/dir/bbi"` and _not_ `"/some/dir"`.
#' @param .version version of bbi to install. Must pass a character scalar
#'   corresponding to a tag found in
#'   `https://github.com/metrumresearchgroup/bbi/releases`
#' @param .force If `FALSE`, the default, skips installation if requested
#'   version and local version are the same. If `TRUE` forces installation if it
#'   will be the same version.
#' @param .quiet **Deprecated.** Use `options("bbr.verbose")` instead to control
#'   printing. Defaults to `NULL`, which reads `!getOption("bbr.verbose")`. If
#'   `TRUE`, suppresses output printed to the console.
#' @return character
#' @export
use_bbi <- function(.path = NULL, .version = "latest", .force = FALSE, .quiet = NULL){

  if (!is.null(.quiet)) {
    deprecate_warn("1.5.0", "use_bbi(.quiet)", details = "Please set `options('bbr.verbose' = FALSE)` instead.")
    checkmate::assert_logical(.quiet, len = 1)
  } else {
    .quiet <- !getOption("bbr.verbose")
  }

  header <- glue::glue('Installing bbi on a {check_os()} system',
                       cli::rule(), .sep = '\n')

  if (is.null(.path)) {
    .path <- build_bbi_install_path()
  }

  if (fs::is_dir(.path)) {
    err_msg <- glue(
      "{.path} is an existing directory. `use_bbi(.path)` needs a path the destination for the `bbi` executable _file_.",
      "Did you mean to install to `{file.path(.path, 'bbi')}`?", .sep = "\n"
    )
    if(.path == getOption("bbr.bbi_exe_path")) {
      err_msg <- paste(err_msg, "Make this change wherever you are setting `options('bbr.bbi_exe_path')`, potentially in your .Rprofile")
    }
    stop(err_msg)
  }

  dir_create(dirname(.path))

  on.exit(install_menu(.path, .version, .force, .quiet), add = TRUE)

  if(isFALSE(.quiet)) print(header)

}

#' Private helper function to most recent release version from repo
#' @importFrom stringr str_detect
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @importFrom utils download.file
#' @param owner Repository owner/organization
#' @param repo Repository name
#' @keywords internal
current_release_url <- function(owner = 'metrumresearchgroup', repo = 'bbi'){

  os <- check_os()

  tmp <- tempfile(fileext = '.json')

  on.exit(unlink(tmp),add = TRUE)

  tryCatch(
    {
      download.file(glue('https://api.github.com/repos/{owner}/{repo}/releases/latest'), destfile = tmp, quiet = TRUE)
    },
    error = function(e) {
      if (str_detect(e$message, "HTTP error 403")) {
        stop(glue('`current_release_url({owner}, {repo})` failed, possibly because this IP is over the public Github rate limit of 60/hour.'))
      }
    }
  )

  release_info <- jsonlite::fromJSON(tmp, simplifyDataFrame = FALSE)

  uris <- grep('gz$',sapply(release_info$assets,function(x) x$browser_download_url),value = TRUE)
  uris <- uris[grepl(x = uris, pattern = "amd64", fixed = TRUE)]
  names(uris) <- sapply(strsplit(gsub('_amd64.tar.gz$','',basename(uris)),'_'),'[[',2)

  uris[os]

}


#' @title Get version number of bbi current release
#' @description Helper function to get version number of most recent release of bbi from GitHub.
#' @param .bbi_url (Optional) URL for a bbi release artifact to strip version
#'   number out of. If `NULL`, the default, will fetch the URL with
#'   `current_release_url()`.
#' @importFrom stringr str_replace
#' @export
bbi_current_release <- function(.bbi_url = NULL){
  checkmate::assert_string(.bbi_url, null.ok = TRUE)
  if (is.null(.bbi_url)) {
    .bbi_url <- current_release_url(owner = 'metrumresearchgroup', repo = 'bbi')
  }
  str_replace(basename(dirname(.bbi_url)), '^v', '')
}

#' Private implementation function for installing bbi with interactive menu
#' @inheritParams use_bbi
#' @keywords internal
install_menu <- function(.path, .version, .force, .quiet){

  .dest_bbi_path <- normalizePath(.path, mustWork = FALSE)
  local_v <- bbi_version(.dest_bbi_path)

  current_bbi_url <- current_release_url(owner = 'metrumresearchgroup', repo = 'bbi')
  current_v <- bbi_current_release(current_bbi_url)

  if (.version == 'latest') {
    .bbi_url <- current_bbi_url
    requested_v <- current_v
  } else {
    .bbi_url <- as.character(glue("https://github.com/metrumresearchgroup/bbi/releases/download/{.version}/bbi_{check_os()}_amd64.tar.gz"))
    requested_v <- .version
  }

  if(!identical(requested_v, local_v) || isTRUE(.force)){

    # suppressing interactivity will allow for suppression in unit tests. This may be more general to name
    # like specifiying its a test environment, but a user could also want to generally suppress interactivity for
    # automatted build pipelines etc.
    if (interactive() && is.null(getOption('bbr.suppress_interactivity'))) {
      if (!isTRUE(.quiet)) version_message(local_v = local_v, current_v = current_v)

      print(glue::glue(cli::rule(left = cli::col_red('Do you want to install version {requested_v} at {.dest_bbi_path}?'),line = 2)))

      if(utils::menu(choices = c('Yes','No'))==1){
        download_bbi(.bbi_url, .dest_bbi_path)
      }
    } else {
      download_bbi(.bbi_url, .dest_bbi_path)
    }
    local_v <- bbi_version(.dest_bbi_path)
  }

  add_to_path_message(.dest_bbi_path)
  if (!isTRUE(.quiet)) version_message(local_v = local_v, current_v = current_v)
}


#' Build default path to install bbi to
#' @keywords internal
build_bbi_install_path <- function() {
  os <- check_os()

  # first check options
  bbi_path <- getOption("bbr.bbi_exe_path")
  if (!is.null(bbi_path) && bbi_path != BBI_DEFAULT_PATH) {
    message(glue("Found options('bbr.bbi_exe_path' = '{bbi_path}')"))
    return(bbi_path)
  }

  # next check which bbi to see if in PATH already
  bbi_path <- Sys.which(BBI_DEFAULT_PATH)
  if (bbi_path != "") {
    message(glue("Found '{bbi_path}' in $PATH with Sys.which('{BBI_DEFAULT_PATH}')"))
    return(bbi_path)
  }

  # next look for XDG dir
  xdg_path_home <- Sys.getenv("XDG_DATA_HOME")
  if (xdg_path_home != "") {
    message(glue("Found '{xdg_path_home}' in $XDG_DATA_HOME')"))
    return(file.path(xdg_path_home, "bbi", "bbi"))
  }

  # finally try for a default path
  bbi_path <- switch(
    os,
    "linux" = {
      home_dir <- Sys.getenv("HOME")
      if (home_dir == "") dev_error("build_bbi_install_path() can't find $HOME")
      file.path(home_dir, ".local", "share", "bbi", "bbi")
    },
    "darwin" = {
      home_dir <- Sys.getenv("HOME")
      if (home_dir == "") dev_error("build_bbi_install_path() can't find $HOME")
      "/usr/local/bin/bbi"
    },
    "windows" = {
      app_dir <- Sys.getenv("APPDATA")
      if (app_dir == "") dev_error("build_bbi_install_path() can't find $APPDATA")
      file.path(app_dir, "bbi", "bbi")
    },
    {
      dev_error(glue("build_bbi_install_path() got invalid operating system: {os}"))
    }
  )

  message(glue("Attempting to install bbi to default path: {bbi_path}"))
  return(bbi_path)
}

#' Download bbi executable to the specified path.
#'
#' @param .bbi_url URL pointing to the tar.gz for a particular release
#' @param .path the final destination of bbi executable
#'
#' @importFrom glue glue
#' @keywords internal
download_bbi <- function(.bbi_url, .path){
  checkmate::assert_string(.path)
  if (!fs::is_absolute_path(.path)) {
    dev_error(glue("Must pass an absolute path but got: `{.path}`"))
  }
  tmpdir <- tempfile()
  dir.create(tmpdir)
  orig_wd <- setwd(dir = tmpdir)
  on.exit(setwd(orig_wd), add = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  download_target <- file.path(tmpdir, "bbi.tar.gz")
  be_verbose <- isTRUE(getOption("bbr.verbose"))

  rc <- download_with_retry(
    .bbi_url, destfile = download_target, mode = "wb",
    quiet = isFALSE(be_verbose))
  if (rc != 0) {
    stop(glue("Non-zero exit ({rc}) for download of {.bbi_url}"))
  }

  utils::untar(tarfile = download_target, verbose = be_verbose)

  # The extracted directory should have only one directory...
  d_extracted <-  list.dirs(tmpdir, recursive = FALSE)
  checkmate::assert_string(d_extracted, min.chars = 1)
  # ... with one executable inside.
  exe <- list.files(d_extracted, full.names = TRUE)
  checkmate::assert_string(exe, min.chars = 1)

  Sys.chmod(exe, mode = "0755", use_umask = FALSE)
  if (be_verbose) {
    message(glue("Copying bbi to '{.path}'"))
  }
  file.copy(exe, .path, overwrite = TRUE)

  return(NULL)
}

#' @title Get version of installed bbi
#'
#' @importFrom stringr str_detect str_replace_all str_trim
#' @param .bbi_exe_path Path to bbi executable
#' @return String giving the version of the bbi binary installed at `.bbi_exe_path`
#' @examples
#' \dontrun{
#' bbi_version()
#' }
#' @export
bbi_version <- function(.bbi_exe_path = getOption('bbr.bbi_exe_path')){
  bbi_path <- Sys.which(.bbi_exe_path)
  if (is.null(bbi_path) || bbi_path == "") {
    return("")
  }
  if (!fs::file_exists(bbi_path)) {
    return("")
  }

  tryCatch(
    {
      res <- processx::run(.bbi_exe_path, "version", error_on_status = TRUE)
      return(str_replace_all(str_trim(res$stdout, side = "right"), '^v', ''))
    },
    error = function(e) {
      if (str_detect(e$message, "error in running command")) {
        stop(glue("The executable at {bbi_path} does not appear to be a valid bbi installation. Use `use_bbi(.path = {bbi_path})` to install bbi at that location."))
      }
      stop(e$message)
    }
  )
}


#' Private helper to construct version comparison message
#' @importFrom cli rule col_blue col_red
#' @importFrom glue glue
#' @param local_v Character scalar for version number on local installation
#' @param current_v Character scalar for version number of current release
#' @keywords internal
version_message <- function(local_v, current_v){
  if (local_v == "") {
    cat(cli::col_red("No version currently installed "))
  } else{
    cat(glue::glue(cli::col_blue(' - Installed Version: {local_v} ')))
    if (!identical(current_v,local_v)) {
      cat(cli::col_red(" (Not Current Release) "))
    }
  }

  cat(glue::glue(cli::col_blue(' - Current release: {current_v}\n')))
}

#' Helper to message user about adding the bbi directory to $PATH
#'
#' Will return invisibly if .bbi_path is the same as `getOption('bbr.bbi_exe_path')`
#'  or if `dirname(.bbi_path)` is already in `$PATH`
#' @param .bbi_path absolute path to the new bbi
#' @importFrom cli cli_alert
#' @keywords internal
add_to_path_message <- function(.bbi_path) {
  if (.bbi_path == getOption('bbr.bbi_exe_path')) {
    return(invisible(NULL))
  }

  old_path <- Sys.getenv("PATH")

  if (check_os() == "windows") {
    .sep = ";"
  } else {
    .sep = ":"
  }

  old_path_dirs <- unlist(str_split(old_path, .sep))

  new_dir <- dirname(.bbi_path)
  if (new_dir %in% old_path_dirs) {
    return(invisible(NULL))
  }

  cli::cli_alert(glue("Please either set `options('bbr.bbi_exe_path' = '{.bbi_path}')` in your .Rprofile, or add this location to $PATH in your .bash_profile"))
}


