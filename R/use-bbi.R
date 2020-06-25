#' @title Installs most current release of bbi
#' @description Identifies system running and pulls the relevant tarball for the current release of babylon from GitHub,
#' and then installs it in the directory passed to `.dir`.
#' If used in an interactive session, will open an installation menu confirming the installed version.
#' @importFrom glue glue glue_collapse
#' @importFrom cli rule
#' @param .dir directory to install bbi to on linux
#' @param .version version of babylon to install. Must pass a character scalar corresponding to a tag found in `https://github.com/metrumresearchgroup/babylon/releases`
#' @param .force Boolean for whether to force the installation even if current version and local version are the same. Primarily used for testing.
#' @param .quiet Boolean for suppressing output printed to the console. False by default.
#' @return character
#' @export
use_bbi <- function(.dir = "/data/apps", .version = "latest", .force = FALSE, .quiet = FALSE){

  os <- c('linux','darwin','mingw')

  header <- glue::glue(
    'Installing bbi on a {cli::col_red(R.version$os)} system',
    cli::rule(),.sep = '\n')

  footer <- ' '

  this_os <- os[sapply(os,grepl,x=R.version$os)]

  if(.version == "latest") {
    .bbi_url <- current_release(owner = 'metrumresearchgroup', repo = 'babylon', os = this_os)
  } else {
    if (this_os == "linux") {
      .bbi_url <- as.character(glue("https://github.com/metrumresearchgroup/babylon/releases/download/{.version}/bbi_linux_amd64.tar.gz"))
    } else {
      warning(glue("User passed `.version = {.version}` and but this argument is only valid on Linux and current OS is {this_os}. Latest version of bbi will be installed."))
    }
  }

  body <-  switch(this_os,
                  'darwin'  = {
                    c('brew tap metrumresearchgroup/homebrew-tap',
                      'brew install bbi')

                  },
                  'linux' = {
                    linux_install_commands(.dir = .dir, .bbi_url = .bbi_url)
                  },
                  {
                    c('browse to: https://github.com/metrumresearchgroup/babylon#getting-started')
                  })

  glue_this <- c(header,body,footer)

  on.exit(install_menu(body, this_os, .dir, .version, .force, .quiet), add = TRUE)

  if(isFALSE(.quiet)) print(glue::glue_collapse(glue_this, sep = '\n'))

}

#' Private helper function to most recent release version from repo
#' @importFrom stringr str_detect
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @importFrom utils download.file
#' @param owner Repository owner/organization
#' @param repo Repository name
#' @param os Operating system user intends to install on
current_release <- function(owner = 'metrumresearchgroup', repo = 'babylon', os = c('linux','darwin','windows')){

  tmp <- tempfile(fileext = '.json')

  on.exit(unlink(tmp),add = TRUE)

  tryCatch(
    {
      download.file(glue('https://api.github.com/repos/{owner}/{repo}/releases/latest'), destfile = tmp, quiet = TRUE)
    },
    error = function(e) {
      if (str_detect(e$message, "HTTP error 403")) {
        stop(glue('`current_release({owner}, {repo})` failed, possibly because this IP is over the public Github rate limit of 60/hour.'))
      }
    }
  )

  release_info <- jsonlite::fromJSON(tmp, simplifyDataFrame = FALSE)

  uris <- grep('gz$',sapply(release_info$assets,function(x) x$browser_download_url),value = TRUE)
  uris <- uris[grepl(x = uris, pattern = "amd64", fixed = TRUE)]
  names(uris) <- sapply(strsplit(gsub('_amd64.tar.gz$','',basename(uris)),'_'),'[[',2)

  uris[os]

}


#' @title Get version number of babylon current release
#' @description Helper function to get version number of most recent release of babylon from GitHub.
#' @param os operating system, Default: 'linux'
#' @importFrom stringr str_replace
#' @rdname bbi_current_release
#' @export
bbi_current_release <- function(os = "linux"){
  str_replace(basename(dirname(current_release(owner = 'metrumresearchgroup', repo = 'babylon', os = os))), '^v', '')
}

#' Private implementation function for installing bbi with interactive menu
#' @param .body Character vector of installation commands to run with `system`
#' @param .this_os Character scalar of OS
#' @param .dir directory to install bbi to
#' @param .version version of babylon to install. Must pass a character scalar corresponding to a tag found in `https://github.com/metrumresearchgroup/babylon/releases`
#' @param .force Boolean for whether to force the installation even if current version and local version are the same
#' @param .quiet Boolean for suppressing output printed to the console. False by default.
install_menu <- function(.body, .this_os, .dir, .version, .force, .quiet){

  .dest_bbi_path <- normalizePath(file.path(.dir, "bbi"), mustWork = FALSE)
  local_v <- bbi_version(.dest_bbi_path)

  if (.version == 'latest') {
    release_v <- bbi_current_release()
  } else {
    release_v <- .version
  }

  if(!identical(release_v,local_v) || isTRUE(.force)){

    if(.this_os%in%c('linux','darwin')){

      # suppressing interactivity will allow for suppression in unit tests. This may be more general to name
      # like specifiying its a test environment, but a user could also want to generally suppress interactivity for
      # automatted build pipelines etc.
      if (interactive() && is.null(getOption('rbabylon.suppress_interactivity'))) {
        version_message(local_v = local_v, release_v = release_v)

        print(glue::glue(cli::rule(left = cli::col_red('Do you want to install version {release_v}?'),line = 2)))

        if(utils::menu(choices = c('Yes','No'))==1){
          map(.body, ~ system(.x, ignore.stdout = .quiet, ignore.stderr = .quiet))
          local_v <- bbi_version(.dest_bbi_path)
        }
      } else {
        map(.body, ~ system(.x, ignore.stdout = .quiet, ignore.stderr = .quiet))
      }

    }

  }

  version_message(local_v = local_v, release_v = release_v)
}


#' Private helper function for building commands to install bbi on Linux
#' @param .dir Directory to install into
#' @param .bbi_url Full url to download tarball from
linux_install_commands <- function(.dir, .bbi_url) {

  bbi_loc <- normalizePath(file.path(.dir, "bbi"), mustWork = FALSE)

  cmd_vec <- c(
    glue::glue('wget {.bbi_url} -O /tmp/bbi.tar.gz  --timeout=15 --tries=2'),
    'tar -xzf /tmp/bbi.tar.gz -C /tmp --overwrite',
    glue::glue('mkdir -p {.dir}'),
    glue::glue('mv /tmp/bbi_linux_amd64/bbi {bbi_loc}'),
    glue::glue('chmod +x {bbi_loc}')
  )

  return(cmd_vec)
}


#' @title Get version number of babylon installed on system
#' @description Returns version number of bbi binary installed at path passed to `.bbi_exe_path`
#' @importFrom stringr str_detect str_replace_all
#' @param .bbi_exe_path Path to bbi exe file that will be checked
#' @return character
#' @examples
#' \dontrun{
#' bbi_version()
#' }
#' @rdname bbi_version
#' @export
bbi_version <- function(.bbi_exe_path = getOption('rbabylon.bbi_exe_path')){
  bbi_path <- Sys.which(.bbi_exe_path)
  if (is.null(bbi_path) || bbi_path == "") {
    return("")
  }
  if (!fs::file_exists(bbi_path)) {
    return("")
  }

  tryCatch(
    {
      res <- system(sprintf('%s version', bbi_path),intern = TRUE)
      return(str_replace_all(res, '^v', ''))
    },
    error = function(e) {
      if (str_detect(e$message, "error in running command")) {
        stop(glue("The executable at {bbi_path} does not appear to be a valid babylon installation. Use `use_bbi({dirname(bbi_path)})` to install babylon at that location."))
      }
      stop(e$message)
    }
  )
}

#' Private helper to construct version comparison message
#' @importFrom cli rule col_blue col_red
#' @importFrom glue glue
#' @param local_v Character scalar for version number on local installation
#' @param release_v Character scalar for version number of current release
version_message <- function(local_v, release_v){

  if (local_v == "") {
    cat(cli::col_red("No version currently installed "))
  } else{
    cat(glue::glue(cli::col_blue(' - Installed Version: {local_v} ')))
    if (!identical(release_v,local_v)) {
      cat(cli::col_red(" (Not Current Release) "))
    }
  }

  cat(glue::glue(cli::col_blue(' - Current release: {release_v}\n')))
}
