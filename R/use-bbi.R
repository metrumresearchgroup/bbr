#' @title Installing bbi
#' @description Identifies system running and return installation instructions
#' @importFrom glue glue glue_collapse
#' @importFrom cli rule
#' @param .dir directory to install bbi to on linux
#' @param .force Boolean for whether to force the installation even if current version and local version are the same
#' @return character
#' @export
use_bbi <- function(.dir = "/data/apps", .force = FALSE){

  os <- c('linux','darwin','mingw')

  header <- glue::glue(
    'Installing bbi on a {cli::col_red(R.version$os)} system',
    cli::rule(),.sep = '\n')

  footer <- ' '

  this_os <- os[sapply(os,grepl,x=R.version$os)]

  body <-  switch(this_os,
                  'darwin'  = {
                    c('brew tap metrumresearchgroup/homebrew-tap',
                      'brew install bbi')

                  },
                  'linux' = {
                    linux_install_commands(.dir = .dir, .bbi_url = current_release(os = "linux"))
                  },
                  {
                    c('browse to: https://github.com/metrumresearchgroup/babylon#getting-started')
                  })

  glue_this <- c(header,body,footer)

  on.exit(install_menu(body,this_os, force=.force),add = TRUE)

  print(glue::glue_collapse(glue_this,sep = '\n'))

}

#' Private helper function to most recent release version from repo
#' @importFrom stringr str_detect
#' @importFrom jsonlite read_json
#' @importFrom glue glue
#' @keywords internal
current_release <- function(owner = 'metrumresearchgroup', repo = 'babylon', os = c('linux','darwin','windows')){

  tf <- tempfile(fileext = '.json')

  on.exit(unlink(tf),add = TRUE)

  release_info <- tryCatch(
    {
      jsonlite::fromJSON(glue('https://api.github.com/repos/{owner}/{repo}/releases/latest'), simplifyDataFrame = FALSE)
    },
    error = function(e) {
      if (str_detect(e$message, "HTTP error 403")) {
        stop(glue('`current_release({owner}, {repo})` failed, possibly because this IP is over the public Github rate limit of 60/hour.'))
      }
    }
  )

  uris <- grep('gz$',sapply(release_info$assets,function(x) x$browser_download_url),value = TRUE)
  uris <- uris[grepl(x = uris, pattern = "amd64", fixed = TRUE)]
  names(uris) <- sapply(strsplit(gsub('_amd64.tar.gz$','',basename(uris)),'_'),'[[',2)

  uris[os]

}


#' @title current release
#' @description Helper function to get version number of most recent release of babylon from GitHub.
#' @param os operating system, Default: 'linux'
#' @importFrom stringr str_replace
#' @rdname bbi_current_release
#' @export
bbi_current_release <- function(os = "linux"){
  str_replace(basename(dirname(current_release(os = os))), '^v', '')
}

#' Private implementation function for installing bbi with interactive menu
#' @param body Character vector of installation commands to run with `system`
#' @param this_os Character scaler of OS
#' @param force Boolean for whether to force the installation even if current version and local version are the same
#' @keywords internal
install_menu <- function(body, this_os, force){

  release_v <- bbi_current_release()
  local_v <- bbi_version()

  if(!identical(release_v,local_v) || isTRUE(force)){

    if(this_os%in%c('linux','darwin')){

      # suppressing interactivity will allow for suppression in unit tests. This may be more general to name
      # like specifiying its a test environment, but a user could also want to generally suppress interactivity for
      # automatted build pipelines etc.
      if (interactive() && is.null(getOption('rbabylon.suppress_interactivity'))) {
        print(glue::glue(cli::rule(left = cli::col_red('{local_v} is currently installed. Update to the {release_v}?'),line = 2)))

        if(utils::menu(choices = c('Yes','No'))==1){
          system(paste(body,collapse =' ; '))
          local_v <- bbi_version()
        }
      } else {
          system(paste(body,collapse =' ; '))
      }

    }

  }

  version_message(local_v = local_v, release_v = release_v)

}


#' Private helper function for building commands to install bbi on Linux
#' @param .dir Directory to install into
#' @param .bbi_url Full url to download tarball from
#' @keywords internal
linux_install_commands <- function(.dir, .bbi_url) {

  bbi_loc <- normalizePath(file.path(.dir, "bbi"), mustWork = FALSE)

  cmd_vec <- c(
    glue::glue('wget {.bbi_url} -O /tmp/bbi.tar.gz'),
    'tar -xzf /tmp/bbi.tar.gz -C /tmp --overwrite',
    glue::glue('mkdir -p {.dir}'),
    glue::glue('mv /tmp/bbi_linux_amd64/bbi {bbi_loc}'),
    glue::glue('chmod +x {bbi_loc}')
  )

  return(cmd_vec)
}


#' @title bbi version
#' @description Returns string of installed bbi cli version
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
#' @importFrom cli rule col_blue
#' @importFrom glue glue
#' @keywords internal
version_message <- function(local_v = bbi_version(), release_v = bbi_current_release()){

  flag <- ifelse(identical(release_v,local_v),"Current Release","Not Current Release")

  cat(glue::glue(cli::col_blue('- Current release: {release_v}\n')))
  cat(glue::glue(cli::col_blue('- Installed Version: {local_v}')))
}
