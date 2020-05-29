#' @title Installing bbi
#' @description Identifies system running and return installation instructions
#' @return character
#' @param .dir directory to install bbi to on linux
#' @export
#' @importFrom glue glue glue_collapse
#' @importFrom cli rule
use_bbi <- function(.dir = "/data/apps"){

  bbi_loc <- normalizePath(file.path(.dir, "bbi"), mustWork = FALSE)
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
                    c(glue::glue('wget {current_release(os = "linux")} -O /tmp/bbi.tar.gz'),
                      'tar -xzf /tmp/bbi.tar.gz -C /tmp --overwrite',
                      glue::glue('mkdir -p {.dir}'),
                      glue::glue('mv /tmp/bbi_linux_amd64/bbi {bbi_loc}'),
                      glue::glue('chmod +x {bbi_loc}'))
                  },
                  {
                    c('browse to: https://github.com/metrumresearchgroup/babylon#getting-started')
                  })

  glue_this <- c(header,body,footer)

  on.exit(install_menu(body,this_os),add = TRUE)

  print(glue::glue_collapse(glue_this,sep = '\n'))

}

#' @importFrom jsonlite read_json
current_release <- function(owner = 'metrumresearchgroup', repo = 'babylon', os = c('linux','darwin','windows')){

  tf <- tempfile(fileext = '.json')

  on.exit(unlink(tf),add = TRUE)

  release_info <- jsonlite::fromJSON(glue::glue('https://api.github.com/repos/{owner}/{repo}/releases/latest'), simplifyDataFrame = FALSE)


  uris <- grep('gz$',sapply(release_info$assets,function(x) x$browser_download_url),value = TRUE)
  uris <- uris[grepl(x = uris, pattern = "amd64", fixed = TRUE)]
  names(uris) <- sapply(strsplit(gsub('_amd64.tar.gz$','',basename(uris)),'_'),'[[',2)

  uris[os]

}


#' @title current release
#' @param os operating system, Default: 'linux'
#' @rdname bbi_current_release
#' @export
bbi_current_release <- function(os = "linux"){
  gsub('^v','',basename(dirname(current_release(os = os))))
}

install_menu <- function(body,this_os){

  release_v <- bbi_current_release()
  local_v <- bbi_version()

  if(!identical(release_v,local_v)){

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

  version_message(local_v = local_v,release_v = release_v)

}

#' @title bbi version
#' @description Returns string of installed bbi cli version
#' @importFrom stringr str_detect
#' @return character
#' @examples
#' \dontrun{
#' bbi_version()
#' }
#' @rdname bbi_version
#' @export
bbi_version <- function(){
  bbi_path <- getOption('rbabylon.bbi_exe_path', Sys.which('bbi'))
  if (is.null(bbi_path) || bbi_path == "") {
    return("")
  }
  if (!fs::file_exists(bbi_path)) {
    return("")
  }

  tryCatch(
    system(sprintf('%s version', bbi_path),intern = TRUE),
    error = function(e) {
      if (str_detect(e$message, "error in running command")) {
        stop(glue("The executable at {bbi_path} does not appear to be a valid babylon installation. Use `use_bbi({bbi_path})` to install babylon at that location."))
      }
      stop(e$message)
    }
  )
}

#' @importFrom cli rule col_blue
#' @importFrom glue glue
version_message <- function(local_v = bbi_version(), release_v = bbi_current_release()){

  flag <- ifelse(identical(release_v,local_v),"Current Release","Not Current Release")

  cat(glue::glue(cli::col_blue('- Current release: {release_v}\n')))
  cat(glue::glue(cli::col_blue('- Installed Version: {local_v}')))
}
