#' @title Installing pkgr
#' @description Identifies system running and return installation instructions
#' @return character
#' @examples
#' use_pkgr()
#' @rdname use_pkgr
#' @export
#' @importFrom glue glue glue_collapse
#' @importFrom cli rule
use_pkgr <- function(){

  os <- c('linux','darwin','mingw')

  header <- glue::glue(
    'Installing pkgr on a {cli::col_red(R.version$os)} system',
    cli::rule(),.sep = '\n')

  footer <- ' '

  this_os <- os[sapply(os,grepl,x=R.version$os)]

  body <-  switch(this_os,
                  'darwin'  = {
                    c('brew tap metrumresearchgroup/homebrew-tap',
                      'brew install metrumresearchgroup/tap/pkgr')

                  },
                  'linux' = {
                    c(glue::glue('sudo wget {current_release(os = "linux")} -O /tmp/pkgr.tar.gz'),
                      'sudo tar xzf /tmp/pkgr.tar.gz pkgr',
                      'sudo mv pkgr /usr/local/bin/pkgr',
                      'sudo chmod +x /usr/local/bin/pkgr')
                  },
                  {
                    c('browse to: https://github.com/metrumresearchgroup/pkgr#getting-started')
                  })

  glue_this <- c(header,body,footer)

  on.exit(install_menu(body,this_os),add = TRUE)

  print(glue::glue_collapse(glue_this,sep = '\n'))

}

#' @importFrom curl curl_download
#' @importFrom jsonlite read_json
current_release <- function(owner = 'metrumresearchgroup', repo = 'pkgr', os = c('linux','darwin','windows')){

  tf <- tempfile(fileext = '.json')

  on.exit(unlink(tf),add = TRUE)

  release_json <- curl::curl_download(glue::glue('https://api.github.com/repos/{owner}/{repo}/releases/latest'),destfile = tf,quiet = TRUE)

  release_info <- jsonlite::read_json(release_json)

  uris <- grep('gz$',sapply(release_info$assets,function(x) x$browser_download_url),value = TRUE)

  names(uris) <- sapply(strsplit(gsub('_amd64.tar.gz$','',basename(uris)),'_'),'[[',3)

  uris[os]

}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param os PARAM_DESCRIPTION, Default: 'linux'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pkgr.current_release
#' @export
pkgr.current_release <- function(os = "linux"){
  gsub('^v','',basename(dirname(current_release(os = os))))
}

install_menu <- function(body,this_os){

  release_v <- pkgr.current_release()
  local_v <- pkgr.version()

  if(!identical(release_v,local_v)){

    if(interactive()&this_os%in%c('linux','darwin')){

      print(glue::glue(cli::rule(left = cli::col_red('{local_v} is currently installed. Update to the {release_v}?'),line = 2)))

      if(utils::menu(choices = c('Yes','No'))==1){
        system(paste(body,collapse =' ; '))
        local_v <- pkgr.version()
      }

    }

  }

  print(version_message(local_v = local_v,release_v = release_v))

}

#' @importFrom cli rule col_blue
#' @importFrom glue glue
version_message <- function(local_v = pkgr.version(), release_v = pkgr.current_release()){

  flag <- ifelse(identical(release_v,local_v),"Current Release","Not Current Release")

  glue::glue(cli::rule(left = cli::col_blue('pkgr Version: {local_v} ({flag}) is Installed'),line = 2))

}
