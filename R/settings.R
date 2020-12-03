.onLoad <- function(libname, pkgname) {
  op <- options()
  op.slab <- list(
    slab_config_file = NULL,
    slab_disable = NULL,
    slab_verbose = FALSE,
    slab_local = interactive()
  )
  toset <- !(names(op.slab) %in% names(op))
  if(any(toset)) options(op.slab[toset])
  invisible()
}

#' Find the configuration file.
#'
#' Tries to find the path to the `soccerlab.json` file. First, it tries to get
#'   this info from `options(slab_config_file = )`. If this option is `NULL`
#'   (the default) it tries to find the `soccerlab.json` within the working
#'   directory. If the file does not exist, it raises an error.
#'
#' @return Character vector of length one contaning the path of the
#'   `soccerlab.json` file.
#'
#' @seealso [`use_soccerlab`].
#'
#' @export
#' @import jsonlite
slab_find_config_file <- function() {

  config_file <- getOption("slab_config_file")

  if (is.null(config_file) || !file.exists(config_file)) {
    config_file <- "./soccerlab.json"
  }

  if (!file.exists(config_file)) {
    stop(
      "Didn't find the  soccerlab.json configuration file. ",
      "There are two possible explanations:\n",
      "1. You didn't create an soccerlab.json file. Solution: Run `use_soccerlab()`\n",
      "2. You created an soccerlab.json file, but it was not found.\n",
      "You have two options:\n",
      "  Solution 2a): set the path for the soccerlab.json ",
      "file running `options(slab_config_file = \"/path/to/soccerlab.json\")`. ",
      "Always use absolute path, because shiny::runApp() modifies ",
      "the working directory.\n",
      "  Solution 2b): If your app.R file is in the same directory as the ",
      "soccerlab.json file, set the working directory to the folder ",
      "where soccerlab.json file is located."
    )
  }

  config_file
}

#' Soccerlab configuration file
#'
#' Generates a minimal config file for soccerlab
#'
#' @param path Directory name. Should be the root of the shiny app
#' @param file File name. Defaults to `soccerlab.json`.
#' @param overwrite Will only overwrite existing path if `TRUE`.
#'
#'
#' @export
use_soccerlab <- function(path = ".", file = "soccerlab.json", overwrite = FALSE) {
  f <- paste0(normalizePath(path), "/", file)
  if (file.exists(f) && !overwrite) {
    stop("File exists and overwrite is FALSE.")
  }

  json_list <- list(
    domain = "yourdomain.soccerlab.com")
  exportJSON <- jsonlite::toJSON(json_list)
  write(exportJSON, "soccerlab.json")
}

jsCodeTemplate <- '
  shinyjs.getSlabToken = function() {
   fetch("%s",{credentials:"include",cache: "no-cache",
  redirect: "error"})
   .then((r)=>r.text())
   .then((token)=>{

    Shiny.onInputChange("slabToken", token);
   }).catch((e)=>{
   Shiny.onInputChange("slabToken", null);
   location.replace("%s"+location.href)
   })
  }
'

tokenPath <- "/APIRest/v0.2/auth/token"
validateTokenPath <- "/APIRest/v0.2/auth/tokenauth"
myProfilePath <- "/APIRest/v0.2/system/authentication/user_accounts/my_user_profile?serializebaseproperties=false"


#' Calculates the settings based on the config file
#'
#'
#' @param config_file `string` Path to the config file
#' @param roles `atomic vector (character)` Optional atomic (character) vector of allowed roles (UUID's)
#'
#' @return a `list` object representing the calculated settings based on config file
getSettings <- function(config_file,roles=NULL) {

  settings <- jsonlite::fromJSON(config_file)

  if(!is.null(roles) && class(roles)=="character") {
    settings$roles <- tolower(roles)
  } else if(!is.null(settings$roles) && class(settings$roles)=="character") {
    settings$roles <- tolower(settings$roles)
  } else {
    settings$roles <- NULL
  }

  settings$getTokenUrl <- sprintf("https://%s%s",settings$domain,tokenPath)
  settings$validateTokenUrl <- sprintf("https://%s%s",settings$domain,validateTokenPath)
  settings$getProfileUrl <- sprintf("https://%s%s",settings$domain,myProfilePath)
  settings$redirectUrl <- sprintf("https://%s/soccerlab/webapps/auth/#/login/",settings$domain)
  settings$jsCode <- sprintf(jsCodeTemplate,settings$getTokenUrl,settings$redirectUrl)

  printVerbose(' -- Soccerlab SETTINGS --')
  printVerbose(settings)

  return(settings)
}

# Get rid of NOTE
globalVariables(c("tokenPath","validateTokenPath","myProfilePath","js","req"))
