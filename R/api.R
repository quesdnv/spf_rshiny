
#' Verifies a Soccerlab issued token
#'
#'
#' @param token `string` The token to verify
#' @param settings `list` The calculated settings object
#'
#' @return a `list` object containing authenticated session and user profile
#' @importFrom httr GET
#' @importFrom httr content
verifyToken <- function(token,settings) {

  printVerbose(sprintf("Soccerlab verifying token: %s",token))
  checkTokenUrl <- paste(settings$validateTokenUrl, token, sep="?token=")
  r <- httr::GET(checkTokenUrl)

  status <- r$status_code
  success <- status==200

  if(success) {
    printVerbose(sprintf("VERIFIED Soccerlab token %s ",token))
    result <- httr::content(r, "parsed")
    sessionId <- result$SessionId
    printVerbose(sprintf("SessionId %s ",sessionId))
    printVerbose("Retrieving user profile")
    profile <- getApiCall(settings$getProfileUrl,sessionId)

    if(!is.null(profile)) {

      isAuth <- list(success=success,sessionId=sessionId,profile=profile$Response)

      return(isAuth)

    } else {
      printVerbose("Failed to retrieve user profile")
      return(NULL)
    }

  } else {
    return(NULL)
  }




}

validateAuthorisation  <- function(user,settings) {
  if(is.null(user)) {
    return(FALSE)
  } else {
    if(!is.null(settings$roles)) {

      printVerbose(sprintf("Validating user has one of %s roles",settings$roles))
      printVerbose(sprintf("User roles: %s",user$profile$UserTypeRoles))

      if(any(settings$roles %in% tolower(user$profile$UserTypeRoles))) {
        printVerbose("User has 1 or more allowed roles")
        return(TRUE)
      } else {
        printVerbose("User does not have any allowed role")
        return(FALSE)
      }
    } else {
      return(TRUE)
    }
  }
}

#' Call as Soccerlab api with credentials (sessionId)
#'
#'
#' @param url `string` The api url to reach
#' @param sessionId `string` The sessionId (available in session$userData$sessionId post login)
#' @param raw `bool` defaults to FALSE (TRUE = non parsed json)
#'
#' @export
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr config
getApiCall <- function(url,sessionId,raw=FALSE) {
  # httr package will send cookies url encoded, to override this behavior, we'll use manual config
  printVerbose(sprintf("Soccerlab api request %s",url))
  r <- httr::GET(url, httr::config(cookie = paste0("ss-id=", sessionId)))


  status <- r$status_code
  printVerbose(sprintf("%s status: %s",url,toString(status)))
  result = NULL
  if(raw) {
    result <- httr::content(r, as="text")
    printVerbose(sprintf("%s RAW result",url))
  } else {
    result <- httr::content(r, as="parsed")
    printVerbose(sprintf("%s parsed result",url))
  }

  printVerbose(result)

  if(status==200) {
    return(result)
  } else {
    return(NULL)
  }

}



