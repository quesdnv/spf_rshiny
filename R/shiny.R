pkg.globals <- new.env()

pkg.globals$verboseEnabled <- FALSE


printVerbose <- function(txt, ...) {
  if(pkg.globals$verboseEnabled) {
    if(class(txt) == "character") {
      print(sprintf("shinySlab::%s",txt), ...)
    } else {
      print(txt, ...)
    }

  }
}

#' Create a Shiny app object with Soccerlab Authentication
#'
#' This function modifies ui and server objects to run using Soccerlab
#' authentication.
#'
#' @param ui an ordinary UI object to create shiny apps.
#' @param server an ordinary server object to create shiny apps.
#' @param config_file path to JSON configuration file.
#' @param roles atomic vector (character) of allowed roles, default NULL (= allow all).
#' @param ... Other arguments as passed on to [shiny::shinyApp()].
#'
#' @details
#' You can also use a different configuration file by setting the
#' `slab_config_file` option with:
#' `options(slab_config_file = "path/to/your_file.json")`.
#'
#' @section Disable Soccerlab while developing apps:
#'
#' In order to develop without using SoccerLab authentication,
#' one can run options(slab_disable = TRUE) to
#'   disable soccerlab temporarily.
#'
#' @export
#' @import shiny
shinySlabApp <- function(ui, server, config_file = NULL,roles=NULL, verboseFlag=getOption("slab_verbose"),  ...) {
  disable <- getOption("slab_disable")
  pkg.globals$verboseEnabled <- verboseFlag

  if (!is.null(disable) && disable) {
    warning('shinySlab:: Soccerlab Authentication package is disabled: to enable run options(slab_disable = NULL)')
    shiny::shinyApp(ui, server)
  } else {
    if (is.null(config_file)) {
      config_file <- slab_find_config_file()
    }
    if(is.null(config_file)) {
      stop("shinySlab:: config file is missing. Provide soccerlab.json config file or run use_soccerlab() ")
    }
    printVerbose("calculating settings from config")
    settings <- getSettings(config_file,roles)
    printVerbose("start of shinyApp")
    shiny::shinyApp(slab_ui(settings), slab_server(server,settings,ui), ...)

  }

}


#' Injects ui objects to facilitate in token based authentication with Soccerlab.
#'
#' This function is not intended to be used by end user.
#'
#' @param settings `list` object to holding settings details
#'
#'
#' @import shinyjs
slab_ui <- function(settings) {

  return(
    shiny::tagList(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = settings$jsCode, functions = c("getSlabToken")),
    if(pkg.globals$verboseEnabled) shiny::p("Soccerlab Verbose is enabled.") else NULL,
    if(pkg.globals$verboseEnabled) shiny::verbatimTextOutput('slabLoginState') else NULL,
    shiny::uiOutput('slabUnauth'),
    shiny::uiOutput('slabUserUi')
  ))
  # uiOutput: Only render user UI when authenticated

}

#' @rdname ui-server
#'
#' @param server the shiny server function.
#' @param settings calculated settings object based on configuration json.
#' @param ui `shiny.tag.list` user defined object/function to generate the user interface.
#'
slab_server <- function(server,settings,ui) {

  function(input, output, session) {

    status <- shiny::reactiveVal(value = NULL)
    statusTxt <- shiny::reactiveVal(value = "Logging in")



    shiny::observe({
      state <- status()
      if(is.null(state)) {
        printVerbose("Unauthenticated, start authentication")
        status("TOKEN")
        statusTxt("Authorizing")
        js$getSlabToken()
      }
    })

    shiny::observeEvent(input$slabToken, {
      printVerbose(paste("Soccerlab token received",input$slabToken,sep = ":"))
      if (!is.null(input$slabToken)) {
        printVerbose("verifying Soccerlab token")
        user <- verifyToken(input$slabToken,settings)
        if(!is.null(user)) {
          printVerbose("successfully validated Soccerlab token")
          printVerbose(user)
          isAuthorised <- validateAuthorisation(user,settings)
          if(isAuthorised) {
            status('IN')

            session$userData = user
            statusTxt(paste0(' as ', session$userData$profile$FullName))
          } else {
            status('UNAUTH')

            session$userData = user
            statusTxt(paste0(session$userData$profile$FullName, ' is unauthorised.'))
          }

        }
        else {
          printVerbose("failed to validate Soccerlab token")
          status('OUT')
          statusTxt('Invalid token')
        }
      }
      else {
        printVerbose("logged out")
        status('OUT')
        statusTxt('Logged out')
      }
    })


    output$slabLoginState <- shiny::renderText({
      paste0('LOGIN:: ', statusTxt())
    }
    )

    output$slabUnauth <- shiny::renderUI({

      req(status()=="UNAUTH")
      shiny::absolutePanel(shiny::p("Not Authorised.") , fixed = TRUE
                           ,draggable = FALSE, top = 10, left = 0,right=0
                           ,width = 100, height = "auto",style="
              font-weight: bold;
              line-height: 1.1;
              background: #FFF;
              color: #CC0000;
              border: 2px solid #cc0000;
              padding: 2em;
              width: 80%;
              text-align: center;
              margin: auto;")
    })

    output$slabUserUi <- shiny::renderUI({
      # use req to only render results when credentials()$user_auth is TRUE
      printVerbose(paste("User UI render state",status(),sep = ":"))
      req(status()=="IN")
      ui
    })

    server(input, output, session)
  }

}


