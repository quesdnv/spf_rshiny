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
#' @param verboseFlag TRUE/FALSE will show all debug information (prints) AND verbose UI.
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
#' @importFrom shiny shinyApp
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

# getSession <- function() {
#   session <- shiny::getDefaultReactiveDomain()
#
#   if (is.null(session)) {
#     errMsg(paste(
#       "could not find the Shiny session object. This usually happens when a"
#     ))
#   }
#
#   session
# }

#' Setup SLab JS requirements
#'
#' This function is not intended to be used by end user.
#'
#' @param settings `list` object to holding settings details
#'
#' @importFrom shiny singleton
#' @importFrom shiny tags
#' @importFrom shiny HTML
setupJS <- function(settings) {
  jsContent <-
    shiny::singleton(
      shiny::tags$head(
        # add the message handlers
        shiny::tags$script(shiny::HTML(settings$jsCode))
      )
    )

  jsContent

}


#' Injects ui objects to facilitate in token based authentication with Soccerlab.
#'
#' This function is not intended to be used by end user.
#'
#' @param settings `list` object to holding settings details
#'
#'
#'
#' @importFrom shiny tagList
#' @importFrom shiny p
#' @importFrom shiny verbatimTextOutput
slab_ui <- function(settings) {
  return(
    shiny::tagList(setupJS(settings),
    if(pkg.globals$verboseEnabled) shiny::p("Soccerlab Verbose is enabled.") else NULL,
    if(pkg.globals$verboseEnabled) shiny::verbatimTextOutput('slabLoginState') else NULL,
    uiOutput("slabAuth")
  ))
}

slab_ui_success <- function(ui) {
   return (shiny::tagList(ui,
   tags$script('var evt = new CustomEvent("shinySlab:connected");
    document.dispatchEvent(evt);')))
}

#' send message to UI to fetch token.
#'
#' This function is not intended to be used by end user.
#'
#' @param session Rshiny session
getSlabToken <- function(session) {
  session$sendCustomMessage(
    type = "slab",
    message = list(
      action = "AUTH"
    ))
}

#' send message to UI to redirect
#'
#' This function is not intended to be used by end user.
#'
#' @param session Rshiny session
sendRedir <- function(session,url) {
  session$sendCustomMessage(
    type = "slab",
    message = list(
      action = "REDIR",
      url=url
    ))
}

#' @param session Rshiny session
signalOk <- function(session) {
  session$sendCustomMessage(
    type = "slab",
    message = list(
      action = "VALID"
    ))
}

isTRUE <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x) && x
}

#' @rdname ui-server
#'
#' @name slab_server
#'
#' @param server the shiny server function.
#' @param settings calculated settings object based on configuration json.
#' @param ui `shiny.tag.list` user defined object/function to generate the user interface.
#'
#' @importFrom shiny reactiveVal
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny renderText
#' @importFrom shiny renderUI
#' @importFrom shiny absolutePanel
#' @importFrom shiny p
#' @importFrom shiny insertUI
slab_server <- function(server,settings,ui) {

  function(input, output, session) {

    status <- shiny::reactiveVal(value = NULL)
    statusTxt <- shiny::reactiveVal(value = "Logging in")

    shiny::observe({
      state <- status()
      printVerbose(state)
      if(!shiny::isTruthy(state)) {
        status("AUTH_GET_TOKEN")
      } else if(state=="AUTH_GET_TOKEN") {
        if(shiny::isTruthy(input$slabToken)) {
          printVerbose("EXISTING TOKEN")
          status("AUTH_VERIFY_TOKEN")
        } else {
          printVerbose("GET TOKEN")
          statusTxt("Authorizing")
          getSlabToken(session)
        }
      } else if(state=="AUTH_VERIFY_TOKEN") {
        printVerbose("VERIFY TOKEN")
        user <- verifyToken(input$slabToken,settings)
        if(!is.null(user)) {
          printVerbose("successfully validated Soccerlab token")
          isAuthorised <- validateAuthorisation(user,settings)
          if(isAuthorised) {
            session$userData = user
            statusTxt(paste0(' as ', session$userData$profile$FullName))
            status('AUTH_SUCCESS')
          } else {
            state("AUTH_FAILED")
          }
        } else {
          state("AUTH_FAILED")
        }

      } else if(state=="AUTH_SUCCESS") {
        print("Success, renderUI")
        #shiny::insertUI(selector = "body",
         #                     #where = "afterEnd",
        #                      ui=ui,immediate = TRUE)
        #uiExtra<-ui,
         output$slabAuth <- renderUI(slab_ui_success(ui))
         #signalOk(session)
         server(input, output, session)
      } else if(state=="AUTH_FAILED" ||state=="AUTH_GET_TOKEN_FAILED" ) {
        failedUi <- shiny::absolutePanel(shiny::p("Not Authorised.") , fixed = TRUE
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

        output$slabAuth <- renderUI(failedUi)
        #shiny::insertUI(selector = "body",
        #                       #where = "afterEnd",
        #                       ui=failedUi,immediate = TRUE)

      }
    })

    shiny::observeEvent(input$slabToken, {
         printVerbose(paste("Soccerlab token received",input$slabToken,sep = ":"))
         if (!is.null(input$slabToken)) {
           if(input$slabToken=="FAILED") {
             status("AUTH_GET_TOKEN_FAILED")
           } else {
             status("AUTH_VERIFY_TOKEN")
           }
         } else {
           status("AUTH_GET_TOKEN_FAILED")
         }
    })

  }

}


