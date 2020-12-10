# Soccerlab R Shiny Authentication Package #

R Package adding SoccerLab Authentication to your Shiny App.


### Installing the package ###

To install the package run `devtools::install_github("quesdnv/spf_rshiny")` in the command window.

### Features ###
* Seamlessly adds Soccerlab authentication to your Shiny App
* The ability to limit allowed SoccerLab roles 
* Provides logged on details in session$userData (success,sessionId,profile)
* Provides logged on user profile info in session$userData$profile
* Provides a convenience function to converse with Soccerlab api's

### Using the package ###
Before you can use the SoccerLab package, it is required to have a config file "soccerlab.json" available in the directory of your Shiny App.
The configuration file can also be generated for you by running the commmand `shinySlab::use_soccerlab()` and replace with your domain.

Adding Soccerlab authentication to your Shiny App is as easy as:
* replacing "shinyApp()" with "shinySlab::shinySlabApp()"

Optionally with additional properties of "shinySlabApp"
* config_file: Path to the json config file
* verboseFlag: boolean value. If TRUE, verbose information will be added to console and ui.
* roles: atomic vector of characters listing allowed user type roles (guid) c("roleId","..",...)


### console features ###
* options(slab_verbose = TRUE/FALSE) True will print verbose information in console and add verbose information in UI
* options(slab_disable = TRUE/FALSE) True to disable Soccerlab authentication (for example for local testing)



