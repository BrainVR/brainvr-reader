#'
#'
#'

launch_application <- function()
{
  shiny::runApp(appDir = system.file("application", package = "brainvr.R"))
}