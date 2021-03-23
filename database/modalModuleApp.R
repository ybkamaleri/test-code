source("modalModule.R")

#  Modularised version of: https://shiny.rstudio.com/reference/shiny/0.14/modalDialog.html
#  Created using instructions at: https://stackoverflow.com/a/48127806/2925434

# get the module UI from the sourced file
ui = basicPage(
  moduleUI("main")
)

# call the module with the same id ("main") as above
server = function(input, output) {
  callModule(module, "main")
}

# run the app
shinyApp(ui, server)
