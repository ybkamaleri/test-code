## For testing with Azure
testUI <- function(id){
  # create the namespace from the id
  ns <- NS(id)
  
  tagList(
    actionButton(ns("show"), "Hvis dialogboks"),
    verbatimTextOutput(ns("dataInfo"))
  )
}

testServer <- function(id){
  moduleServer(id, function(input, output, session){
    # reactiveValues object for storing current data set.
    vals <- reactiveValues(data = NULL)

    # Return the UI for a modal dialog with data selection input. If 'failed' is
    # TRUE, then display a message that the previous value was invalid.
    dataModal <- function(failed = FALSE) {
      
      # load the namespace
      ns <- session$ns

      # build the modal
      modalDialog(

        # make sure to wrap all id tokens in ns()
        textInput(ns("dataset"), "Valg datasett",
                  placeholder = 'F.eks "mtcars" eller "abc"'),

        span('(Valg en valide objekt som "mtcars", ',
             'deretter et objekt som ikke finnes f.eks "abc")'),

        if (failed)
          div(tags$b("Invalid name of data object", style = "color: red;")),

        footer = tagList(
          modalButton("Avbryt"),
          actionButton(ns("ok"), "OK") # wrapped in ns()
        )

      )
    }

    # Show modal when button is clicked.
    observeEvent(input$show, {
      showModal(dataModal())
    })

    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$ok, {
      
      print(input$dataset)
      
      # Check that data object exists and is data frame.
      if (!is.null(input$dataset) && nzchar(input$dataset) &&
            exists(input$dataset) && is.data.frame(get(input$dataset))) {
        vals$data <- get(input$dataset)
        removeModal()
      } else {
        showModal(dataModal(failed = TRUE))
      }
    })

    # Display information about selected data
    output$dataInfo <- renderPrint({
      if (is.null(vals$data))
        "Ingen data er valgt"
      else
        summary(vals$data)
    })

  }  

  )
}


testApp <- function(){
  ui <- fluidPage(
    testUI("test01")
  )

  server <- function(input, output, session){
    testServer("test01")
  }

  shinyApp(ui, server)
}

testApp()
