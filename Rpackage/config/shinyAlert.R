

library(shiny)
library(shinyalert)

ui <- fluidPage(
  useShinyalert()
)

server <- function(input, output) {
  shinyalert(
    title = "Data Already Loaded",
    text = "Do you want to replace the data, or append to the existing data?",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "warning",
    showConfirmButton = TRUE,
    showCancelButton = TRUE,
    confirmButtonText = "Replace",
    confirmButtonCol = "#FAC800",
    cancelButtonText = "Append",
    timer = 0,
    imageUrl = "",
    animation = TRUE,
    callbackR = function(x) { if(x != FALSE) message("Hello ", x) }
  )
}

shinyApp(ui, server)

