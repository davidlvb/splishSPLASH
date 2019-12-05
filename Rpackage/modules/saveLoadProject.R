###########################################
##        Save Project Function          ##
###########################################

# David LV Bauer
# 6 August 2019


saveLoadProject_UI <- function (id, variableList) {
  ns = NS(id)
  
  bs4TabCard(id=ns("saveLoadProject"), title = "Project", status = "success", elevation = NULL,
             solidHeader = TRUE, headerBorder = TRUE, gradientColor = NULL,
             tabStatus = NULL, width = 6, height = NULL, collapsible = TRUE,
             collapsed = FALSE, closable = FALSE, maximizable = FALSE, side = "right",
             bs4TabPanel(tabName=list(icon("folder-open"),"Load"), active = TRUE,
                         helpText("Upload a project file from a previous splishSPLASH session"),
                         fileInput(ns("uploadProject"), label=NULL)
             ),
             bs4TabPanel(tabName=list(icon("save"),"Save"), active = FALSE,
                         helpText("Save current splishSPLASH session as a project file",
                                  "for later use"),
                         downloadButton(ns("downloadProject"),label = "Download Project")
             )
  )
}


saveLoadProject <- function(input, output, session, projectData){
  
  p <- reactiveValues( importedData = NULL,
                       trigger = 0)
  
  ## Download Project
  output$downloadProject <- downloadHandler(
    filename = function() {
      paste('splishSplashProj.rds', sep='')
    },
    content = function(con) {
      withProgress(message="Gathering data", detail="... this may take a while", {
        # dataToWrite <- projectData
        # print("###################")
        # print(str(dataToWrite))
        # print("###################")
        # print(as.matrix(lapply(dataToWrite, function(x) length(serialize(x,NULL)))))
        # print("###################")
        #write("text", con)
        saveRDS(projectData, con, compress = TRUE)
      })
      showNotification("Finished saving file!", type = "message")
    }
  )
  
  
  ## Upload Project
  observeEvent(input$uploadProject,{
    chimericTable <- tryCatch(
      {
        withProgress(value = 0.2, message="Importing data... ", detail="please wait",{
          importedData <- readRDS(input$uploadProject$datapath)
        })
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        showModal(modalDialog(
          title = "Cannot Read Project File",
          paste("Please check you have selected the correct file ...", safeError(e), sep="")
        ))
        return(NULL)
      }
    )
    req(importedData)
    
    if (!is.list(importedData)){
      showModal(modalDialog(
        title = "Unexpected format",
        paste("Please check you have selected the correct file ...", safeError(e), sep="")
      ))
      importedData <- NULL
    }
    req(importedData)
    
    p$importedData <- c(importedData, projFileName=input$uploadProject$name)

  })
  
  return(reactive({p$importedData}))

}