library(tictoc)
library(data.table)
library(R.utils)
library(shiny)
library(bs4Dash)
library(DT)     # for rendering DataTables
library(Biostrings)  # for reading / writing FASTA files
library(tidyverse)  # for datatable manipulation
library(RColorBrewer)
library(GenomicAlignments)
library(colourpicker)
library(plotly)
library(Rcpp)
library(viridis)
library(imager)
Rcpp::sourceCpp('modules/generateInteractionMatrix.cpp')
Rcpp::sourceCpp('modules/findLocalMaxima.cpp')

setProgress <- shiny::setProgress

options(shiny.maxRequestSize=1000*1024^2)


shinyServer(function(input, output) {
  
  ### Main Program modules
  refGenomeMain <- callModule(refGenome, "main")
  importDataMain <- callModule(importData, "main")
  filterDataMain <- callModule(filterData, "main", refGenome = refGenomeMain, 
                                                   parsedData = importDataMain)
  viewMatrixMain <- callModule(viewMatrix, "main", refGenome = refGenomeMain,
                                                   parsedData = filterDataMain)
  
  
  
  ### IMPORT / EXPORT OF ENTIRE PROJECT
  ### Set up function to tell modules to update their data when restored:
  # see: https://stackoverflow.com/questions/53777215/shiny-modules-accessing-and-changing-reactivevalues-situated-inside-modules-se
  syncData <- function(session, new_Data, Module){
    Module()$updateData(new_Data)
  }
  ### Import/Export project Module
  projectLoaded <- callModule(saveLoadProject, "main", projectData = list ( refGenome = refGenomeMain()$data,
                                                                           importedData = as.data.frame(importDataMain()$data) )  )
  ### Tell modules to update their data when project is loaded
  # note that the structure is set by the projectData list structure above
  observeEvent(projectLoaded(), {
    req(projectLoaded())
    syncData(session, projectLoaded()$refGenome, refGenomeMain)
    syncData(session, projectLoaded()$importedData, importDataMain)
  })
  # Put project name in navbar:
  output$projectName <- renderText(projectLoaded()$projFileName)
  


  
  
})
