library(tictoc)
library(data.table)
library(R.utils)
library(shiny)
library(bs4Dash)
library(DT)     # for rendering DataTables
library(Biostrings)  # for reading / writing FASTA files
library(tidyverse)  # for datatable manipulation
library(RColorBrewer)
library(colourpicker)
library(GenomicAlignments)
library(plotly)
library(Rcpp)
library(viridis)
library(imager)


# Rcpp::sourceCpp('modules/generateInteractionMatrix.cpp')
# Rcpp::sourceCpp('modules/findLocalMaxima.cpp')



source("config/colourPresets.R")
source("modules/refGenome.r")
source("modules/importData.r")
source("modules/saveLoadProject.R")
source("modules/filterData.R")
source("modules/viewMatrix.R")




bs4DashPage(
  old_school = FALSE,
  sidebar_collapsed = FALSE,
  controlbar_collapsed = TRUE,
  title = "splishSPLASH2",
  navbar = bs4DashNavbar(h6(textOutput("projectName", inline=TRUE))),
  sidebar= bs4DashSidebar(
      skin = "light",
      status = "primary",
      title = "splishSPLASH v2",
      src = "splishSPLASH_logo.png",
      brandColor = "",
      elevation = 4,
      opacity = 0.8,
      bs4SidebarMenu(
        bs4SidebarMenuItem(
          "Home",
          tabName = "home",
          icon = "home"
        ),
        bs4SidebarHeader("Project"),
        bs4SidebarMenuItem("Setup",
          tabName = "setup",
          icon = "cog",
          bs4SidebarMenuSubItem("Reference Genome", 
                                tabName="referenceGenome",
                                icon="dna"),
          bs4SidebarMenuSubItem("Import Data", 
                                tabName="importData",
                                icon="file-import"),
          bs4SidebarMenuSubItem("Filter Data", 
                                tabName="filterData",
                                icon="filter")
        
            
        ),
        bs4SidebarMenuItem("Process Data",
                           tabName = "processData",
                           icon = "braille",
                           bs4SidebarMenuSubItem("Set Threshold", 
                                                 tabName="thresholdData",
                                                 icon="sliders-h"),
                           bs4SidebarMenuSubItem("Auto-Find Loci", 
                                                 tabName="autoFindLoci",
                                                 icon="magic"),
                           bs4SidebarMenuSubItem("Review Loci", 
                                                 tabName="reviewLoci",
                                                 icon="search-location"),
                           bs4SidebarMenuSubItem("Loci Table", 
                                                 tabName="lociTable",
                                                 icon="table")
                          ),
        bs4SidebarMenuItem("Analyse Data",
                           tabName = "analyseData",
                           icon = "diagnoses",
                           bs4SidebarMenuSubItem("Plot Circos", 
                                                 tabName="plotCircos",
                                                 icon="circle-notch"),
                           bs4SidebarMenuSubItem("RNA structures", 
                                                 tabName="RNAstructures",
                                                 icon="dna"),
                           bs4SidebarMenuSubItem("Summary Statistics", 
                                                 tabName="summaryStatistics",
                                                 icon="chart-bar")
                           
                          )
                           
      )
    )
  ,
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(tabName = "home",
                 bs4Card(title = tags$h1("splishSPLASH v2"), status = "primary", solidHeader = TRUE, gradientColor = "primary", collapsible = FALSE, closable = FALSE,
                         tags$p("Analyse RNA:RNA interactome data with ease")),
                 #### CALL saveLoadProject MODULE UI inside home tab ###
                 saveLoadProject_UI("main")
      ),
      #### CALL refGenome MODULE UI ###
      refGenome_UI("main")
      ,
      #### CALL importData MODULE UI ###
      importData_UI("main")
      ,
      ### Call filter data
      filterData_UI("main")
      ,
      ### Call view matrix / threshold data
      viewMatrix_UI("main")
      ,
      findLoci_UI("main")
      
    )
  )
)
