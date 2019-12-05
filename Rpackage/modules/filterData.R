###########################################
##          Filter Parsed Data           ##
###########################################

# David LV Bauer
# 8 August 2019


filterData_UI <- function (id) {
  ns = NS(id)
  bs4TabItem(tabName = "filterData",
             column(8,align="center",
                    bs4Card(
                      title = "Parsed Data",
                      width = 12,
                      status = "primary",
                      solidHeader = FALSE,
                      closable = FALSE,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      textOutput(ns("previewParsedData"))
                    ),
                    icon("arrow-alt-circle-down"),
                    bs4Card(
                      title = "Select Segments & Strand",
                      width = 12,
                      status = "",
                      solidHeader = FALSE,
                      closable = FALSE,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      column(8, align="left",
                        tags$b("Select segments below to include"),
                        DT::dataTableOutput(ns("filterBySegment"))
                      ),
                      column(4, align="center",
                        checkboxInput(ns("filterByStrand"), label="Filter by indicated strand", value=TRUE),
                        checkboxInput(ns("filterSameSeg"), label="Include same-segment interactions", value=FALSE),
                        tags$hr(),
                        textOutput(ns("previewAfterSegStrandFilter")),
                        p("Selected segments:"),
                        textOutput(ns("previewSelectedSegs"))
                      )
                    ),
                    icon("arrow-alt-circle-down"),
                    bs4Card(
                      title = "Remove PCR Duplicates",
                      width = 12,
                      status = "",
                      solidHeader = FALSE,
                      closable = FALSE,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      radioButtons(ns("filterPCRduplicates"), label = "PCR duplicate filter", choiceNames = c("None", "Classic", "Ignore CIGAR strings (uses start/end positions only)", "Strict (ignores CIGAR and end2)"),
                                                                                              choiceValues = c("", "dC", "dI", "dS"),
                                                                                              selected = "dC"),
                      textOutput(ns("previewAfterPCRduplicates"))
                      
                    ),
                    icon("arrow-alt-circle-down"),
                    bs4Card(
                      title = "Filtered Data",
                      width = 12,
                      status = "success",
                      solidHeader = FALSE,
                      closable = FALSE,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      DT::dataTableOutput(ns("previewFilteredTable"))
                    ),
                    icon("arrow-alt-circle-down"),
                    p(),
                    downloadButton(ns("downloadFilteredTable"), label = "Download Table"),
                    checkboxInput(ns("downloadFilteredTable_classic"), label = "Classic splishSPLASH v1 format", value=FALSE )
                    
                    
             )
  )
}

filterData <- function(input, output, session, refGenome, parsedData){
  
  # Set up reactive values table
  f <- reactiveValues( filteredTable = NULL,
                       selectedSegmentNames = NULL,
                       filterSegmentTable = NULL,
                       numPassingAfterStrandSeg = NULL,
                       numPassingAfterPCRduplicates = NULL,
                       pctPCRduplicates = NULL)
 
  ####### !!!!!!!!!! ######
  ### This seems to be a source of slowness in other modules... remove reactive and refer to actual data strucutre itself??
  parsedTable <- reactive({
    parsedData()$data
  })
  
  # render preview of parsed data
  output$previewParsedData <- renderText( paste( format(nrow(parsedTable()),big.mark = ","), " reads in parsed table", sep = "") )
  
  observe({
    f$filterSegmentTable <- refGenome()$data$refGenomeDF[,c("segment","strandFilter")]
  })
  
  # Show filter by segment table
  output$filterBySegment <-  DT::renderDataTable(f$filterSegmentTable, 
                                           class='compact',
                                           selection = list('multiple' 
                                                            # ,selected=(1:nrow(f$filterSegmentTable))
                                                            ),
                                           autoHideNavigation = TRUE,
                                           editable = FALSE,
                                           options = list(
                                             dom = 'ti',
                                             paging = FALSE)
  )
  
  
  
  
  
  ## ACTUAL TABLE FILTERING
  # Update selected table based on user selections
  observe({
    f$selectedSegmentNames <- f$filterSegmentTable[input$filterBySegment_rows_selected,"segment"]
    filtBySegment <- parsedTable()[which((parsedTable()$chr1 %in% f$selectedSegmentNames) & (parsedTable()$chr2 %in% f$selectedSegmentNames)),]
    req(filtBySegment)
    req(f$filterSegmentTable)
    
    if (input$filterByStrand == TRUE){
      desiredStrand1 <- dplyr::inner_join(filtBySegment, f$filterSegmentTable, by = c("chr1" = "segment"))
      names(desiredStrand1)[names(desiredStrand1) == 'strandFilter'] <- 'strandFilter1'
      desiredStrand2 <- dplyr::inner_join(desiredStrand1, f$filterSegmentTable, by = c("chr2" = "segment"))
      names(desiredStrand2)[names(desiredStrand2) == 'strandFilter'] <- 'strandFilter2'
      
      desiredStrand2$passStrand1Filt <- desiredStrand2$strand1 == desiredStrand2$strandFilter1
      desiredStrand2$passStrand1Filt[which(desiredStrand2$strandFilter1 == "+/-")] <- TRUE
      desiredStrand2$passStrand2Filt <- desiredStrand2$strand2 == desiredStrand2$strandFilter2
      desiredStrand2$passStrand2Filt[which(desiredStrand2$strandFilter2 == "+/-")] <- TRUE
      filtBySegment <- desiredStrand2[which(desiredStrand2$passStrand1Filt & desiredStrand2$passStrand2Filt),]
      filtBySegment <- select(filtBySegment, -strandFilter1, -strandFilter2, -passStrand1Filt, -passStrand2Filt)
    }
    
    if (input$filterSameSeg == FALSE){
      filtBySegment$failSameFilt <- as.character(filtBySegment$chr1) == as.character(filtBySegment$chr2)
      filtBySegment <- filtBySegment[which(filtBySegment$failSameFilt == FALSE),]
      filtBySegment <- select(filtBySegment, -failSameFilt)
    }
    
    f$numPassingAfterStrandSeg <- nrow(filtBySegment)
    
    if (isTruthy(input$filterPCRduplicates)){
      filtBySegment <- filtBySegment[!grepl(input$filterPCRduplicates, filtBySegment$flags),]
    }
    f$numPassingAfterPCRduplicates <- nrow(filtBySegment)
    f$pctPCRduplicates <- 100*(f$numPassingAfterStrandSeg - f$numPassingAfterPCRduplicates) / f$numPassingAfterStrandSeg
    f$filteredTable <- filtBySegment
  })
  
  # Calculate reads passing filter
  # countAfterSegStrandFilter <- reactive({
  #   sum( nrow(f$filteredTable) )
  # })
  
  # Give info about which rows were removed
  output$previewAfterSegStrandFilter <- renderText(paste(length(input$filterBySegment_rows_selected), " rows selected. ", format(f$numPassingAfterStrandSeg, big.mark = ","), " reads selected." ))
  output$previewSelectedSegs <- renderText(paste(f$selectedSegmentNames, collapse=", ") )
  output$previewAfterPCRduplicates <- renderText(paste(format(f$numPassingAfterPCRduplicates, big.mark = ","), " reads after deduplication. (",format(f$pctPCRduplicates, digits=2),"% duplicated)", sep=""))  
  
  # Preview filtered table
  output$previewFilteredTable <- DT::renderDataTable(f$filteredTable, class='compact', filter = 'top', options=list(buttons = I('colvis')))
  
  # Download filtered table
  output$downloadFilteredTable <- downloadHandler(
    filename = function() {
      paste('filteredReadTable.txt', sep='')
    },
    content = function(con) {
      withProgress(message="Gathering data", detail="... this may take a while", {
        
        tableToWrite <- f$filteredTable
        
        if(input$downloadFilteredTable_classic == TRUE){
          # c("readName", "CIGAR1", "CIGAR2", "flags", "chr1", "strand1", "start1", "end1", "chr2", "strand2", "start2", "end2")
          tableToWrite <- dplyr::select(tableToWrite, chr1, start1, end1, chr2, start2, end2)
          colnames(tableToWrite) <- c("Parent", "StartParent", "EndParent", "Child", "StartChild", "EndChild")
        }
        
        fwrite(tableToWrite, con, showProgress=TRUE, row.names=FALSE, col.names=TRUE, eol="\n", sep="\t", quote=FALSE)
      })
      showNotification("Finished saving file!", type = "message")
    }
  )
  
  
  
  ### End of main functions
  # Function to run to restore data when uploading project
  updateDataFn = function(new_Data) {
    # leave empty -- want filtering to re-run automatically?
  }
  # Return function and data as reactive list
  return(reactive({
    list(updateData = updateDataFn, data = f$filteredTable, selectedSegmentNames = f$selectedSegmentNames)
  }))
  
  
}
  
  