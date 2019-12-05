###########################################
##   Import Data Functions from STAR     ##
###########################################

# David LV Bauer
# 26 July 2019


importData_UI <- function (id) {
  ns = NS(id)
  bs4TabItem(tabName = "importData",
             fluidRow(column(4,
                             bs4TabCard(id=ns("importData"), title = "Import Data Files", status = NULL, elevation = NULL,
                                        solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL,
                                        tabStatus = NULL, width = 12, height = NULL, collapsible = TRUE,
                                        collapsed = FALSE, closable = FALSE, maximizable = FALSE, side = "right",
                                        bs4TabPanel(tabName=list(icon("star"),"Chimeric"), active = TRUE,
                                                    helpText("Upload Chimeric.out.junction files"),
                                                    fileInput(ns("importDataChimeric"), label=NULL)
                                        ),
                                        bs4TabPanel(tabName=list("CIGAR", icon("star")), active = FALSE,
                                                    helpText("Upload a table of SAM file CIGAR strings",
                                                             "for intra-segment chimeras."),
                                                    fileInput(ns("importDataCIGAR"), label=NULL)
                                        ),
                                        bs4TabPanel(tabName=icon("cogs"), active = FALSE,
                                                    # Input: Checkbox if file has header ----
                                                    checkboxInput(ns("header"), "File has header", FALSE),
                                                    
                                                    # Input: Select separator ----
                                                    selectInput(ns("sep"), "Field Separator",
                                                                choices = c(Tab = "\t",
                                                                            Space = " ",
                                                                            Comma = ",",
                                                                            Semicolon = ";"),
                                                                selected = "\t",
                                                                selectize=FALSE)
                                        )
                                        
                             ),
                             bs4TabCard(id=ns("importDataParsing"), title = "Parsing", status = NULL, elevation = NULL,
                                        solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL,
                                        tabStatus = NULL, width = 12, height = NULL, collapsible = TRUE,
                                        collapsed = FALSE, closable = FALSE, maximizable = FALSE, side = "right",
                                        bs4TabPanel(tabName=icon("cogs"), active = TRUE,
                                                    column(12, align="center",actionButton(ns("parseChimeras"), "Parse Data", icon=icon("calculator"))),
                                                    tags$br(),
                                                    checkboxInput(ns("shortenReadIDs"), "Shorten Read IDs (removes run info)", TRUE),
                                                    numericInput(ns("maxCIGARspan"), "Maximum CIGAR span", min=1, value=130),
                                                    radioButtons(ns("duplicateReadPref"), label = "Resolve duplicates by using", choices = c("Chimeric" = "chimeric", "Same Segment" = "sameSeg", "Exclude" = "exclude")),
                                                    tags$hr(),
                                                    checkboxInput(ns("renameChrFindRep"), "Find-Replace Segment Names", FALSE),
                                                    helpText("Enter find and replace text separated by a comma (no spaces).",
                                                             "e.g. to rename 'NS1' to 'NS', enter: 'NS1,NS'. You can enter",
                                                             "multiple pairs on separate lines."),
                                                    textAreaInput(ns("renameChrFindRepList"), label=NULL, rows=4)
                                        ))
             ),column(8,
                      fluidRow(column(12, align="center",
                        bs4ValueBoxOutput(ns("statNumChimeric"), width = 12),
                        bs4ValueBoxOutput(ns("statNumSameCIGAR"), width = 12),
                        bs4ValueBoxOutput(ns("statNumDiscarded"), width = 12),
                        bs4ValueBoxOutput(ns("statNumParsed"), width = 12)
                      )),
                      bs4TabCard(id=ns("viewImported"), title = "Imported Data", status = NULL, elevation = NULL,
                                 solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL,
                                 tabStatus = NULL, width = 12, height = NULL, collapsible = TRUE,
                                 collapsed = TRUE, closable = FALSE, maximizable = TRUE, side = "right",
                                 bs4TabPanel(tabName="Chimeric", active = TRUE,
                                             dataTableOutput(ns("viewChimeraTable"))
                                 ),
                                 bs4TabPanel(tabName="CIGAR", active = FALSE,
                                             dataTableOutput(ns("viewCIGARsameData"))
                                 )
                      ),
                      bs4TabCard(id=ns("viewParsed"), title = "Parsed Data", status = NULL, elevation = NULL,
                                 solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL,
                                 tabStatus = NULL, width = 12, height = NULL, collapsible = TRUE,
                                 collapsed = TRUE, closable = FALSE, maximizable = TRUE, side = "right",
                                 bs4TabPanel(tabName="Parsed", active = TRUE,
                                             dataTableOutput(ns("viewCIGARtable"))
                                 ),
                                 bs4TabPanel(tabName=icon("file-download"), active = FALSE,
                                             p("Download parsed CIGAR table"),
                                             checkboxInput(ns("downloadParsedTable_classic"), label = "Classic splishSPLASH v1 format", value=FALSE),
                                             downloadButton(ns("downloadParsedTable"), label = "Download Table")
                                 )
                      )
             ))
             
             
  )
  
  
}


importData <- function(input, output, session){
  
  d <- reactiveValues( chimericTable = NULL,
                       cigarTable = NULL,
                       CIGARsameTable = NULL,
                       CIGARsameParsed = NULL)
  
  ### Import Chimeric.out file
  observeEvent(input$importDataChimeric,{
    chimericTable <- tryCatch(
      {
        withProgress(value = 0.2, message="Importing data... ", detail="please wait",{
          chimericTable <- fread(file=input$importDataChimeric$datapath,header=input$header,
                                 sep=input$sep, na.strings="NA", dec=".", strip.white=TRUE, stringsAsFactors=FALSE,
                                 data.table = FALSE, showProgress=TRUE) 													         
          #chimericTable <- read.table(input$importDataChimeric$datapath,header=input$header, sep=input$sep, na.strings="NA", dec=".", strip.white=TRUE, stringsAsFactors=FALSE) 													         
        })
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        showModal(modalDialog(
          title = "Cannot Read Chimeric.out.junction File",
          paste("Please check the format of your file and try again. Change the field separator from the settings tab if necessary... ...", safeError(e), sep="")
        ))
        return(NULL)
      }
    )
    
    req(chimericTable)  # Do not proceed if file read failed
    
    # Validate that this is a chimeric.out file -- give message but do not abort.
    
    #Should have 14 columns
    expectedColNum <- 14
    if (ncol(chimericTable) != expectedColNum){
      showModal(modalDialog(
        title = "Warning: Unexpected Chimeric.out.junction File Format",
        paste("File contains ", ncol(chimericTable), " columns, but ", expectedColNum," are expected.", sep="")
      ))
    }
    # truncate columns if more columns are present, otherwise do not proceed
    if (ncol(chimericTable) > expectedColNum){
      showModal(modalDialog(
        title = "Warning: Truncating Chimeric Table Columns!",
        paste("The terminal ", ncol(chimericTable)-expectedColNum, " column(s) will be removed.", sep="")
      ))
      chimericTable <- chimericTable[,1:14]
    } else if (ncol(chimericTable) < expectedColNum)  {
      chimericTable <- NULL
    }
    req(chimericTable)  # Do not proceed if number of columns is unexpected
    
    # Columns 2,5,11,13 should be numeric
    if ( (!is.numeric(chimericTable[,2])) |
         (!is.numeric(chimericTable[,5])) |
         (!is.numeric(chimericTable[,11])) |
         (!is.numeric(chimericTable[,13])) ) {
          showModal(modalDialog(
            title = "Warning: Unexpected Chimeric.out.junction File Format",
            "Columns 2, 5, 11, and 13 are expected to be numeric"))
    }
    
    # Columns 12 and 14 should be CIGAR strings
    if ((sum(grepl("[0-9]+M", chimericTable[,12])) != nrow(chimericTable)) |
        (sum(grepl("[0-9]+M", chimericTable[,14])) != nrow(chimericTable))){
      showModal(modalDialog(
        title = "Warning: Unexpected Chimeric.out.junction File Format",
        "Columns 12 and 14 should contain a CIGAR string in each row"))
    }
    
    # Show notification that import was successful
    showNotification(paste( format(nrow(chimericTable), big.mark = ","), " rows successfully imported from Chimeric.out.junction", sep = ""), type="message", duration=10)
    d$chimericTable <- chimericTable
    
    #print(str(chimericTable))
    
  })
  
  
  ### Import same-segment CIGAR data 
  ### Import Chimeric.out file
  observeEvent(input$importDataCIGAR,{
    CIGARsameTable <- tryCatch(
      {
        withProgress(value = 0.2, message="Importing data... ", detail="please wait",{
          # CIGARsameTable <- fread(file=input$importDataCIGAR$datapath,header=input$header,
          #                        sep=input$sep, na.strings="NA", dec=".", strip.white=TRUE, stringsAsFactors=FALSE,
          #                        data.table = FALSE, showProgress=TRUE, fill = TRUE) 													         
          
          CIGARsameTable <- read.table(input$importDataCIGAR$datapath,header=input$header, sep=input$sep, na.strings="NA", dec=".", strip.white=TRUE, stringsAsFactors=FALSE, comment.char = "@") 													         
        })
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        showModal(modalDialog(
          title = "Cannot Read Same-Segment CIGAR File",
          paste("Please check the format of your file and try again. Change the field separator from the settings tab if necessary... ...", safeError(e), sep="")
        ))
        return(NULL)
      }
    )
    
    req(CIGARsameTable)  # Do not proceed if file read failed
    
    # Validate that this is a chimeric.out file -- give message but do not abort.
    showNotification(paste( format(nrow(CIGARsameTable), big.mark = ","), " rows successfully imported from same-segment CIGAR table", sep = ""), type="message", duration=10)
    d$CIGARsameTable <- CIGARsameTable
  })
  
  
  
  # Parse CIGAR when button clicked.
  observeEvent(input$parseChimeras, {
    
    if( (!isTruthy(d$chimericTable) & !isTruthy(d$CIGARsameTable)) ){
      showModal(modalDialog(
        title = "Error: No data imported yet",
        "Please import data before attempting to parse"))
    }
    
    req((isTruthy(d$chimericTable) | isTruthy(d$CIGARsameTable)))
    
    setProgress <- shiny::setProgress
    withProgress(message="Parsing CIGAR strings", value=0, {
      
      setProgress(value=0.1, detail = "...chimeric table")
      # Parse CIGAR strings to get start / end coordinates
      if ( isTruthy(d$chimericTable) ){
        chimTable <- parseChimeraCIGAR(d$chimericTable)
      } else {
        chimTable <- NULL
      }
      
      setProgress(value=0.2, detail = "...cigar same table")
      if ( isTruthy(d$CIGARsameTable) ){
        sameTable <- parseSameCIGAR(d$CIGARsameTable)
      } else {
        sameTable <- NULL
      }
      
      setProgress(value=0.3, detail = "...converting columns to factors")
      # Merge table & Convert dataframe columns to factors to shrink size
      mergedTable <- bind_rows(chimTable, sameTable)
      mergedTable$chr1 <- as.factor(mergedTable$chr1)
      mergedTable$chr2 <- as.factor(mergedTable$chr2)
      mergedTable$strand1 <- as.factor(mergedTable$strand1)
      mergedTable$strand2 <- as.factor(mergedTable$strand2)
      mergedTable$start1 <- as.integer(mergedTable$start1)
      mergedTable$end1 <- as.integer(mergedTable$end1)
      mergedTable$start2 <- as.integer(mergedTable$start2)
      mergedTable$end2 <- as.integer(mergedTable$end2)
      
      #saveRDS(mergedTable, "mergedTable.RDS")
      print(nrow(mergedTable))
      setProgress(value=0.4, detail = "...discarding long spans")
      rowsToKeep <- which(!((mergedTable$end1 - mergedTable$start1) > input$maxCIGARspan) | 
                       ((mergedTable$end2 - mergedTable$start2) > input$maxCIGARspan))
      numToDiscard_tooLong <- nrow(mergedTable) - length(rowsToKeep)
      mergedTable <- mergedTable[rowsToKeep,]

      setProgress(value=0.5, detail = "...discarding duplicate read IDs")
      rowsToKeep <- which(!duplicated(mergedTable$readName))
      numToDiscard_dupName <- nrow(mergedTable) - length(rowsToKeep)
      mergedTable <- mergedTable[rowsToKeep,]
      
      setProgress(value=0.6, detail = "...flagging PCR duplicates")
      # flag duplicates on the merged table and write to the reactive d$cigarTable
      d$cigarTable <- flagDuplicates(mergedTable)
      
      # Find-replace segment names 
      if(input$renameChrFindRep == TRUE){
        setProgress(value=0.65, detail = "... find/replacing chromosome names")
        d$cigarTable$chr1 <- as.character(d$cigarTable$chr1)
        d$cigarTable$chr2 <- as.character(d$cigarTable$chr2)
        tryCatch(
          {
            if(input$renameChrFindRepList != ""){  
              searchPairs <- unlist(strsplit(input$renameChrFindRepList, "\n")) # Split each line apart
              for (i in 1:length(searchPairs)){
                if(searchPairs[i] != ""){   # empty lines somehow match lead to 'finding' all names and deleting them, so check not empty
                  findRep <- unlist(strsplit(searchPairs[i], ","))
                  if (findRep[1] != ""){ # empty lines somehow match lead to 'finding' all names and deleting them, so check not empty
                    d$cigarTable$chr1 <- str_replace(d$cigarTable$chr1, findRep[1], findRep[2])
                    d$cigarTable$chr2  <- str_replace(d$cigarTable$chr2,  findRep[1], findRep[2])
                  }
                }
              }
            }
          },
          error = function(e) {
            # return a safeError if a parsing error occurs
            showModal(modalDialog(
              title = "Warning: segment chromome names",
              paste("Could not find-replace chromosome names. Check the format of the input in the find/replace box, and remove any spaces or tabs... ... ", safeError(e), sep="")
            ))
          }
        )
        d$cigarTable$chr1 <- as.factor(d$cigarTable$chr1)
        d$cigarTable$chr2 <- as.factor(d$cigarTable$chr2)
      }
      
      
      # Shorten Read IDs
      if (input$shortenReadIDs == TRUE){
        setProgress(value=0.7, detail = "... truncating read names")
        d$cigarTable$readName <- gsub("^[a-zA-Z0-9]+:[a-zA-Z0-9]+:[a-zA-Z0-9]+:", "", d$cigarTable$readName)
      }
      

      # Count flagged duplicates
      setProgress(value=0.8, detail = "... counting duplicates")
      numDuplicates <- list()
      numDuplicates$readName <- sum(grepl("dR", as.character(d$cigarTable$flags)))
      numDuplicates$classic <- sum(grepl("dC", as.character(d$cigarTable$flags)))
      numDuplicates$strict <- sum(grepl("dS", as.character(d$cigarTable$flags)))
      numDuplicates$ignoreCIGAR <- sum(grepl("dI", as.character(d$cigarTable$flags)))
      numReads <- nrow(d$cigarTable)
      
      # Display flagged duplicates to console
      showNotification("Parsing completed successfully.", type="message", duration=10)
      showNotification(paste(sprintf("%.1f", numDuplicates$readName*100/numReads), "% duplicated by read name", sep=""), type="error", duration=10)
      showNotification(paste(sprintf("%.1f", numDuplicates$classic*100/numReads), "% duplicated classic", sep=""), type="warning", duration=10)
      showNotification(paste(sprintf("%.1f", numDuplicates$strict*100/numReads), "% duplicated strict", sep=""), type="warning", duration=10)
      showNotification(paste(sprintf("%.1f", numDuplicates$ignoreCIGAR*100/numReads), "% duplicated ignoring CIGAR", sep=""), type="warning", duration=10)
      
      #Validate against FASTA file 
      # (not yet implemented)
      
    }) # end progress block
    
  })
  
  ### Show statistics boxes
  
  output$statNumChimeric <- renderbs4InfoBox({
    req(d$chimericTable)
    bs4InfoBox(
      title = "Chimeric Reads Imported",
      gradientColor = "primary",
      value = format(nrow(d$chimericTable), big.mark = ","),
      icon = "stream"
    )
  })
  
  output$statNumSameCIGAR <- renderbs4InfoBox({
    req(d$CIGARsameTable)
    bs4InfoBox(
      title = "Same-Segment Reads Imported",
      gradientColor = "primary",
      value = format(nrow(d$CIGARsameTable), big.mark = ","),
      icon = "stream"
    )
  })
  
  output$statNumDiscarded <- renderbs4InfoBox({
    req(d$cigarTable)
    bs4InfoBox(
      title = "Reads discarded (span too long, duplicated in Chimeric/same)",
      gradientColor = "danger",
      value = format(nrow(d$chimericTable) + nrow(d$CIGARsameTable) - nrow(d$cigarTable), big.mark = ","),
      icon = "backspace"
    )
  })
  
  output$statNumParsed <- renderbs4InfoBox({
    req(d$cigarTable)
    bs4InfoBox(
      title = "Final Reads in Parsed Table",
      gradientColor = "success",
      value = format(nrow(d$cigarTable), big.mark = ","),
      icon = "check"
    )
  })
  
  
  ### Preview tables
  output$viewCIGARtable <- renderDT(server=TRUE, {
    DT::datatable(d$cigarTable, class='compact', filter = 'top') 
  })
  
  output$viewChimeraTable <- renderDT(server=TRUE, {
    DT::datatable(d$chimericTable, class='compact') 
  })
  
  output$viewCIGARsameData <- renderDT(server=TRUE, {
    DT::datatable(d$CIGARsameTable, class='compact') 
  })
  
  # Deduplicate    
  # use anti_join to get rows that don't have same readID in Chimierc as CIGAR
  # merge the two together.
  
  ### Download Parsed data table
  ## Download Project
  output$downloadParsedTable <- downloadHandler(
    filename = function() {
      paste('parsedReadTable.txt', sep='')
    },
    content = function(con) {
      withProgress(message="Gathering data", detail="... this may take a while", {
        
        tableToWrite <- d$cigarTable
        
        if(input$downloadParsedTable_classic == TRUE){
          # c("readName", "CIGAR1", "CIGAR2", "flags", "chr1", "strand1", "start1", "end1", "chr2", "strand2", "start2", "end2")
          tableToWrite <- dplyr::select(tableToWrite, chr1, start1, end1, chr2, start2, end2)
          colnames(tableToWrite) <- c("Parent", "StartParent", "EndParent", "Child", "StartChild", "EndChild")
        }
        
        fwrite(tableToWrite, con, showProgress=TRUE, row.names=FALSE, col.names=TRUE, eol="\n", sep="\t", quote=FALSE)
      })
      showNotification("Finished saving file!", type = "message")
    }
  )
  
  
  
  updateDataFn = function(new_Data) {
    d$cigarTable <- new_Data
  }
  
  
  
  return(reactive({
    list(updateData = updateDataFn, data = d$cigarTable)
  }))
  
  
  # return(reactive({
  #   d$cigarTable
  # }))
  
}


########################
### Helper functions ###
########################

parseChimeraCIGAR <- function(dfInput){
  library(stringr)
  library(GenomicAlignments)
  library(dplyr)
  # library(readr)
  #dfInput <- read_delim("~/Dropbox/_Lab Notebook/2018 - Lab Notebook/splishSPLASH/data/Bernadeta-WSN2/Chimeric.out.junctionWSN2", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
  colnames(dfInput) <- c("chr1", "pos1", "strand1", "chr2", "pos2", "strand2", "junctionType", "repeatLenLeft", "repeatLenRight", "readName", "firstPos1", "CIGAR1", "firstPos2", "CIGAR2")
  
  ## 'Parent' first segment
  SumUpBeforeParent <- cigarWidthAlongReferenceSpace(dfInput$CIGAR1)
  StartParent <- dfInput$firstPos1
  EndParent <- (dfInput$firstPos1+SumUpBeforeParent -1)
  
  # 'Child' second segment
  SumUpBeforeChild <- cigarWidthAlongReferenceSpace(dfInput$CIGAR2)
  StartChild <- dfInput$firstPos2
  EndChild <- (dfInput$firstPos2+SumUpBeforeChild-1) 
  
  flags <- rep("", length(EndChild))
  dfParsed <- cbind(dfInput$readName, dfInput$CIGAR1, dfInput$CIGAR2, flags, dfInput$chr1, dfInput$strand1, StartParent, EndParent, dfInput$chr2, dfInput$strand2, StartChild, EndChild)
  colnames(dfParsed) <- c("readName", "CIGAR1", "CIGAR2", "flags", "chr1", "strand1", "start1", "end1", "chr2", "strand2", "start2", "end2")
  
  return(as_tibble(dfParsed))
}

# dfInput <- read.table("~/Dropbox/_Lab Notebook/2019 - Lab Notebook/splishSPLASH2/data/CIGAR.same.out.txt.gz", header=FALSE, comment.char = "@", sep="\t", na.strings="NA", dec=".", strip.white=TRUE, stringsAsFactors=FALSE) 		
parseSameCIGAR <- function(dfInput){
  colnames(dfInput) <-c("readName", "chr", "strand", "pos", "CIGAR")
  dfInput <- dfInput[(str_count(dfInput$CIGAR, "N") == 1),]
  splitCIGARS <- data.frame(str_split_fixed(dfInput$CIGAR, "[0-9]+N", 2), stringsAsFactors=FALSE)
  SumUpBeforeParent <- cigarWidthAlongReferenceSpace(splitCIGARS[,1])
  SumUpBeforeChild <- cigarWidthAlongReferenceSpace(splitCIGARS[,2])
  gapJunction <- as.integer(gsub("N", "", str_extract_all(dfInput$CIGAR, "(\\d+)[N]")))
  
  StartParent <- dfInput$pos
  EndParent <- StartParent + SumUpBeforeParent -1
  StartChild <- StartParent + SumUpBeforeParent + gapJunction
  EndChild <- StartChild + SumUpBeforeChild - 1
  flags <- rep("iS", length(EndChild))  # iS = inputSame = from sameSeg dataset
  
  dfParsed <- cbind(dfInput$readName, splitCIGARS[,1], splitCIGARS[,2], flags, dfInput$chr, dfInput$strand, StartParent, EndParent, dfInput$chr, dfInput$strand, StartChild, EndChild)
  colnames(dfParsed) <- c("readName", "CIGAR1", "CIGAR2", "flags", "chr1", "strand1", "start1", "end1", "chr2", "strand2", "start2", "end2")
  
  return(as_tibble(dfParsed))
  
}

flagDuplicates <- function(dfParsed){
  dfParsed <- as_tibble(dfParsed)
  
  dfParsed$flags <- as.character(dfParsed$flags)
  
  # uniqueIDs <- which(duplicated(dfParsed$readName))   # No longer needed since reads are pre-deduplicated based on read name!
  # dfParsed$flags[uniqueIDs] <- paste("dR", dfParsed$flags[uniqueIDs], sep="")
  
  uniqueIDs <- which(duplicated(paste(dfParsed$chr1, dfParsed$strand1, dfParsed$chr2, dfParsed$strand2, dfParsed$start1, dfParsed$end1, dfParsed$start2, dfParsed$end2, dfParsed$CIGAR1, dfParsed$CIGAR2)))
  dfParsed$flags[uniqueIDs] <- paste("dC", dfParsed$flags[uniqueIDs], sep="")
  
  uniqueIDs <- which(duplicated(paste(dfParsed$chr1, dfParsed$strand1, dfParsed$chr2, dfParsed$strand2, dfParsed$start1, dfParsed$end1, dfParsed$start2)))
  dfParsed$flags[uniqueIDs] <- paste("dS", dfParsed$flags[uniqueIDs], sep="")
  
  uniqueIDs <- which(duplicated(paste(dfParsed$chr1, dfParsed$strand1, dfParsed$chr2, dfParsed$strand2, dfParsed$start1, dfParsed$end1, dfParsed$start2, dfParsed$end2)))
  dfParsed$flags[uniqueIDs] <- paste("dI", dfParsed$flags[uniqueIDs], sep="")
  
  dfParsed$flags <- as.factor(dfParsed$flags)
  return(dfParsed)
  
}


