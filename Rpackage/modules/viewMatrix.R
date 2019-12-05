###########################################
##       Interaction Matrix & Loci       ##
###########################################

# David LV Bauer
# 8 August 2019


viewMatrix_UI <- function (id) {
  ns = NS(id)
  bs4TabItem(tabName = "thresholdData",
             fluidRow(
               column(4,
                      bs4TabCard(id=ns("viewMatrixControl"), title = "View", status = NULL, elevation = NULL,
                                 solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL,
                                 tabStatus = NULL, width = 12, height = 700, collapsible = FALSE,
                                 collapsed = FALSE, closable = FALSE, maximizable = FALSE, side = "right",
                                 bs4TabPanel(tabName=list(icon("eye")), active = TRUE,
                                             selectInput(ns("segmentA"), label="Segment A", choices=NULL, selected=NULL),
                                             selectInput(ns("segmentB"), label="Segment B", choices=NULL, selected=NULL),
                                             textOutput(ns("segmentNumReadsSelected")),
                                             textOutput(ns("segmentMaxInMatrix")),
                                             textOutput(ns("segmentPairCount")),
                                             actionButton(ns("segmentPairPrev"), label="", icon=icon("angle-double-left")),
                                             actionButton(ns("segmentPairNext"), label="", icon=icon("angle-double-right")),
                                             numericInput(ns("segmentPairCurrent"), label = "Current Seg Pair", min=0, max=0, value = 0),
                                             tags$hr(),
                                             radioButtons(ns("outputImage_logLin"),label = "Image scaling", choices = c("Linear", "Log2"), inline = TRUE, selected = "Linear"),
                                             radioButtons(ns("outputImage_colormap"),label = "Colormap", choices = c("viridis", "cividis"), inline = TRUE, selected = "viridis"),
                                             checkboxInput(ns("outputImage_showMask"), label="Show Maxima as Mask")
                                             
                                 ),
                                 bs4TabPanel(tabName=list(icon("search-location")), active = FALSE,
                                             helpText("Settings"),
                                             numericInput(ns("findMaxima_baseline"), "Baseline (reads)", value = 25),
                                             numericInput(ns("findMaxima_searchBox"), "Search Window (nt)", value = 75),
                                             numericInput(ns("findMaxima_dilate"), "Dilation factor (x)", min=0.5, max = 10, step=0.1, value = 1.1),
                                             numericInput(ns("findMaxima_grow"), "ImageR Grow (nt)", min=0, max = 100, step=5, value = 30),
                                             numericInput(ns("findMaxima_shrink"), "ImageR Shrink (nt)", min=0, max = 100, step=5, value = 25),
                                             numericInput(ns("findMaxima_minArea"), "Minimum area (nt^2)", min=0, max = 1000, step=25, value = 100),
                                             actionButton(ns("findMaxima_execute"), "Find Local Maxima", icon=icon("search-location"))
                                 ),
                                 bs4TabPanel(tabName=list(icon("cogs")), active = FALSE,
                                             helpText("Settings"),
                                             downloadButton(ns("downloadParsedReads"), "Download Current Parsed Reads"),
                                             tags$br(),
                                             downloadButton(ns("downloadMatrix"), "Download Matrix")
                                             
                                             
                                 )
                      )
               ),
               column(8,
                      bs4TabCard(id=ns("viewMatrixPlot"), title = "Interaction Plot", status = NULL, elevation = NULL,
                                 solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL,
                                 tabStatus = NULL, width = 12, height = 700, collapsible = TRUE,
                                 collapsed = FALSE, closable = FALSE, maximizable = TRUE, side = "right",
                                 bs4TabPanel(tabName="Static", active = TRUE,
                                             plotOutput(ns("outputImageStatic"), height = "600px")
                                 ),
                                 bs4TabPanel(tabName="Maxima", active = FALSE,
                                             plotOutput(ns("outputImageMaximaStatic"), height = "600px")
                                 ),
                                 bs4TabPanel(tabName="Interactive", active = FALSE,
                                             plotlyOutput(ns("outputImagePlotly"), height = "600px")
                                 )
                      ),
                      bs4TabCard(id=ns("viewBlosFound"), title = "Identified Loci", status = NULL, elevation = NULL,
                                 solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL,
                                 tabStatus = NULL, width = 12, collapsible = TRUE,
                                 collapsed = TRUE, closable = FALSE, maximizable = TRUE, side = "right",
                                 bs4TabPanel(tabName="Table", active = TRUE,
                                             DT::dataTableOutput(ns("blobsFoundTable"))
                                 )
                      )
                      )
               
             )
  )
}
  
findLoci_UI <- function (id) {
  ns = NS(id)
  bs4TabItem(tabName = "autoFindLoci",
             fluidRow(
               column(4,
                      bs4Card(id=ns("autoFindControl"), title = "Auto-find", status = "primary", elevation = NULL,
                                 solidHeader = TRUE, headerBorder = TRUE, gradientColor = NULL,
                                 tabStatus = NULL, width = 12, collapsible = FALSE,
                                 collapsed = FALSE, closable = FALSE, maximizable = FALSE, side = "right",
                                 p("Loci will be automatically detected using the settings on the",
                                          "'Set Threshold' tab. Detection typically takes between 1 and 10 minutes,",
                                          "depending on the size of the dataset and the settings chosen."),
                              actionButton(ns("autoFind_run"), icon=icon("magic"), label = "Auto-Find Loci")
                      ),
                      bs4Card(id=ns("autoFindMatrices"), title = "Matrices", status = "primary", elevation = NULL,
                              solidHeader = TRUE, headerBorder = TRUE, gradientColor = NULL,
                              tabStatus = NULL, width = 12, collapsible = FALSE,
                              collapsed = FALSE, closable = FALSE, maximizable = FALSE, side = "right",
                              p("Generate all contact matrices for export"),
                              checkboxInput(ns("autoFindMatrices_includeSameSeg"), label="Include Same-Segment Contacts", value=FALSE),
                              checkboxInput(ns("autoFindMatrices_includeDiffSeg"), label="Include Inter-Segment Contacts", value=TRUE),
                              actionButton(ns("autoFindMatrices_run"), icon=icon("border-style"), label = "Count")
                      ),
                      bs4Card(id=ns("autoFindCounts"), title = "Counts", status = "primary", elevation = NULL,
                              solidHeader = TRUE, headerBorder = TRUE, gradientColor = NULL,
                              tabStatus = NULL, width = 12, collapsible = FALSE,
                              collapsed = FALSE, closable = FALSE, maximizable = FALSE, side = "right",
                              p("Generate a matrix of the number of reads or loci per segment pair"),
                              radioButtons(ns("autoFindCounts_mode"), label="Count", choices=c("Reads")),
                              radioButtons(ns("autoFindCounts_norm"), label="Normalise", choices=c("None", "By Segment Length")),
                              actionButton(ns("autoFindCounts_run"), icon=icon("sort-numeric-down"), label = "Count")
                      )
               ),
               column(8,
                      bs4TabCard(id=ns("autoFindOutput"), title = "Loci", status = NULL, elevation = NULL,
                              solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL,
                              tabStatus = NULL, width = 12, collapsible = FALSE,
                              collapsed = FALSE, closable = FALSE, maximizable = TRUE, side = "right",
                              bs4TabPanel(tabName=list(icon("table")), active = TRUE,
                                          DT::dataTableOutput(ns("autoFind_tableOutput"))
                              ),
                              bs4TabPanel(tabName=list(icon("file-download")), active = FALSE,
                                          p("Download loci list"),
                                          checkboxInput(ns("autoFind_downloadTableOutput_classic"), label = "Classic splishSPLASH v1 format", value=FALSE),
                                          downloadButton(ns("autoFind_downloadTableOutput"))
                              )
                      )
               )
             )
  )
}           


viewMatrix <- function(input, output, session, refGenome, parsedData){
  
  # Set up reactive values table
  v <- reactiveValues( currentParsedReads = NULL,
                       currentMatrix = NULL,
                       currentMatrixDim = NULL,
                       currentMatrixMaxima = NULL,
                       currentMatrixBlobs = NULL,
                       segmentList = NULL,
                       segmentLens = NULL,
                       segmentSeqs = NULL,
                       segmentPairs = NULL,
                       autoFindBlobsTable = NULL)
  
  # Get list of segments in filtered dataset, and generate list of pairs!
  observe({
    segList <- refGenome()$data$refGenomeDF[,c("segment")]
    segLens <- refGenome()$data$refGenomeDF[,c("length")]
    segSeqs <- refGenome()$data$refGenomeDF[,c("seq")]
    selctedSegs <- parsedData()$selectedSegmentNames
    
    relevantRows <- segList %in% selctedSegs
    
    v$segmentList <- segList[relevantRows]
    v$segmentLens <- segLens[relevantRows]
    v$segmentSeqs <- segSeqs[relevantRows]
  })
  
  observe({
    req(v$segmentList)
    numChr <- length(v$segmentList)
    req(numChr > 2)
    # For not including 'self'
    numPairs <- (numChr^2)/2 - (numChr/2)  # If we included self interactions, it would be:  numMatrices <- (numChr^2)/2 + (numChr/2)
    segPairs <- data.frame(seg1=rep("", numPairs), seg2=rep("", numPairs), stringsAsFactors=FALSE)
    rowCount <- 1
    for (i in 1:(numChr-1)){  # remove "-1" to include self interactions, and...
      for (j in (i+1):numChr){ # ...remove "+1" to include self interactions
        segPairs$seg1[rowCount] <- v$segmentList[i]
        segPairs$seg2[rowCount] <- v$segmentList[j]
        rowCount <- rowCount + 1
      }
    }
    v$segmentPairs <- segPairs
    print(segPairs)
  })
  
  
  parsedTable <- reactive({
    parsedData()$data
  })
  
  # Update segment dropdown boxes based on filtered segments / reference genome
  observe({
    req(v$segmentList)
    updateSelectInput("segmentA", session = session, choices = c("Select...", v$segmentList))
    updateSelectInput("segmentB", session = session, choices = c("Select...", v$segmentList))
  })
  
  # Update segment pair control based on filtered segments / reference genome
  observe({
    req(v$segmentList)
    updateNumericInput("segmentPairCurrent", session = session, min = 1, max=nrow(v$segmentPairs), value = 1)
  })
  
   # Change segment pair when previous is clicked
   observeEvent(input$segmentPairPrev, {
     req(v$segmentList)
     if (input$segmentPairCurrent > 1) {
       updateNumericInput("segmentPairCurrent", session = session, value = (input$segmentPairCurrent -1) )
     }
   })
  
   # Change segment pair when next is clicked
   observeEvent(input$segmentPairNext, {
     req(v$segmentPairs)
     if (input$segmentPairCurrent < nrow(v$segmentPairs)) {
       updateNumericInput("segmentPairCurrent", session = session, value = (input$segmentPairCurrent + 1) )
     }
   })

   # Change segment input boxes when segment pair is updated
   observeEvent(input$segmentPairCurrent, {
     req(v$segmentPairs)
     segPairs <- v$segmentPairs
     updateSelectInput("segmentA", session = session, selected = segPairs$seg1[input$segmentPairCurrent])
     updateSelectInput("segmentB", session = session, selected = segPairs$seg2[input$segmentPairCurrent])
   })
  
  
  
  
  # Select reads from input parsed table based on user selection
  observe({
    req(parsedTable())
    if(!(input$segmentA %in% c("","Select...")) & !(input$segmentB %in% c("","Select...")) ){
      rowsToSelect <- which(
        ((isolate(parsedTable()$chr1) %in% input$segmentA) & (isolate(parsedTable()$chr2) %in% input$segmentB)) |
          ((isolate(parsedTable()$chr1) %in% input$segmentB) & (isolate(parsedTable()$chr2) %in% input$segmentA))
        )
      # swapChrReadTable ensure segmentA is in chr1
      v$currentParsedReads <- swapChrReadTable(parsedTable()[rowsToSelect,], input$segmentA, input$segmentB)
      v$currentMatrixDim <- c( v$segmentLens[which(v$segmentList == input$segmentA)] , v$segmentLens[which(v$segmentList == input$segmentB)] )
    }
  })
  
  
  
  # Display number of reads to user
  output$segmentNumReadsSelected <- renderText({
    req(v$currentParsedReads)
    paste(nrow(v$currentParsedReads), " reads selected",sep="")
  })
  
  # Display maximum number of reads interaction matrix to user
  output$segmentMaxInMatrix <- renderText({
    req(v$currentMatrix)
    paste(max(v$currentMatrix), " reads max. in matrix",sep="")
  })
  
  
  ## GENERATE INTERACTION MATRIX
  observe({
    req(isTruthy(v$currentParsedReads))
    req(isTruthy(v$currentMatrixDim))
    v$currentMatrix <- generateInteractionMatrix_cpp(v$currentParsedReads, rep(max(v$currentMatrixDim),2)) # rep(max(...),2) makes a square matrix
    # Reset maxima and blobs if currentMatrix has changed!
    v$currentMatrixMaxima <- NULL
    v$currentMatrixBlobs <- NULL
    
  })
  
  # Download current parsed reads
  output$downloadParsedReads <- downloadHandler(
    filename = function() {
      paste('currentParsedReads.rds', sep='')
    },
    content = function(con) {
      withProgress(message="Gathering data", detail="... this may take a while", {
        write_rds(v$currentParsedReads, con)
      })
    }
  )
  
  
  # Download Matrix when asked
  output$downloadMatrix <- downloadHandler(
    filename = function() {
      paste('matrix.rds', sep='')
    },
    content = function(con) {
      withProgress(message="Gathering data", detail="... this may take a while", {
        write_rds(v$currentMatrix, con)
      })
    }
  )
  
  ### Find local Maxima when clicked
  observeEvent(input$findMaxima_execute, {
    tic()
    # withProgress(value=0.2,message = "Finding local maxima", detail = "... ETA 3 sec" ,{
      v$currentMatrixMaxima <- findLocalMaxima_cpp(v$currentMatrix, input$findMaxima_baseline, input$findMaxima_searchBox, input$findMaxima_dilate)
      if (input$findMaxima_grow > 0){
        v$currentMatrixMaxima <- as.matrix(grow(as.pixset(as.cimg(v$currentMatrixMaxima)), input$findMaxima_grow))
      }
      if (input$findMaxima_shrink > 0){
        v$currentMatrixMaxima <- as.matrix(shrink(as.pixset(as.cimg(v$currentMatrixMaxima)), input$findMaxima_shrink))
      }
      # setProgress(value=0.75)
      v$currentMatrixBlobs <- findBlobs(v$currentParsedReads, v$currentMatrixMaxima, v$currentMatrix)
      if (input$findMaxima_minArea > 0){
        v$currentMatrixBlobs <- dplyr::filter(v$currentMatrixBlobs, npixel >= input$findMaxima_minArea)
      }
    # })
    toc()
  })
  
  
  ### **NEW 7 Dec** Find Auto-Find All local Maxima when clicked
  observeEvent(input$autoFind_run, {
    tic()
    
    req(parsedTable())
    
    segPairs <- v$segmentPairs
    for (i in 1:nrow(segPairs)){
      segmentA <- segPairs$seg1[i]
      segmentB <- segPairs$seg2[i]
      cat(paste(segmentA, "::", segmentB, "   "))
      tic()
      # Get relevant parsed Reads
      rowsToSelect <- which(
        ((isolate(parsedTable()$chr1) %in% segmentA) & (isolate(parsedTable()$chr2) %in% segmentB)) |
          ((isolate(parsedTable()$chr1) %in% segmentB) & (isolate(parsedTable()$chr2) %in% segmentA))
      )
      # swapChrReadTable ensure segmentA is in chr1
      currentParsedReads <- swapChrReadTable(parsedTable()[rowsToSelect,], segmentA, segmentB)
      currentMatrixDim <- c( v$segmentLens[which(v$segmentList == segmentA)] , v$segmentLens[which(v$segmentList == input$segmentB)] )
      
      currentMatrix <- generateInteractionMatrix_cpp(currentParsedReads, rep(max(currentMatrixDim),2))
      
      # withProgress(value=0.2,message = "Finding local maxima", detail = "... ETA 3 sec" ,{
      currentMatrixMaxima <- findLocalMaxima_cpp(currentMatrix, input$findMaxima_baseline, input$findMaxima_searchBox, input$findMaxima_dilate)
      if (input$findMaxima_grow > 0){
        currentMatrixMaxima <- as.matrix(grow(as.pixset(as.cimg(currentMatrixMaxima)), input$findMaxima_grow))
      }
      if (input$findMaxima_shrink > 0){
        currentMatrixMaxima <- as.matrix(shrink(as.pixset(as.cimg(currentMatrixMaxima)), input$findMaxima_shrink))
      }
      # setProgress(value=0.75)
      currentMatrixBlobs <- findBlobs(currentParsedReads, currentMatrixMaxima, currentMatrix)
      if (input$findMaxima_minArea > 0){
        currentMatrixBlobs <- dplyr::filter(currentMatrixBlobs, npixel >= input$findMaxima_minArea)
      }
      currentMatrixBlobs <- add_column(currentMatrixBlobs, seg1 = segmentA, seg2 = segmentB)
      # })
      
      if (i == 1){
        autoFindBlobsTable <- currentMatrixBlobs
      } else {
        autoFindBlobsTable <- rbind(autoFindBlobsTable, currentMatrixBlobs)
      }
      toc()
    }
    
    v$autoFindBlobsTable <- autoFindBlobsTable
    toc()
  })
  
  
  
  
  ### Render output images
  output$outputImageStatic <- renderPlot({
    req(v$currentMatrix)
    
    if(input$outputImage_logLin == "Log2"){
      matToPlot <- log20(v$currentMatrix)
    } else {
      matToPlot <- v$currentMatrix
    }
    
    fields::image.plot(matToPlot, asp=1, useRaster=TRUE, legend.shrink = 0.3,
               col=viridis(256, option=input$outputImage_colormap),
               xaxt="n",
               yaxt="n",
               xlab = input$segmentA,
               ylab = input$segmentB
               )
    
  })  
  
  
  output$outputImageMaximaStatic <- renderPlot({
    req(v$currentMatrixMaxima)
    
    if(input$outputImage_showMask == FALSE){
      if(input$outputImage_logLin == "Log2"){
        matToPlot <- log20(v$currentMatrix)
      } else {
        matToPlot <- v$currentMatrix
      }
    } else {
      matToPlot <- v$currentMatrixMaxima
    }
    
    plot(mirror(as.cimg((matToPlot)), "y"), ylim=c(1, height(matToPlot)))
    req(v$currentMatrixBlobs)
    points(v$currentMatrixBlobs$mx, v$currentMatrixBlobs$my, col="green", pch="o", cex=1, lwd=1)
  })  

  
  output$outputImagePlotly <- renderPlotly({
    req(v$currentMatrix)
    
    if(input$outputImage_logLin == "Log2"){
      matToPlot <- log20(v$currentMatrix)
    } else {
      matToPlot <- v$currentMatrix
    }
    
    p <- plot_ly(z = t(matToPlot), type = "heatmap", width=600, height=500) %>%
      layout(xaxis = list(title=input$segmentA),
             yaxis = list(title=input$segmentB),
             scene = list(aspectratio=list(x=1,y=1)))
    # add_trace(x = points$x, y = points$y, type='scatter') %>%
    
    if (isTruthy(v$currentMatrixBlobs)){
      rectangle <- list(
        type = "rect",
        line = list(color = "magenta"),
        xref = "x",
        yref = "y"
      )
      rectangles <- list()
      for (i in 1:nrow(v$currentMatrixBlobs)) {
        rectangle[["x0"]] <- v$currentMatrixBlobs$minX[i]
        rectangle[["x1"]] <- v$currentMatrixBlobs$maxX[i]
        rectangle[["y0"]] <- v$currentMatrixBlobs$minY[i]
        rectangle[["y1"]] <- v$currentMatrixBlobs$maxY[i]
        rectangles <- c(rectangles, list(rectangle))
      }
      p <- p %>% layout(shapes=rectangles)
      
    }
    p
    
    # p <- ggplot(data=melt(v$currentMatrix), aes(x=Var1, y=Var2, fill=value)) + 
    #   geom_raster() + theme_minimal() + scale_fill_viridis_c()
    # 
    # if (nrow(points) > 0){
    #   p <- p + geom_point(data = points, aes(x=x, y=y), inherit.aes = FALSE)
    #   # Add rectangle plotting here
    # }
    #   
    # ggplotly(p)
  })
  
  output$blobsFoundTable <- DT::renderDataTable({ req(v$currentMatrixBlobs)
    
                                                v$currentMatrixBlobs %>% DT::datatable(
                                                class='compact',
                                                editable = FALSE) %>%
                                              formatRound(columns=c('mx', 'my', 'normRPKC'), digits=1)
  })
  
  
  
  
  
  
  ### Auto-find tab
  
  ### Execute read count matrix upon click ###
  observeEvent(input$autoFindCounts_run, {
    
  })
  
  
  ### Execute export of all contact matrices upon click ###
  observeEvent(input$autoFindMatrices_run, {
    req(v$segmentList)
    req(parsedData())
    
    #!! Note: the segment pair is identical to blob auto-finding below. Consider separating it
    
    numChr <- length(v$segmentList)
    # For not including 'self'
    numPairs <- (numChr^2)/2 - (numChr/2)  # If we included self interactions, it would be:  numMatrices <- (numChr^2)/2 + (numChr/2)
    
    # Set up a matrix to list the segment pairs we will test
    segPairs <- data.frame(seg1=rep("", numPairs), seg2=rep("", numPairs), seg1num=rep(0, numPairs), seg2num=rep(0, numPairs), stringsAsFactors=FALSE)
    rowCount <- 1
    if(input$autoFindMatrices_includeDiffSeg == TRUE){
      for (i in 1:(numChr-1)){  # remove "-1" to include self interactions, and...
        for (j in (i+1):numChr){ # ...remove "+1" to include self interactions
          segPairs$seg1[rowCount] <- v$segmentList[i]
          segPairs$seg2[rowCount] <- v$segmentList[j]
          segPairs$seg1num[rowCount] <- i
          segPairs$seg2num[rowCount] <- j
          rowCount <- rowCount + 1
        }
      }
    }
    if (input$autoFindMatrices_includeSameSeg == TRUE){
      for (i in 1:numChr){
        segPairs$seg1[rowCount] <- v$segmentList[i]
        segPairs$seg2[rowCount] <- v$segmentList[i]
        segPairs$seg1num[rowCount] <- i
        segPairs$seg2num[rowCount] <- i
        rowCount <- rowCount + 1
      }
    }
    
    
    # Copy reactive parsedData() to dfParsed for use here
    dfParsed <- parsedData()$data
    
    
    # Iterate over each segment pair to find blobs
    setProgress <- shiny::setProgress
    withProgress(min=0, max=numPairs, message = "Finding local maxima",{
      tic()
      for (i in 1:numPairs){
        cat(paste(i, " of ", numPairs, "—————————————", sep=""))
        setProgress(value = i, detail=paste(segPairs$seg1[i], "::",segPairs$seg2[i], sep=""))
        tic()
        rowsToSelect <- which(
          ((dfParsed$chr1 %in% segPairs$seg1[i]) & (dfParsed$chr2 %in% segPairs$seg2[i])) |
            ((dfParsed$chr1 %in% segPairs$seg2[i]) & (dfParsed$chr2 %in% segPairs$seg1[i]))
        )
        
        # if no rows match the selectd pairs, then do not continue with the rest of the loop iteration 
        if (length(rowsToSelect) == 0) { toc(quiet=TRUE); cat('no data\n'); next }
        
        relevantRows <- dfParsed[rowsToSelect,]
        
        
        # swapChrReadTable ensure segmentA is in chr1
        currentDFparsed <- swapChrReadTable(relevantRows, segPairs$seg1[i], segPairs$seg2[i])
        currentMatrixDim <- c( v$segmentLens[which(v$segmentList == segPairs$seg1[i])] , v$segmentLens[which(v$segmentList == segPairs$seg2[i])] )
        
        # generate interaction matrix
        mat <- generateInteractionMatrix_cpp(currentDFparsed, currentMatrixDim)
        # export matrix
        
        fileName <- paste(segPairs$seg1num[i], "-", segPairs$seg2num[i], "_", 
                          segPairs$seg1[i], "-", segPairs$seg2[i], "_",
                          currentMatrixDim[1], "ntX", currentMatrixDim[2], "nt",
                          ".csv", sep = "")
        
        cat(fileName)
        
        outDir <- "outData"
        dirSep <- "/"
        fileName <- paste(outDir, fileName, sep=dirSep)
        
        fwrite(mat, file = fileName, eol="\n",
               col.names = FALSE, row.names = FALSE,
               quote = FALSE
               )
        
        toc()
      }

      cat("—————————————Done\n")
      toc()
    })
    
    
    
  })
  


  output$autoFind_tableOutput <- DT::renderDataTable({
    v$autoFindBlobsTable},
    class='compact',
    editable = FALSE
  )
  
  output$autoFind_downloadTableOutput <- downloadHandler(
    filename = function() {
      paste('lociTable.txt', sep='')
    },
    content = function(con) {
      withProgress(message="Gathering data", detail="... this may take a while", {
        
        tableToWrite <- v$autoFindBlobsTable
        
        if(input$autoFind_downloadTableOutput_classic == TRUE){
          tableToWrite <- dplyr::select(tableToWrite, seg1, minX, maxX, seg2, minY, maxY, readsMax)
          colnames(tableToWrite) <- c("chr1", "start1", "end1", "chr2", "start2", "end2", "intensity")
        }
        
        fwrite(tableToWrite, con, showProgress=TRUE, row.names=FALSE, col.names=TRUE, eol="\n", sep="\t", quote=FALSE)
      })
      showNotification("Finished saving file!", type = "message")
    }
  )

  
  

  ### End of main functions
  # Function to run to restore data when uploading project
  updateDataFn = function(new_Data) {
    # leave empty -- want to re-run automatically?
  }
  # Return function and data as reactive list
  return(reactive({
    list(updateData = updateDataFn, data = v$currentMatrix)
  }))
  
}





########################
### Helper functions ###
########################

swapChrReadTable <- function(readTable, segA, segB){
  if (segA == segB){
    rowsNoFlip <- subset(readTable, start1 <= start2)
    rowsToFlip <- subset(readTable, start1 >  start2)
    rowsToFlip <- select(rowsToFlip, chr2, start2, end2, chr1, start1, end1)
    colnames(rowsToFlip) <- c("chr1", "start1", "end1", "chr2", "start2", "end2")
    readTable <- bind_rows(rowsNoFlip, rowsToFlip)
    return(readTable)
  }
  readTable <- select(readTable, chr1, start1, end1, chr2, start2, end2)
  # ensure segA is in the first column and flip rows that need to flip
  rowsNoFlip <- subset(readTable, chr1 == segA)
  rowsToFlip <- subset(readTable, chr2 == segA)
  rowsToFlip <- select(rowsToFlip, chr2, start2, end2, chr1, start1, end1)
  colnames(rowsToFlip) <- c("chr1", "start1", "end1", "chr2", "start2", "end2")
  readTable <- bind_rows(rowsNoFlip, rowsToFlip)
  return(readTable)
}


## Get number of reads from parsed table ('selected') that lie within a given window
readsWithinRange <- function(selected, segAwindowStart, segAwindowEnd, segBwindowStart, segBwindowEnd){
  return(
    subset(selected, (  ((selected$start1 >= segAwindowStart) & (selected$start1 <= segAwindowEnd) |
                           (selected$end1 >= segAwindowStart) & (selected$end1 <= segAwindowEnd) |
                           (selected$start1 <= segAwindowStart) & (selected$end1 >= segAwindowEnd)) 
                        &
                          ((selected$start2 >= segBwindowStart) & (selected$start2 <= segBwindowEnd) |
                             (selected$end2 >= segBwindowStart) & (selected$end2 <= segBwindowEnd) |
                             (selected$start2 <= segBwindowStart) & (selected$end2 >= segBwindowEnd))        ))
  )
}

dfReadsWithinRange <- function(dfParsed, centers){
  centers <- add_column(centers, readsBounded = 0)
  for (i in 1:nrow(centers)){
    centers$readsBounded[i] <- nrow(readsWithinRange(dfParsed,
                                                     centers$minX[i], centers$maxX[i],
                                                     centers$minY[i], centers$maxY[i]))
  }
  return(arrange(centers, desc(readsBounded)))
}

dfMaxReadsWithinRange <- function(currentMatrix, centers){
  centers <- add_column(centers, readsMax = 0)
  for (i in 1:nrow(centers)){
    centers$readsMax[i] <- max(currentMatrix[ centers$minX[i]:centers$maxX[i],
                                              centers$minY[i]:centers$maxY[i]  ]
                               )
  }
  return(arrange(centers, desc(readsMax)))
}




### Blob finding script
findBlobs <- function(dfParsed, blobMask, currentMatrix){
  # Find blobs using label function and calculate position, bounding box, and num pixels
  blobMask <- as.pixset(as.cimg(blobMask))
  blobs <- dplyr::group_by(subset(as.data.frame(label(blobMask)), value > 0), value) %>% 
    dplyr::summarise(mx=mean(x),my=mean(y), 
                     minX=min(x), maxX=max(x), 
                     minY=min(y), maxY=max(y),
                     npixel=n())
  
  numBlobsFound <- nrow(blobs)
  
  if(numBlobsFound == 0){
    return(blobs)
  }
  
  # Count number of reads that fall within the bounding box of each row of blobs
  # (note, uses a for loop, and assumes blobs has named columns as defined above)
  blobs <- dfReadsWithinRange(dfParsed, blobs)
  blobs <- dfMaxReadsWithinRange(currentMatrix, blobs)
  blobs <- select(blobs, -value)
  blobs$normRPKC <- 1000*blobs$readsBounded/((blobs$maxX-blobs$minX+1)*(blobs$maxY-blobs$minY+1))
  blobs <- arrange(blobs, desc(readsBounded))
  return(blobs)
}

## Log2 with NA replacement to 0
log20 <- function(input){
  out <- log2(input)
  out[!is.finite(out)] <- 0
  return(out)
}



  