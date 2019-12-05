###########################################
##  Reference Genome Loading UI/Server   ##
###########################################

# David LV Bauer
# 25 July 2019

refGenome_UI <- function (id) {
  ns = NS(id)
  bs4TabItem(tabName = "referenceGenome",
             fluidRow(column(4,
                             bs4TabCard(id=ns("referenceGenome_import"), title = list(icon("upload"), "Import"), status = NULL, elevation = NULL,
                                        solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL,
                                        tabStatus = NULL, width = 12, height = NULL, collapsible = TRUE,
                                        collapsed = FALSE, closable = FALSE, maximizable = FALSE, side = "right",
                                        bs4TabPanel(tabName="FASTA", active = TRUE,
                                                    helpText("Upload the reference genome FASTA file",
                                                             "used to map the sequencing reads."),
                                                    fileInput(ns("refGenomeFile"), label=NULL)
                                        ),
                                        bs4TabPanel(tabName="BED", active = FALSE,
                                                    helpText("Upload the a BED file with genome features",
                                                             "such as ORFs."),
                                                    fileInput(ns("refGenomeBED"), label=NULL)
                                        ),
                                        bs4TabPanel(tabName="RDS", active = FALSE,
                                                    helpText("Upload a RDS file previously-created in splishSPLASH",
                                                             "containing reference sequences, features, and colours."),
                                                    fileInput(ns("refGenomeRDS"), label=NULL)
                                        )
                             ),
                             bs4TabCard(id=ns("referenceGenome_colour"), title = "Properties", status = NULL, elevation = NULL,
                                        solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL,
                                        tabStatus = NULL, width = 12, height = NULL, collapsible = TRUE,
                                        collapsed = TRUE, closable = FALSE, maximizable = FALSE, side = "right",
                                        bs4TabPanel(tabName=list(icon("palette"),"Pallete"), active=TRUE,
                                                    helpText("Select colours to be used for each segment.",
                                                             "Colour Scheme 1 will be used for indicated number",
                                                             "of segments, and the remainder will be coloured",
                                                             "according to Colour Scheme 2. The colour of each",
                                                             "segment can be manually edited in the table to the right."),
                                                    fluidRow(
                                                      column(6,align="center",
                                                             selectInput(ns("genomeColourScheme1"), "Colour Scheme 1", 
                                                                         choices = segmentColours, selected = "Bernadeta"),
                                                             selectInput(ns("genomeColourScheme2"), "Colour Scheme 2", 
                                                                         choices = segmentColours, selected = "Greys")
                                                             
                                                      ),
                                                      column(6,align="center",
                                                             numericInput(ns("genomeColourScheme1_numSegs"), "Segs w/Scheme 1",
                                                                          min = 3, value = 8),
                                                             actionButton(ns("genomeApplyColours"), "Apply", icon=icon("palette"))
                                                      )
                                                    )
                                        ),
                                        bs4TabPanel(tabName=list(icon("fill-drip"), "Custom"), active=FALSE,
                                                    fluidRow(
                                                      column(6,align="center",
                                                             colourInput(ns("genomeCustomColour_colour"), "Select Custom Colour")
                                                      ),
                                                      column(6,align="center",
                                                             actionButton(ns("genomeCustomColour_apply"), "Apply", icon=icon("fill-drip"))
                                                      ),
                                                      column(12, align="left",
                                                             tags$b(textOutput(ns("refGenome_rowsSelected"))),
                                                             helpText("Select a colour above, then select the row(s)",
                                                                      "in the Reference Genome table to change the",
                                                                      "colour to and click 'Apply'")
                                                      )
                                                    )
                                        ),
                                        bs4TabPanel(tabName=list(icon("dna"), "Strand"), active=FALSE,
                                                    fluidRow(
                                                      column(6,align="center",
                                                             selectInput(ns("genomeSelectStrand_strand"), "Select Strand", choices = c("+/-", "+", "-"), multiple = FALSE)
                                                      ),
                                                      column(6,align="center",
                                                             actionButton(ns("genomeSelectStrand_apply"), "Apply", icon=icon("dna"))
                                                      ),
                                                      column(12, align="left",
                                                             tags$b(textOutput(ns("refGenome_rowsSelected_forStrand"))),
                                                             helpText("Select a strand above, then select the row(s)",
                                                                      "in the Reference Genome table to change the",
                                                                      "strand to and click 'Apply'")
                                                      )
                                                    )
                                        )

                             )
             ),
             column(8,
                    bs4TabCard(id=ns("referenceGenome_segTable"), title = list(icon("dna"), "Reference Genome"), status = NULL, elevation = NULL,
                               solidHeader = TRUE, headerBorder = TRUE, gradientColor = NULL,
                               tabStatus = NULL, width = 12, height = NULL, collapsible = TRUE,
                               collapsed = FALSE, closable = FALSE, maximizable = TRUE, side = "right",
                               bs4TabPanel(tabName="Genome", active = TRUE,
                                           helpText("Drag the ID column to reorder the segments",
                                                    "so that segments are in order. Double-click",
                                                    "on the segment name to rename the segment",
                                                    "to something sensible."),
                                           dataTableOutput(ns("refGenomePreview"))
                               ),
                               bs4TabPanel(tabName=icon("file-download"), active = FALSE,
                                           tags$p(helpText("FASTA file export: "),
                                                  checkboxInput(ns("downloadFasta_selectedSegsOnly"), "Only download selected segments", value=FALSE),
                                                  checkboxInput(ns("downloadFasta_convertToUpper"), "Convert all sequences to uppercase", value=TRUE),
                                                  checkboxInput(ns("downloadFasta_convertToRevComp"), "Convert all sequences to REVERSE COMPLEMENT", value=FALSE),
                                                  downloadButton(ns("downloadFasta"), "Download FASTA File")),
                                           tags$p(helpText("splishSPLASH refGenome file export:"),
                                                  downloadButton(ns("downloadRefGenomeRDS"), "Download refGenome.rds"))
                               )
                    ),
                    bs4TabCard(id="referenceGenome_featureTable", title = list(icon("bars"), "Genome Features"), status = NULL, elevation = NULL,
                               solidHeader = TRUE, headerBorder = TRUE, gradientColor = NULL,
                               tabStatus = NULL, width = 12, height = NULL, collapsible = TRUE,
                               collapsed = FALSE, closable = FALSE, maximizable = FALSE, side = "right",
                               bs4TabPanel(tabName="Features", active = TRUE,
                                           helpText("Drag the ID column to reorder the segments",
                                                    "so that segments are in order. Double-click",
                                                    "on the segment name to rename the segment",
                                                    "to something sensible."),
                                           tableOutput(ns("refGenomeFeatures"))
                               ),
                               bs4TabPanel(tabName=icon("file-download"), active = FALSE,
                                           tags$p(helpText("BED file export: "),
                                                  downloadButton(ns("downloadBED"), "Download BED File")),
                                           tags$p(helpText("splishSPLASH refGenome file export:"),
                                                  downloadButton(ns("downloadRefGenomeFeatures"), "Download Genome Features"))
                               )
                    )
             )
             )
  )
  
}

refGenome <- function(input, output, session){
  
  # Set up reactive values to hold reference genome variables

  l <- reactiveValues( linkTable = NULL,
                       linkFileToCompare = NULL,
                       linkComparisonTable = NULL,
                       linkComparisonPlot = NULL,
                       strainInfo = NULL,
                       refGenome = NULL,
                       refGenomeDF = NULL,
                       refGenomeBED = NULL)
  
  ### Handle user upload of reference FASTA file
  observeEvent(input$refGenomeFile, {
    req(input$refGenomeFile)
    # Read in FASTA file as biostring object and save in reactive value
    rGen <- tryCatch({readDNAStringSet(input$refGenomeFile$datapath)},
                     error = function(e){
                       showModal(modalDialog(
                         title = "Cannot Read FASTA File",
                         "Please check the format of your file and try again."))
                       return(NULL)
                     })
    req(rGen)
    l$refGenome <- rGen
    # convert to dataframe for manipulation
    dframe <- as.data.frame(rGen)
    colnames(dframe) <- c("seq")
    dframe$segment <- row.names(dframe)
    row.names(dframe) <- 1:nrow(dframe)
    info <- as.data.frame(rGen@ranges)
    dframe$length <- info$width
    dframe$color <- rep("#bbbbbb", nrow(dframe))
    dframe$displayName <- dframe$segment
    dframe$strandFilter <- rep("+/-", nrow(dframe))
    dframe <- dframe[,c("segment", "color", "displayName", "strandFilter", "length", "seq")]
    l$refGenomeDF <- dframe
    
  })
  
  ### Display reference genome table
  output$refGenomePreview <- renderDT(server = FALSE, {
    
    # Only continue with rendering if the refGenomeDF dataframe has been set
    # i.e. by upload of fasta file or by upload of previously-generated table
    shiny::validate(
      need(l$refGenomeDF, "Please load a reference genome file.")
    )
    
    if (is.null(l$refGenomeDF)){
      return()
    } 
    
    # truncate sequences to sent to user's table.
    dfPreview <- l$refGenomeDF
    dfPreview$seq <- paste(substr(dfPreview$seq,1,30), "...", sep="")
    
    DT::datatable(dfPreview, 
                  class='compact',
                  colnames = c(ID = 1),  # add the name 
                  extensions = 'RowReorder',
                  selection = 'multi',
                  autoHideNavigation = TRUE,
                  editable = TRUE,
                  options = list(
                    order = list(list(0, 'asc')), 
                    rowReorder = TRUE,
                    dom = 'ti',
                    paging = FALSE)
    ) %>% formatStyle(
      0, cursor = 'row-resize') %>% formatStyle(
        2, backgroundColor = styleEqual(dfPreview$color, dfPreview$color)) 
  })
  
  ### Reorder datatable when user has dragged rows to reorder
  observeEvent(input$refGenomePreview_rows_all,{
    # reorder the dataframe according to the order the user has dragged
    l$refGenomeDF <- l$refGenomeDF[input$refGenomePreview_rows_all,]
    # renumber the dataframe rows accordingly
    row.names(l$refGenomeDF) <- 1:nrow(l$refGenomeDF)
  })
  
  ### Update data table when user edits a cell
  observeEvent(input$refGenomePreview_cell_edit,{
    # capture the edited cell coordinates and content
    info = input$refGenomePreview_cell_edit
    # update the dataframe accordingly -- but do not allow user to edit anything but first 2 columns.
    if(info$col > 4){
      showModal(modalDialog(
        title = "Cannot edit sequence.",
        "Please upload a new FASTA file instead.", easyClose = TRUE))
      l$refGenomeDF[info$row, info$col] <- l$refGenomeDF[info$row, info$col]
    } else if ((info$col == 4) & !(info$value %in% c("+/-", "-", "+"))){
      showModal(modalDialog(
        title = "Input error",
        "strandFilter must be one of 3 options:  +/- , + , -", easyClose = TRUE))
      l$refGenomeDF[info$row, info$col] <- l$refGenomeDF[info$row, info$col]
    } else {
      l$refGenomeDF[info$row, info$col] <- info$value
    }
   
  })
  
  ### Apply colour pallete when user clicks on Apply
  applyColourPallete <- observeEvent(input$genomeApplyColours,{
    l$refGenomeDF$color <- assignGenomeColours(nrow(l$refGenomeDF), input$genomeColourScheme1, input$genomeColourScheme1_numSegs, input$genomeColourScheme2)
  })
  
  ### Let user know which rows are selected
  output$refGenome_rowsSelected <- renderText({
    if (isTruthy(input$refGenomePreview_rows_selected)){
      c("Selected: ", paste(l$refGenomeDF$segment[input$refGenomePreview_rows_selected], sep=", ", collapse=", "))
    } else {
      paste("No rows selected.")
    }
  })
  output$refGenome_rowsSelected_forStrand <- renderText({
    if (isTruthy(input$refGenomePreview_rows_selected)){
      c("Selected: ", paste(l$refGenomeDF$segment[input$refGenomePreview_rows_selected], sep=", ", collapse=", "))
    } else {
      paste("No rows selected.")
    }
  })
  
  
  ### Apply custom colour to given row
  applyCustomColour <- observeEvent(input$genomeCustomColour_apply,{
    l$refGenomeDF$color[input$refGenomePreview_rows_selected] <- input$genomeCustomColour_colour
  })
  
  ### Apply strand to selected row
  
  applyStrand <- observeEvent(input$genomeSelectStrand_apply,{
    l$refGenomeDF$strandFilter[input$refGenomePreview_rows_selected] <- input$genomeSelectStrand_strand
  })
  
  
  ### Upload BED feature table
  observeEvent(input$refGenomeBED, {
    loadedTable <- tryCatch({read.table(input$refGenomeBED$datapath, header=TRUE, na.strings="")},
                     error = function(e){
                       showModal(modalDialog(
                         title = "Cannot Read BED File",
                         "Please check the format of your file and try again."))
                       return(NULL)
                     })
    req(loadedTable)
    l$refGenomeBED <- loadedTable
  })
  
  
  ### Download FASTA file
  output$downloadFasta <- downloadHandler(
    filename = function() {
      paste('refGenome.fasta', sep='')
    },
    content = function(con) {
      
      outDF <- select(l$refGenomeDF, segment, seq)
      if(input$downloadFasta_selectedSegsOnly == TRUE){
        if(isTruthy(input$refGenomePreview_rows_selected)){
          outDF <- outDF[input$refGenomePreview_rows_selected,]
        } else {
          showNotification("No rows selected... writing whole table.", type = "error", duration=10)
        }
        
      }
      if(input$downloadFasta_convertToUpper == TRUE) {
        outDF$seq <- str_to_upper(outDF$seq)
      }
      # Create DNAStringSet Object to write fasta file
      outDNA <- DNAStringSet(outDF$seq)
      names(outDNA) <- outDF$segment
      
      if(input$downloadFasta_convertToRevComp == TRUE){
        outDNA <- reverseComplement(outDNA)
      }
      
      # Write to FASTA file
      writeXStringSet(outDNA, con)
    }
  )


  
  ### Download BED feature table
  output$refGenomeORFdownload <- downloadHandler(
    filename = function() {
      paste('featureTable.txt', sep='')
    },
    content = function(con) {
      write.table(l$refGenomeBED, con, quote=FALSE, row.names=FALSE, col.names=TRUE, sep='\t')
    }
  )
  
  ### Render BED table
  output$refGenomeFeatures <- renderTable({
    shiny::validate(
      need(l$refGenomeBED, "Please upload a BED feature file.")
    )
    l$refGenomeBED
    })
  
  
  ### Download refGenome Table RDS file ###
  output$downloadRefGenomeRDS <- downloadHandler(
    filename = function() {
      paste(input$refGenomeFile$name, '.refGenome.rds', sep='')
    },
    content = function(con) {
      req(l$refGenomeDF)
      rgObj <- as.list(l)
      saveRDS(rgObj, con)
    }
  )
  
  ### Upload refGenome Table RDS file ###
  processUploadRefGenomeTableFile <- observeEvent(input$refGenomeRDS,{
    rgObj <- tryCatch({
                    rgObj <- readRDS(input$refGenomeRDS$datapath)
                    if(class(rgObj) == "list"){
                      l$refGenomeDF <- rgObj[[1]]
                      l$refGenomeBED <- rgObj[[2]]
                    } else {   # legacy files
                      l$refGenomeDF <- rgObj
                    }
                    rgObj
                    },
                    error = function(e){
                      showModal(modalDialog(
                        title = "Cannot Read RDS File",
                        "Please check the format of your file and try again."))
                      return(NULL)
                      })
    req(rgObj)
  })
  


  updateDataFn = function(new_Data) {
    l$refGenomeDF <- new_Data$refGenomeDF
  }
  
  ### Return the refGenome list back to the main server function
  return(reactive({
    list(updateData = updateDataFn, data=reactiveValuesToList(l))
  }))

}
### END OF SERVER FUNCTION ###





### Helper Functions used in server code

assignGenomeColours <- function(numSegments, scheme1, num1, scheme2){
  colours1 <- assignColorPallete(scheme1, min(numSegments,num1))
  if ( (numSegments - num1) > 0){
    colours2 <- assignColorPallete(scheme2, (numSegments - num1))
  } else {
    colours2 <- NULL
  }
  return(c(colours1, colours2))
}

assignColorPallete <- function(scheme, number){
  # If the user has selected a custom pallete that is hard-coded.
  if (substr(scheme, 1,2) == "c."){
    if (scheme == "c.Bernadeta"){
      pallete <- c("#35899a","#6a5d7a", "#da727e", "#f9bd7f", "#ffeebd", "#d7da8f", "#bbdbbe", "#67c1bf")
    } else if (scheme == "c.GreyOnly"){
      pallete <- rep("#cccccc", 25)
    } else if (scheme == "c.Kevin-16"){
      pallete <-c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", "gold1", "skyblue2", "palegreen2", "#FDBF6F", "gray70", "maroon", "orchid1", "darkturquoise", "darkorange4", "brown")
    } else if (scheme == "c.Kevin-25") {
      pallete <- c("dodgerblue2","#E31A1C", "green4","#6A3D9A", "#FF7F00","black","gold1","palegreen2","#CAB2D6", "#FDBF6F", "gray70", "khaki2","maroon","orchid1","deeppink1","blue1","steelblue4","darkturquoise","green1","yellow4","yellow3","darkorange4","brown")
    }
    repeatsRequired <- ceiling(number/length(pallete))
    colors <- rep(pallete, repeatsRequired)
    colors <- colors[1:number]
  } else {
    pallete <- brewer.pal(number, scheme)
    # colorBrewer returns less than the value of numbers sometimes, so repeat if desired
    # ...could also force interpolation with colorRampPalette(pallete)(number), but this is not implemented here.
    if (length(pallete) < number) {
      repeatsRequired <- ceiling(number/length(pallete))
      colors <- rep(pallete, repeatsRequired)
      colors <- colors[1:number]
    } else {
      colors <- pallete
    }
  }
  colors <- colors[1:number]
  return(colors)
}


