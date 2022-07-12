server <- function(input, output,session) {
  ##preprocess; check credential
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "pw.sqlite",
      passphrase = key_get("R-shinymanager-key", "testkey")
      # passphrase = "passphrase_wihtout_keyring"
    )
  )
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  
  output$tab1UI <- renderUI({
    tagList(fluidRow(column(width = 12,
                            fluidRow(box( title = "Sample information", status = "primary", width = 12, solidHeader = T, collapsible = T,
                                          fluidRow(column(4,
                                                          p(
                                                            HTML("<b>PhenoDB sample ID</b>"), span(shiny::icon("info-circle"), id = "info_bh_id"), textInput(inputId="BH_id",label = NULL),
                                                            tippy::tippy_this(elementId = "info_bh_id", tooltip = "e.g.,BH11001-1", placement = "right")
                                                          )),
                                                   column(4,
                                                          p(
                                                            HTML("<b>Internal sample ID</b>"), 
                                                            span(shiny::icon("info-circle"), id = "info_alter_id"), textInput(inputId="BAB_id",label = NULL),
                                                            tippy::tippy_this(elementId = "info_alter_id", tooltip = "e.g.,BAB or internal id", placement = "right"))),
                                                   column(4,
                                                          p(
                                                            HTML("<b>Requester email</b>"), 
                                                            span(shiny::icon("info-circle"), id = "info_email_id"), textInput(inputId="email_address",label = NULL),
                                                            tippy::tippy_this(elementId = "info_email_id", tooltip = "Email address", placement = "right")))),
                                          fluidRow(column(4,
                                                          p(HTML("<b>Data type</b>"), span(shiny::icon("info-circle"), id = "info_seq_id"), selectInput(inputId = "seq_id",label = NULL,
                                                                                                                                                        c("Genome Sequencing"="wgs","Clinical Exome Sequencing"="wes"),selected = "wes"),
                                                            tippy::tippy_this(elementId = "info_seq_id", tooltip = "e.g.,WES or WGS", placement = "right"))),
                                                   column(4,
                                                          p(HTML("<b>Clinical Lab</b>"), span(shiny::icon("info-circle"), id = "info_lab_id"), textInput(inputId = "lab_id",label = NULL,placeholder = "Baylor Genetics"),
                                                            tippy::tippy_this(elementId = "info_lab_id", tooltip = "e.g.,BG,GeneDx", placement = "right"))),
                                                   column(4,
                                                          p(
                                                            HTML("<b>Prefered reference genome to map</b>"), span(shiny::icon("info-circle"), id = "info_ref_id"),selectInput(inputId = "ref_id",label = NULL,
                                                                                                                                                                              c("GRCh38"="hg38","GRCh37"="hg19"),selected = "hg19"),
                                                            tippy::tippy_this(elementId = "info_ref_id", tooltip = "version of reference mapped", placement = "right")
                                                          ))),
                                          fluidRow(column(4,fileInput("batch_tbl",label = "Batch upload",accept=c("*.csv"),multiple = F,buttonLabel = "Browse..."))),
                                          fluidRow(column(2,actionButton("btn_validate", "Validate Record")),
                                                   column(2,actionButton("btn_insert", "Insert Record"))))))),
            fluidRow(column(12,
                            fluidRow(box(title = "Sample in database", status = "success", width = 12, solidHeader = T, collapsible = T,
                                         DT::dataTableOutput("sample_meta_tbl")
                                         ))))
            )
    
  })
  output$tab2UI <- renderUI({
    fluidRow(
      column(width = 12,
             fluidRow(
               box(
                 title = "Filter parameters", status = "primary", width = 12, solidHeader = T, collapsible = T,
                 fluidRow(
                   column(
                     6,
                     p(
                       HTML("<b>Gene</b>"), span(shiny::icon("info-circle"), id = "info_gene"), textInput("gene",label=NULL),
                       tippy::tippy_this(elementId = "info_gene", tooltip = "Selected gene", placement = "right")
                     )
                   )
                 ),
                 fluidRow(
                   useWaiter(),
                   column(2, actionButton("btn_filter", "Filter"))
                 )
               )
             )),
      column(width = 12,
             fluidRow(
               box(
                 title = "Table", width = 12, solidHeader = T, status = "success", collapsible = T,
                 DT::dataTableOutput("gene_variant_table")))))
    })
  
  # Reavtive Values --------------------------
  values <- reactiveValues()
  
  values$data <- data.frame(stringsAsFactors = F)
  values$work_data <- data.frame(stringsAsFactors = F)
  values$meta_tbl <- sample_metatable
  # Tab1
  output$sample_meta_tbl <- DT::renderDataTable({
    values$meta_tbl
  },
  extensions=c("Responsive","Buttons"),
  server = T,filter = list(position = 'top', clear = T),
  options = list(dom = 'Bfrtip',
                 buttons = list(list(extend = "csv",text="Download Current Page",filename = input$gene,
                                     exportOptions = list(
                                       modifier = list(page = "current")
                                     )),
                                list(extend = "csv",text="Download Full Result",filename = input$gene,
                                     exportOptions = list(
                                       modifier = list(page = "all")
                                     )))))
  
  # Tab2
  # button to filter range---------
  observeEvent(input$btn_filter,{
    gene <- input$gene
    values$work_data <- data%>%
      filter(Gene.refGene==gene)%>%
      collect()
    selectkey <- values$work_data$key
    values$work_data <- tbl(conn,"hg19_WES_allele_tbl")%>%
      filter(key%in%selectkey)%>%
      collect()%>%merge(.,values$work_data,by="key")
  })
  output$gene_variant_table <- DT::renderDataTable({ 
    values$work_data
  },
  extensions=c("Responsive","Buttons"),
  server = T,filter = list(position = 'top', clear = T),
  options = list(dom = 'Bfrtip',
                 buttons = list(list(extend = "csv",text="Download Current Page",filename = input$gene,
                                     exportOptions = list(
                                       modifier = list(page = "current")
                                     )),
                                list(extend = "csv",text="Download Full Result",filename = input$gene,
                                     exportOptions = list(
                                       modifier = list(page = "all")
                                     ))
                                )))
  
  

  
}