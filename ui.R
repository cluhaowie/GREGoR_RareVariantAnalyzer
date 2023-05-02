ui <- dashboardPage(
  dashboardHeader(title = "GREGoR RareVariantAnalyzer"),
  dashboardSidebar(
    sidebarMenu(
        menuItem(tabName = "input", text = "Input/Filtering", icon = icon("upload"))
      )
    ),
  dashboardBody(
      tabItems(
        tabItem(tabName = "input",
                fluidRow(
                    column(width=12,
                           box(title="Step 0: File import/upload",status="primary",width = 12,solidHeader = T,collapsible = T,
                               radioButtons(inputId = "ref",label = h3("Reference Genome Build"),choices = list("hg19"="hg19","GRCh38"="hg38"),inline = T,selected = "GRCh38"),
                               h5("Upload or select snv vcf file",dashboardBadge("required", color = "primary")),
                               mod_snp_upload_UI("snv")
                               )
                           )
                    ),
                fluidRow(
                    column(width=12,
                           box(title="Step 1: Select region",status="primary",width = 12,solidHeader = T,collapsible = T,
                               fluidRow(
                                   column(4,shiny::textInput("goto_reg",label = NULL,placeholder = "gene symbol, chromosome location/range")),
                                   column(1,shiny::actionButton("btn_go","go"))
                                   ),
                               fluidRow(column(4,sliderInput("geneExtend", label = "Flanking Gene regions (bp)",
                                                             min = 0,max = 1e6, value = 1e5,step=1000)))

                               )

                           )
                    ),
                fluidRow(
                    column(width=12,
                           box(title="Step 2: view variants",status="primary",width = 12,solidHeader = T,collapsible = T,
                               fluidRow(
                                       DT::dataTableOutput("filter_snv_table")
                                   ),
                               fluidRow(
                                   column(1,uiOutput("ui_dlbtn_tbl"))
                               )
                           )
                    )
                )
                )
        )
      )
)

