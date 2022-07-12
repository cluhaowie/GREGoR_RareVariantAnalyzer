ui <- dashboardPage(
  dashboardHeader(title = "GREGoR RareVariantAnalyzer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "insert_value", text = "Insert Entries", icon = icon("edit")),
      menuItem(tabName = "variant_table", text = "Variant Table", icon = icon("search")))),
  dashboardBody(
    fluidRow(
      tabItems(
        tabItem(tabName = "insert_value",uiOutput("tab1UI")),
        tabItem(tabName = "variant_table",uiOutput("tab2UI")))))
)

ui <- secure_app(ui, enable_admin = TRUE)
