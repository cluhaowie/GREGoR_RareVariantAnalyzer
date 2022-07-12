# Loading ----
library(dplyr)
library(data.table)
library(knitr)
library(testthat)
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyWidgets)
library(shinyjs)
library(waiter)
library(fs)
library(tippy)
# library(shinycssloaders)
# library(shinytest)
# library(htmltools)
library(DT)
# for file processing
library(ggplot2)
library(bedr)
library(RSQLite)
library(Rsamtools)
library(VariantAnnotation)
library(BSgenome.Hsapiens.UCSC.hg38)
library(shinymanager)
library(keyring)
library(arrow)
#library(validate)
#library(data.validator)

sqlitePath <- "va.sqlite"
conn <- DBI::dbConnect(RSQLite::SQLite(), sqlitePath)
data <- arrow::read_parquet(
  file = here::here("data/variant_anno_hg19_tbl.parquet"), 
  as_data_frame = FALSE   # keep as parquet table to keep back-end lighter
)
sample_metatable <- tbl(conn,"sample_metatable_hg38")%>%collect()
sample_metatable_rules <- validate::validator(is_unique(COLLABORATOR_SAMPLE_ID,INTERNAL_SAMPLE_ID, APPLICATION),
                                         !is.na(INTERNAL_SAMPLE_ID),
                                         !is.na(APPLICATION))

#sample_metatable <- fread("GREGoR Data Sharing Set 1_07062022.csv")
#values(validate::confront(sample_metatable, sample_metatable_rules))

InitialTable <- function(conn,Tablename,DF,key){
  require(DBI)
  s <- sprintf("create table %s(%s, primary key(%s))", Tablename,
               paste(names(DF), collapse = ", "),
               key)
  DBI::dbExecute(conn=conn, statement=s)
}
#dbWriteTable(conn,"sample_metatable_hg38",sample_metatable)

