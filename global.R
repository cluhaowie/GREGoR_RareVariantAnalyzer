# Loading ----
library(dplyr)
library(data.table)
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinyFiles)
library(waiter)
library(tippy)
library(DT)
# for file processing
library(VariantAnnotation)
# Install missing packages from CRAN, 'arrow' may be a problem
list.of.packages <- c("dplyr", "data.table", "shiny", "shinydashboard","bs4Dash",
                      "tippy","DT","ggplot2","shinyWidgets","shinyFiles","waiter",
                      "BiocManager","arrow","shinydashboardPlus","bs4Dash", "colourpicker")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Install missing packages from Bioconductor
biocLitePackages <- c("GenomicRanges", "VariantAnnotation")
new.biocLitePackage <- biocLitePackages[!(biocLitePackages %in% installed.packages()[,"Package"])]
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
if(length(new.biocLitePackage)) BiocManager::install(new.biocLitePackage)

options(timeout = 6000)
options(shiny.maxRequestSize=3*1024^3) ## max file size 3 Gb
options(shiny.autoreload=TRUE)

geneExtend <- 1e5 # window size extend to 100kb

# use the module for file upload

mod_snp_upload_UI <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(width = 6,
                   fileInput(ns("file"),label = NULL, accept=c("*.vcf","*.vcf.gz"),multiple = F, buttonLabel = "Browse...")
            ),
            column(width=1,h5("or")),
            column(width = 4,
                   shinyFilesButton(id = ns("local_snp_file"), label = "Browse...", title = "Please select a file", multiple = F, viewtype = "detail")
            )
        )
    )
}

mod_snp_upload_Server <- function(id,volumes,values) {
    moduleServer(
        id,
        function(input, output, session) {
            shinyFileChoose(input, "local_snp_file", roots = volumes, session=session)
            observeEvent(input$file, {
                Rsamtools::indexTabix(input$file$datapath,format = "vcf")
                values[["snp_gvcf_file_ref"]] <- VariantAnnotation::scanVcfHeader(input$file$datapath)@reference
                values[["snp_gvcf_file_path"]] <- input$file$datapath
                showModal(modalDialog(title = "File upload",
                                      "The joint called SNP file has been uploaded and indexed"))
            })
            observeEvent(input$local_snp_file,{
                if(is.integer(input$local_snp_file)){
                    cat("no local snp file found \n")
                }else{
                    local_snp_file <- parseFilePaths(volumes, input$local_snp_file)
                    index.file <- paste0(local_snp_file$datapath,".tbi")
                    values[["snp_gvcf_file_ref"]] <- VariantAnnotation::scanVcfHeader(local_snp_file$datapath)@reference
                    values[["snp_gvcf_file_path"]] <-  local_snp_file$datapath
                    if(!file.exists(index.file)){
                        Rsamtools::indexTabix(local_snp_file$datapath,format = "vcf")
                    }
                    showModal(modalDialog(title = "File upload",
                                          paste0("The joint called SNP file has been indexed")))
                }
            },ignoreInit = T)

        }
    )
}

ReadGVCF <- function(path_to_gVCF,ref_genome=ref_genome,param = param){
    print("Grabbing regions")
    vcf<- VariantAnnotation::readVcf(file = path_to_gVCF,genome = ref_genome,param = param)
    vcf.gr <- vcf@rowRanges
    ranges <- as.data.frame(vcf.gr@ranges)
    strList <- strsplit(ranges$names,":|_|/")
    CHROM <- sapply(strList,"[[",1)
    POS <- sapply(strList,"[[",2)
    REF <- sapply(strList,"[[",3)
    ALT <- sapply(strList,"[[",4)
    GT <- VariantAnnotation::geno(vcf)$GT
    AD <- VariantAnnotation::geno(vcf)$AD
    DP <- VariantAnnotation::geno(vcf)$DP
    GT <- as.data.table(GT)
    AD <- as.data.table(AD)
    merged <- cbind(GT ,AD)
    anno <- data.table(CHROM,POS,ID=".",REF,ALT)
    anno <- cbind(anno,merged)
    return(anno)
}
chrom_id <- c(1:22,"X")
names(chrom_id) <- paste0("chr",chrom_id)

