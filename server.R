server <- function(input, output,session) {
    # Reavtive Values --------------------------
    values <- reactiveValues()
    values$snp_gvcf_file_ref <- vector()
    values$snp_gvcf_file_path <- vector()
    values$snp_gvcf_table <- data.frame(stringsAsFactors = F)
    ranges <- reactiveValues(x = NULL, y = NULL, chr = NULL)

    volumes <- c(Home="~/Downloads/","R installation" = R.home(),shinyFiles::getVolumes()())

    w <- waiter::Waiter$new(html = spin_3(),
                            color = transparent(.5))

    # matching ui module
    mod_snp_upload_Server("snv",volumes=volumes,values)

    # select reference
    observeEvent(input$ref,{
        if(input$ref=="hg38"){
            values$p1_file <-  "MANE.GRCh38.v1.0.refseq.gz.parquet"
        }else if(input$ref=="hg19"){
            values$p1_file <-  "NCBI_RefSeq_hg19_clean.bed.parquet"
        }
    })

    ## btn_goto
    observeEvent(input$btn_go,{
        req(!is.null(input$goto_reg))
        geneExtend <- input$geneExtend
        #req(!is.null(values$snp_gvcf_file_path))
        #browser()
        str <- stringr::str_trim(input$goto_reg)
        str <- strsplit(str,":|-|_")
        x <- vector()
        if (length(str[[1]]) == 1) {
            showNotification("Looking up gene name", type = "message")
            search <- as.character(unlist(strsplit(str[[1]],";|,")))
            path <- "./data/"
            p1_file <- values$p1_file
            RefSeq <- arrow::read_parquet(paste0(path,p1_file),as_data_frame = F)
            #search <- as.character(str[[1]])
            snp_gvcf_file_path=values$snp_gvcf_file_path
            ref_genome <- input$ref
            browser()
            found <- RefSeq %>%
                filter(gene_id%in%search)%>%collect()
            if (nrow(found) != 0){
                gene.list <- split.data.frame(found,f=as.factor(found$gene_id))
            }else{
                showNotification("Gene not found", type = "error")
            }
        }
        else if (length(str[[1]]) == 2){
            showNotification("Jumping to coordinates", type = "message")
            chr <- as.character(str[[1]][1])
            from <- as.numeric(str[[1]][2])-500000
            if (from < 0){from <- 0}
            to <- as.numeric(str[[1]][2])+500000
            x <- c(from, to)
        }
        else if (length(str[[1]]) == 3){
            showNotification("Jumping to coordinates", type = "message")
            chr <- as.character(str[[1]][1])
            from <- as.numeric(str[[1]][2])
            to <- as.numeric(str[[1]][3])
            x <- c(from, to)
        }
        if(length(x)!=0&length(values$snp_gvcf_file_path)!=0){
            if(!chr%in%values$snp_gvcf_file_ref){
                if(chr%in%chrom_id){
                    chr <- names(chrom_id)[which(chrom_id==chr)]
                }else{
                    chr <- chrom_id[which(names(chrom_id)==chr)]
                }
            }
            range.gr <- GenomicRanges::GRanges(chr,ranges = IRanges(x[1],x[2]))
            w$show()
            browser()
            values$snp_gvcf_table_filter <- ReadGVCF(values$snp_gvcf_file_path,ref_genome=input$ref,param = range.gr)%>%
                as.data.frame()
            w$hide()

        }
        if(length(x)!=0&nrow(values$snp_gvcf_table)!=0){
            if(!chr%in%values$snp_gvcf_file_ref){
                if(chr%in%chrom_id){
                    chr <- names(chrom_id)[which(chrom_id==chr)]
                }else{
                    chr <- chrom_id[which(names(chrom_id)==chr)]
                }
            }
            w$show()
            #browser()
            values$snp_gvcf_table_filter <- values$snp_gvcf_table%>%
                filter(Chr%in%chr,Start>x[1],Start<x[2])
            w$hide()
        }
        if(length(gene.list)!=0&nrow(values$snp_gvcf_table)!=0){
            w$show()
            values$snp_gvcf_table_filter <- rbindlist(lapply(gene.list,function(x){
                    chr <- as.character(unique(found$seqname))
                    gene_range <- c(as.numeric(min(x$start)-geneExtend),
                                    as.numeric(max(x$end)+geneExtend))
                    values$snp_gvcf_table%>%
                        filter(Chr%in%chr,Start>gene_range[1],Start<gene_range[2])
                }))
            w$hide()
        }
        if(length(gene.list)!=0&length(values$snp_gvcf_file_path)!=0){
            w$show()
            values$snp_gvcf_table_filter <- rbindlist(lapply(gene.list,function(x){
                chr <- as.character(unique(found$seqname))
                if(!chr%in%values$snp_gvcf_file_ref){
                    if(chr%in%chrom_id){
                        chr <- names(chrom_id)[which(chrom_id==chr)]
                    }else{
                        chr <- chrom_id[which(names(chrom_id)==chr)]
                    }
                }

                range.gr <- GenomicRanges::GRanges(chr,ranges = IRanges(as.numeric(min(x$start)-geneExtend),
                                                                        as.numeric(max(x$end)+geneExtend)))
                values$snp_gvcf_table <- ReadGVCF(values$snp_gvcf_file_path,ref_genome=input$ref,param = range.gr)%>%
                    as.data.frame()
            }))
            w$hide()
        }


    }, ignoreInit = F)

    ##output table
    output$filter_snv_table <- renderDataTable({
        req(nrow(values$snp_gvcf_table_filter) != 0)
        values$snp_gvcf_table_filter
    },extensions=c("Responsive","Buttons"),
    server = T,
    editable = TRUE,
    filter = list(position = 'top', clear = T),
    options = list(dom = 'Bfrtip',buttons = c('tsv','csv','excel')))


}
