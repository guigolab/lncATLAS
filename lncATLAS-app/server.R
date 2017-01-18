################################################################################
################################################################################
#                             lncATLAS -- Server
#
#         author: David Mas-Ponte @CRG
#         R Shiny app
#
################################################################################
################################################################################


# INDEX

# 0. LOADING PACKAGES
# 1. Reading data
# 2. Server Function


## 0) LOADING PACKAGES

library(shiny)
library(ggplot2)
source("mysqlHelper.R")
source("ggThemes.R")
source("helper.R")
library(dplyr)

# library(plotly)




# 1) READING DATA

# I need this in order to set up the selectize input.


# 2) Server function

shinyServer(function(input, output,session) {

isolate(sess_id <- paste("session",as.numeric(Sys.time()),sep="_"))
isolate(dir.create(sess_id))
# When a session ends, decrement the counter.
session$onSessionEnded(function(){
# We use isolate() here for the same reasons as above.
isolate(unlink(sess_id,recursive = TRUE))
})

  output$retrieve <- renderTable({
    gene.ids <- unlist(strsplit(input$listretrive,"\n"))
    table.out <- getAllfromIDvec(gene.ids)
    table.out <- table.out[,c(1:4,7:9)]
    colnames(table.out) <- c("ENSEMBL ID","Data Source","Data Type",
                             "Value","Gene Name","Coding Type",
                             "Biotype")
    if (input$downltype == 1){
      table.out = table.out[grep("ratio*",table.out[,"Data Type"]),]
      a = table.out[,"Data Type"]
      a = gsub("ratio2","CNRCI",a)
      a = gsub("ratioK","RCI",a)
      table.out[,"Data Type"] = a
    } else if (input$downltype == 3){
      table.out = table.out[-grep("ratio*",table.out[,"Data Type"]),]
      a = table.out[,"Data Type"]
      a = gsub("cytosol","cytoplasm",a)
      table.out[,"Data Type"] = a
    } else{
      a = table.out[,"Data Type"]
      a = gsub("ratio2","CNRCI",a)
      a = gsub("ratioK","RCI",a)
      a = gsub("cytosol","cytoplasm",a)
      table.out[,"Data Type"] = a
    }
    table.out
  })



  output$retrieveall <- downloadHandler(
    filename = function() {
      fn <- paste(Sys.Date(),"lncATLAS","all","data",sep="_")
      if (input$downltype == 1){
        fn <- paste(fn,"RCI",sep="_")
      } else if (input$downltype == 3){
        fn <- paste(fn,"expression",sep="_")
      } else{}
      paste(fn, '.csv', sep='')},
    content = function(file){
      progress <- shiny::Progress$new()
      progress$set(message = "Querying the Database", value = 0.01)
      query.all <- "SELECT genes_ensembl_gene_id,
      data_source_expression_sites_name,
       data_source_data_types_name,
         expression_value,
      gene_name,
      coding_type,
      coding_type
    FROM expression
    INNER JOIN genes ON genes_ensembl_gene_id = ensembl_gene_id"
      if (input$downltype == 1){
        query.all <- paste0(query.all, " WHERE data_source_data_types_name REGEXP 'ratio*';")
      } else if (input$downltype == 3){
        query.all <- paste0(query.all, " WHERE data_source_data_types_name NOT REGEXP 'ratio*';")
      } else{query.all <- paste0(query.all,";")}
      progress$set(message = "Conecting to the Database", value = 0.25)
      cn <- lncatlasConnect()
      progress$set(message = "Retrieving the Data", value = 0.5)
      table.out <- DBI::fetch(dbSendQuery(cn, query.all),n=-1)
      suppressWarnings(dbDisconnect(cn))
      colnames(table.out) <- c("ENSEMBL ID","Data Source","Data Type",
                               "Value","Gene Name","Coding Type",
                               "Biotype")

      if (input$downltype == 1){
        a = table.out[,"Data Type"]
        a = gsub("ratio2","CNRCI",a)
        a = gsub("ratioK","RCI",a)
        table.out[,"Data Type"] = a
      }

      on.exit(progress$close())
      progress$set(message = "Writing the data", value = 0.75)
      write.table(table.out,file,quote=FALSE,sep=',',row.names = FALSE)
    })


  # this is used to wait the user press the go button.
  re.id <- eventReactive(input$go,{
    ## progres bar
    progress <<-shiny::Progress$new()
    #unlist the input   geneIdtmp
    if (grepl("^ENSG[0-9]+",input$geneId)){
      ids_tmp <- unlist(strsplit(input$geneId,","))
    } else if (grepl("^ENSG[0-9]+",input$geneIdtmp)) {
      ids_tmp <- unlist(strsplit(input$geneIdtmp,","))
    } else {
      ids_tmp <- unlist(strsplit(input$geneId,","))
    }

    if (all(grepl("^ENSG[0-9]+",ids_tmp))){
      if (length(ids_tmp)<4){
        return(ids_tmp)
      } else{
        return(ids_tmp[1:3])
      }
    } else {
      return(ids_tmp[grepl("^ENSG[0-9]+",ids_tmp)])
    }
    })

  # function to store the K distribution. Thinking in moving it out of this
  # scope
  get.distroK <- function(){

    cl <- "K562"
    cond <- paste0("(data_source_data_types_name REGEXP 'ratioK+') ",
                   "AND ",
                   "data_source_expression_sites_name = '",
                   cl,"' AND expression_value IS NOT NULL")

    ij <- paste0("genes",
                 " INNER JOIN ",
                 "expression",
                 " ON ensembl_gene_id = genes_ensembl_gene_id")
    columns <- "ensembl_gene_id AS geneid,expression_value,data_source_data_types_name AS source,coding_type,bio_type"

    query <- paste0(
      "SELECT ",
      columns,
      " FROM ",
      ij,
      " WHERE ",
      cond,
      ";"
    )



    cn <- lncatlasConnect()
    distr.df <- DBI::fetch(dbSendQuery(cn, query),n=-1)
    suppressWarnings(dbDisconnect(cn))

    return(distr.df)
  }

  get.distro.all <- reactive({
    cond <- paste0("(data_source_data_types_name = 'ratio2' OR ",
                   "data_source_data_types_name = 'cell') ",
                   "AND "," expression_value IS NOT NULL")

    ij <- paste0("genes",
                 " INNER JOIN ",
                 "expression",
                 " ON ensembl_gene_id = genes_ensembl_gene_id")
    columns <- "ensembl_gene_id AS geneid,expression_value,data_source_data_types_name AS source,coding_type,bio_type, data_source_expression_sites_name AS cellline"

    query <- paste0(
      "SELECT ",
      columns,
      " FROM ",
      ij,
      " WHERE ",
      cond,
      ";"
    )

    cn <- lncatlasConnect()
    distr.df <- DBI::fetch(dbSendQuery(cn, query),n=-1)
    suppressWarnings(dbDisconnect(cn))
    distr.df
  })





  get.distro <- reactive({
    cl <- input$cellLine
    cond <- paste0("(data_source_data_types_name = 'ratio2' OR ",
                   "data_source_data_types_name = 'cell') ",
                   "AND ",
                   "data_source_expression_sites_name = '",
                   cl,"' AND expression_value IS NOT NULL")

    ij <- paste0("genes",
                 " INNER JOIN ",
                 "expression",
                 " ON ensembl_gene_id = genes_ensembl_gene_id")
    columns <- "ensembl_gene_id AS geneid,expression_value,data_source_data_types_name AS source,coding_type,bio_type"

    query <- paste0(
      "SELECT ",
      columns,
      " FROM ",
      ij,
      " WHERE ",
      cond,
      ";"
    )

    cn <- lncatlasConnect()
    distr.df <- DBI::fetch(dbSendQuery(cn, query),n=-1)
    suppressWarnings(dbDisconnect(cn))
    distr.df
  })

  current.cl <- eventReactive(input$go,{input$cellLine})

  output$ID <- renderUI({
    names = unlist(strsplit(input$e1,"[, \t]+"))
    ids = getID(names)

    if (grepl("^ENSG*",input$geneIdtmp)){
      oldids = unlist(strsplit(input$geneIdtmp,"[, \t]+"))
      ids <- c(ids,oldids)
    }

    if(length(ids)>3){
      return(
        column(10,offset=1,align="center",
          icon("remove", lib = "glyphicon", "fa-3x"),
          p("No more than 3 genes should be selected")
        )
      )
    } else {
      gene.id <- paste(ids,collapse = ",")

      if (!is.null(input$refnucl)) {
        nucl.id <- paste(getID(input$refnucl),collapse = ",")
        gene.id <- paste(gene.id,nucl.id, sep = ",")
      }

      if (!is.null(input$refcyto)){
        cyto.id <- paste(getID(input$refcyto),collapse = ",")
        gene.id <- paste(gene.id,cyto.id, sep = ",")
      }
      if (!is.null(input$refdual)){
        cyto.id <- paste(getID(input$refdual),collapse = ",")
        gene.id <- paste(gene.id,cyto.id, sep = ",")
      }
      gene.id <- as.character(gene.id)
      return(textInput("geneId", label = h3("Enter a Ensembl ID:"),
                  value = gene.id))
      }
  })


    #### PLOTS ####

  #### RATIO ####

  output$ratioPlot <- renderPlot({

    geneID <- re.id()
    geneID <- unlist(strsplit(geneID,","))
    # progress update
    progress$set(message = "Ratio Plot", value = 0.1)



    name <- getGeneName(geneID)
    name.title <- paste(name,collapse = " : ")

    geneID.cond <- paste("genes_ensembl_gene_id = '",geneID, sep="")
    geneID.str.cond <- paste(geneID.cond, collapse = "' OR ")

    cond <- paste0("((",geneID.str.cond,"') AND ",
                   "(data_source_data_types_name = 'ratio2' ",
                   "OR data_source_data_types_name = 'nucleus'",
                   "OR data_source_data_types_name = 'cytosol'))")

    cn <- lncatlasConnect()
    ratios.cell.df <- getTable(cn, "expression" ,whole_table = FALSE,
                       column = c("genes_ensembl_gene_id AS gene_id",
                         "data_source_expression_sites_name AS cellline",
                                  "expression_value AS value",
                                  "data_source_data_types_name AS source"),
                       condition = cond )

    suppressWarnings(dbDisconnect(cn))

    # I obtain the plot data frame by filter
    plot.df <- dplyr::filter(ratios.cell.df,source == 'ratio2')
    #plot.df$whole.cell <- dplyr::filter(ratios.cell.df,source == 'cell')$value
    d <- log2(dplyr::filter(ratios.cell.df,source == 'nucleus')$value)
    plot.df$d <- d

    # i select the na values
    nc <- dplyr::filter(ratios.cell.df,source == "nucleus" |
                          source == "cytosol")
    nc <- nc %>% group_by(gene_id,cellline) %>% mutate(pse =
                      (xor(value[1],value[2]) ) # checking pse
                      & all(!is.na(value))) #removing NA)
    na.df <- dplyr::filter(nc,pse) #removing NA
    label.df <- dplyr::filter(nc,!pse)
    label.df <- label.df %>% group_by(gene_id,cellline) %>%
      mutate(r = all(value != 0) & !is.na(value))
    label.df <- filter(label.df,r)
    label.df <- label.df %>% group_by(gene_id,cellline) %>%
      mutate(pos = setUpPos(value),c = getCol(value))

    pseudocounts.df <- na.df %>% group_by(gene_id,cellline) %>% summarise(
      rest = round(getRest(value),1) , value = getPSE(value), pos = getPos(value)
    )


    cls.vec <- unique(ratios.cell.df$cellline)
    ant.df <- data.frame(ratio = -Inf, cell.line = cls.vec[length(cls.vec)],
                         gene_id = geneID[order(geneID)][length(geneID)])

    if (dim(pseudocounts.df)[1] != 0){
      pseudocounts.df <- as.data.frame(pseudocounts.df)
      pse.gg <- geom_bar(
                 data = pseudocounts.df,
                 aes(y = value, x = cellline),
                 fill = "white",
                 color = "orange",
                 linetype = "dashed",
                 stat="identity",
                 position = "identity")
      pse.label.gg <- geom_text(
          data = pseudocounts.df,
          color = "orange",
          aes(y = pos,
              x = cellline,
              label = paste(rest, "FPKM",sep=" ")),
          angle = 90,
          size = 5
        )
    } else {
      pse.gg <- NULL
      pse.label.gg <- NULL
    }

    if (exists("label.df")){
      label.gg <- geom_text(
        data = label.df,
        aes(x=cellline,
            y=pos,
            label=paste(round(value,1), "FPKM",sep=" ")
            ),
        color = "#7F2704",
        size = rel(5),
        angle = 90
      )
     limits <- c(min(label.df$pos - 1.5),max(label.df$pos + 1.5))
    } else {
      label.gg = NULL
    }


    g <- ggplot() + geom_bar(aes(y = value, x = cellline, fill = d),
               data = plot.df,
               stat="identity", position = "identity") +
      pse.gg +
      pse.label.gg +
      label.gg +
      facet_grid(. ~ gene_id) +
      ggtitle(name.title) +
      theme_lncatlas_Ratio() +
      scale_fill_lncatlas_Ratio() +
      labs(x = "Cell lines",
           y = "CN RCI") +
      geom_text(data = ant.df, aes(x= cell.line, y= ratio),label = "lncATLAS",
               hjust=1, vjust=-1.1, col="black", cex=6,
               fontface = "bold", alpha = 0.8,size = 5) +
      ylim(limits) +
      guides(fill = guide_legend("Nuclear expression (log2(FPKM))"))
    path = paste(sess_id,"plotR1.pdf",sep="/")
    ggsave(path, g)
    progress$set(message = "Zoom in", value = 0.2)
    g
  })


  #### DISTRIBUTION ####

  output$distribution <- renderPlot({

    geneID <- re.id()

    geneID <- unlist(strsplit(geneID,","))

    name <- getGeneName(geneID)


    name.title <- paste(name,collapse = " : ")

    cl <- input$cellLine
    name.title <- paste(name.title,cl,sep = " in ")

    group <- "coding_type"

    geneID <- unlist(strsplit(geneID,","))

    geneID.cond <- paste("ensembl_gene_id = '",geneID, sep="")
    geneID.str.cond <- paste(geneID.cond, collapse = "' OR ")


    cond2 <- paste0("data_source_data_types_name = 'ratio2' AND ",
                   "data_source_expression_sites_name = '",
                   cl,
                   "' AND ( ",
                   geneID.str.cond,"')")
    ij <- paste0("genes",
                 " INNER JOIN ",
                 "expression",
                 " ON ensembl_gene_id = genes_ensembl_gene_id")
    columns <- "expression_value AS ratio,coding_type,bio_type"



    query2 <- paste0(
      "SELECT ",
      columns,
      " FROM ",
      ij,
      " WHERE ",
      cond2,
      ";"
    )

    cn <- lncatlasConnect()
    #distr.df <- DBI::fetch(dbSendQuery(cn, query),n=-1)
    gene.df <- DBI::fetch(dbSendQuery(cn, query2),n=-1)
    suppressWarnings(dbDisconnect(cn))





    distr.df <- get.distro()

    distr.df <- distr.df[distr.df$source == "ratio2",]



    gene.df$name <- name

    gene.value <- as.numeric(gene.df[,1])

    distr.nc <- distr.df[distr.df$coding_type == "nc",]
    distr.c <- distr.df[distr.df$coding_type == "coding",]
   extr <- extreme(distr.nc$expression_value,
                   gene.value)
   extr <- round(extr*100,1)

   n1 <- paste0("lncRNAs (n = ",
               dim(distr.df[distr.df$coding_type == "nc",])[1],
               ")")
   n2 <- paste0("mRNA (n = ",
                dim(distr.df[distr.df$coding_type == "coding",])[1],
                ")")
   ypos <- max(density(distr.c$expression_value)$y)

   distr.df$coding_type <- factor(distr.df$coding_type,
                                 levels = c("nc","coding"),ordered = TRUE)

   g <- ggplot(data=distr.df,
           aes(expression_value)) +
      geom_density(aes(color=distr.df[,group])) +
      stat_extreme(data=distr.nc,aes(x=expression_value,val=gene.value[1])) +
      stat_extreme(data=distr.nc,aes(x=expression_value,val=gene.value[2])) +
      stat_extreme(data=distr.nc,aes(x=expression_value,val=gene.value[3])) +
      geom_vline(xintercept=gene.value) +
      geom_text(
        data = gene.df,
        aes(x=ratio,y=0.2, label = paste(name,paste0(extr," %"),
                                         paste0("RCI =  ",round(ratio,1)),
                                         sep = " : ")),
        check_overlap = TRUE,
        angle = 90,
        vjust = -1.5,
        size = 5) +
      scale_y_continuous(limits=c(0, NA), expand = c(0, 0)) +
      ggtitle(name.title) +
      theme_lncatlas_distr() +
      scale_colour_Publication(labels = c(n1, n2)) +
      scale_fill_Publication() +
      guides(colour = guide_legend(title = "")) +
      annotate("text", x = -Inf , y = Inf,
               label = "lncATLAS",
               hjust=-0.8,vjust=1.1, col="black", cex=6,
               fontface = "bold", alpha = 0.8) +
      labs(x = "CN RCI ",
          y = "Density")
    path = paste(sess_id,"plotD1.pdf",sep="/")
    ggsave(path, g)
   progress$set(message = "Ratio Distro", value = 0.4)
   g
  })



  output$distributionAlt <- renderPlot({
    # General Distro block
    cond <- paste0("(data_source_data_types_name = 'ratio2'",
                   "AND "," expression_value IS NOT NULL)")

    ij <- paste0("genes",
                 " INNER JOIN ",
                 "expression",
                 " ON ensembl_gene_id = genes_ensembl_gene_id")
    columns <- "ensembl_gene_id AS geneid,expression_value,data_source_data_types_name AS source,coding_type,bio_type, data_source_expression_sites_name AS cellline"

    query <- paste0(
      "SELECT ",
      columns,
      " FROM ",
      ij,
      " WHERE ",
      cond,
      ";"
    )

    #cn <- lncatlasConnect()
    #distr.df <- DBI::fetch(dbSendQuery(cn, query),n=-1)
    #suppressWarnings(dbDisconnect(cn))

    distr.df <- get.distro.all()
    distr.df <- distr.df[distr.df$source == "ratio2",]


    # getting specifics genes
    geneID <- re.id()
    geneID <- unlist(strsplit(geneID,","))
    gene.df <- distr.df[ distr.df$geneid %in% geneID ,]
    name <- getGeneName(unique(gene.df$geneid))
    name.title <- paste(name,collapse = " : ")
    names(name) <- unique(gene.df$geneid)
    gene.df$name <- name[gene.df$geneid]
    #only the values we have information from
    distr.df <- distr.df[distr.df$cellline %in% unique(gene.df$cellline),]

    #extreme values
    distr.list <- list()
    distr.type.list <- list()
    for (i in unique(distr.df$cellline)){
      distr.list[[i]] <- distr.df[distr.df$cellline == i,]$expression_value
      distr.type.list[[i]][["coding"]] <-
        distr.df[distr.df$cellline == i &
                   distr.df$coding_type == "coding" ,]$expression_value
      distr.type.list[[i]][["nc"]] <-
        distr.df[distr.df$cellline == i &
                   distr.df$coding_type == "nc" ,]$expression_value
    }
    cls.vec <-  unique(gene.df$cellline)
    gene.list <- list()
    for (i in cls.vec){
      gene.list[[i]] <- gene.df[gene.df$cellline == i,]$expression_value
      names(gene.list[[i]]) <- gene.df[gene.df$cellline == i,]$geneid
    }

    extr.list <- extreme.multiple.nc(distr.type.list,gene.list)
    gene.df$extr <- unlist(extr.list)[paste(gene.df$cellline,
                                            gene.df$geneid,
                                            sep=".")]


    # computing the number of genes per plot

    n <- c()
    m <- c()
    cl <- c()
    ct <- c()
    ev <- c()
    for ( i in unique(distr.df$cellline)){
      for (j in c("nc","coding")){
        n <- c(n,length(distr.type.list[[i]][[j]]))
        m <- c(m, median(distr.type.list[[i]][[j]]))
        cl <- c(cl,i)
        ct <- c(ct,j)
        ev <- c(ev,max(distr.type.list[[i]][[j]]))
      }
    }
    n.df <- data.frame(n = n,
                       m = m,
                       cellline = cl,
                       coding_type = ct,
                       expression_value = ev)

    n.df[order(n.df$coding_type),]$expression_value <-
          c(rep(max(n.df$expression_value),dim(n.df)[1]/2),
            rep(max(n.df$expression_value + 1.5),dim(n.df)[1]/2))


    # ploting block


    distr.df$coding_type <- factor(distr.df$coding_type,
                         levels = c("nc","coding"),ordered = TRUE)
    dodge <- position_dodge(width = 0.8)
    ylimit = max(n.df$expression_value)

    g <- ggplot(distr.df,aes(cellline,expression_value,color = coding_type)) +
      geom_violin(position = dodge) +
      geom_boxplot(width=0.2,outlier.size = 0.05,position=dodge) +
      geom_point(data = gene.df, aes(shape = name ),size = 3) +
      geom_text(data = gene.df ,
                aes(label = paste0(round(extr*100,1),"%")),
                hjust=-0.5,size = 5,
                check_overlap = TRUE) +
      geom_text(data=n.df, aes(label=paste0("n = ",n)), position = "identity",
                vjust = -1,size = 5) +
      geom_text(data=n.df, aes(label=paste0("m = ",round(m,2))),
                position = "identity",
                vjust = -2.5,size = 5) +
      expand_limits(y=ylimit+1) +
      scale_colour_Publication(labels=c("lncRNA","mRNA")) +
      scale_shape_manual(values = c(8:(7+length(name)))) +
      theme_lncatlas_distrK() +
      theme(axis.text.x = element_text(angle=45,vjust=0.5),
	    axis.title.x = element_blank()) +
      labs(x="",
           y="CN RCI ") +
      annotate("text", x = cls.vec[length(cls.vec)] , y = -Inf,
               label = "lncATLAS",
               hjust=0.8, vjust=-1.1, col="black", cex=6,
               fontface = "bold", alpha = 0.8) +
      guides(color = guide_legend(title = ''),
             shape = guide_legend(title = 'Genes')) +
      ggtitle(name.title)
    path = paste(sess_id,"plotR2.pdf",sep="/")
    ggsave(path, g)
    progress$set(message = "2D plot", value = 0.6)
    g
  })

  #### DISTRIBUTION 2D ####


  output$distribution2D <- renderPlot({

    geneID <- re.id()
    cl <- input$cellLine
    group <- "coding_type"

    geneID <- unlist(strsplit(geneID,","))

    name <- getGeneName(geneID)


    name.title <- paste(name,collapse = " : ")
    name.title <- paste(name.title,cl,sep =" in " )


    geneID.cond <- paste("ensembl_gene_id = '",geneID, sep="")
    geneID.str.cond <- paste(geneID.cond, collapse = "' OR ")

    cond2 <- paste0("(data_source_data_types_name = 'ratio2' OR ",
                    "data_source_data_types_name = 'cell') ",
                    "AND ",
                    "data_source_expression_sites_name = '",
                    cl,
                    "' AND ( ",
                    geneID.str.cond,"')")
    ij <- paste0("genes",
                 " INNER JOIN ",
                 "expression",
                 " ON ensembl_gene_id = genes_ensembl_gene_id")
    columns <- "expression_value,data_source_data_types_name AS source,coding_type,bio_type"


    query2 <- paste0(
      "SELECT ",
      columns,
      " FROM ",
      ij,
      " WHERE ",
      cond2,
      ";"
    )

    cn <- lncatlasConnect()
    gene.df <- DBI::fetch(dbSendQuery(cn, query2),n=-1)
    suppressWarnings(dbDisconnect(cn))

    #validation function
    validate(
      need(try("cell" %in% gene.df$source),
           "There is no whole cell data for this cell line.")
    )

    distr.df <- get.distro()
    #### ERR
    cell.genes <- distr.df[distr.df$source == "cell" ,]$geneid
    ratio.genes <- distr.df[distr.df$source == "ratio2",]$geneid
    unique.genes <- intersect(cell.genes,ratio.genes)


    ncgenes <- distr.df[distr.df$coding_type == "nc",]$geneid
    cgenes <- distr.df[distr.df$coding_type == "coding",]$geneid

    l1 <- length(unique.genes[unique.genes %in% ncgenes])
    l2 <- length(unique.genes[unique.genes %in% cgenes])

    n1 <- paste0("lncRNA (n = ",
                 l1,
                 ")")
    n2 <- paste0("mRNA (n = ",
                 l2,
                 ")")




    plot.df <- distr.df[distr.df$source == "cell" &
                          distr.df$geneid %in% unique.genes ,]
    plot.df$ratio <- distr.df[distr.df$source == "ratio2" &
                                distr.df$geneid %in% unique.genes,]$expression_value
    plot.df$coding_type <- factor(plot.df$coding_type,
                                  levels = c("nc","coding"),ordered = TRUE)
    plot2.df <- gene.df[gene.df$source == "cell",]
    plot2.df$ratio <- gene.df[gene.df$source == "ratio2",]$expression_value
    plot2.df$name <- name

    g <- ggplot(data=plot.df,
           aes(x=log10(expression_value),y=ratio)) +
      geom_density2d(aes(colour = plot.df[,group])) +
      geom_point(
        data = plot2.df,
        aes(x=log10(expression_value),y=ratio),
        size = 3) +
      geom_text(
        data = plot2.df,
        aes(x=log10(expression_value),y=ratio,
            label = paste(name,
                          paste0(round(expression_value,1)," FPKM"),
                          paste0("RCI: ",round(ratio,1)),
                          sep=" \n ")),
        check_overlap = TRUE,
        vjust = -0.5, size = 5) +
      ggtitle(name.title) +
      theme_lncatlas_2Ddistr() +
      scale_colour_Publication(labels=c(n1,n2)) +
      guides(colour = guide_legend(title = "")) +
      labs(x = "Cell expression (log10(FPKM)) ",
           y = "CN RCI ") +
      scale_y_continuous(expand = c(0.05, 1)) +
      annotate("text", x = Inf, y = -Inf, label = "lncATLAS",
               hjust=1.1, vjust=-1.1, col="black", cex=6,
               fontface = "bold", alpha = 0.8)
    path = paste(sess_id,"plotD2.pdf",sep="/")
    ggsave(path, g)
    progress$set(message = "K distro", value = 0.8)
    g
  })




  gene.table <- reactive({
    geneID <- re.id()
    geneID <- unlist(strsplit(geneID,","))

    name <- getGeneName(geneID)
    geneID.cond <- paste("genes_ensembl_gene_id = '",geneID, sep="")
    geneID.str.cond <- paste(geneID.cond, collapse = "' OR ")



    ij <- paste0("genes",
                 " INNER JOIN ",
                 "expression",
                 " ON ensembl_gene_id = genes_ensembl_gene_id")

    columns <- "ensembl_gene_id AS geneid,expression_value,data_source_data_types_name AS data_type,data_source_expression_sites_name AS cell_line,coding_type,bio_type"

    query <- paste0(
      "SELECT ",
      columns,
      " FROM ",
      ij,
      " WHERE ",
      geneID.str.cond ,
      "';"
    )

    cn <- lncatlasConnect()
    gene.df <- DBI::fetch(dbSendQuery(cn, query),n=-1)
    suppressWarnings(dbDisconnect(cn))

    n <- dim(gene.df)[1]/length(name)
    names <- c()
    for (i in name) {
      names <- c(names,rep(i,n))
    }

    gene.df$name <- names
    gene.df
  })


  output$downloadData <- downloadHandler(
    filename = function() {
      geneID <- re.id()
      fn <- paste(unlist(strsplit(geneID,",")),collapse = "_")
      paste(fn, '.csv', sep='')
    },
    content = function(file) {
      write.csv(gene.table(), file)
    }
  )


  output$downloadData2 <- downloadHandler(
    filename = function() {

      fn <- paste(Sys.Date(),"lncATLAS",collapse="_")
      paste(fn, '.csv', sep='')
    },
    content = function(file) {
      gene.ids <- unlist(strsplit(input$listretrive,"\n"))
      table.out <- getAllfromIDvec(gene.ids)
      table.out <- table.out[,c(1:4,7:9)]
      colnames(table.out) <- c("ENSEMBL ID","Data Source","Data Type",
                               "Value","Gene Name","Coding Type",
                               "Biotype")
      if (input$downltype == 1){
        table.out = table.out[grep("ratio*",table.out[,"Data Type"]),]
        a = table.out[,"Data Type"]
        a = gsub("ratio2","CNRCI",a)
        a = gsub("ratioK","RCI",a)
        table.out[,"Data Type"] = a
      } else if (input$downltype == 3){
        table.out = table.out[-grep("ratio*",table.out[,"Data Type"]),]
        a = table.out[,"Data Type"]
        a = gsub("cytosol","cytoplasm",a)
        table.out[,"Data Type"] = a
      } else{
        a = table.out[,"Data Type"]
        a = gsub("ratio2","CNRCI",a)
        a = gsub("ratioK","RCI",a)
        a = gsub("cytosol","cytoplasm",a)
        table.out[,"Data Type"] = a
      }
      write.csv(table.out, file)
    }
  )

  output$distroK <- renderPlot({
    # to avoid unclosing exits


    # I obtain the data
    distr.df <- get.distroK()


    # getting specifics genes
    geneID <- re.id()
    geneID <- unlist(strsplit(geneID,","))

    on.exit(progress$close())

    # validate
    validate(
      need(try(geneID %in% distr.df$geneid),
           "There is no subcompartment data for this gene.")
    )

    gene.df <- distr.df[ distr.df$geneid %in% geneID ,]
    name <- getGeneName(unique(gene.df$geneid))
    names(name) <- unique(gene.df$geneid)
    gene.df$name <- name[gene.df$geneid]
    name.title <- paste(name,collapse = " : ")


    distr.list <- list()
    distr.type.list <- list()
    for (i in unique(distr.df$source)){
      distr.list[[i]] <- distr.df[distr.df$source == i,]$expression_value
      distr.type.list[[i]][["coding"]] <-
        distr.df[distr.df$source == i &
                   distr.df$coding_type == "coding" ,]$expression_value
      distr.type.list[[i]][["nc"]] <-
        distr.df[distr.df$source == i &
                   distr.df$coding_type == "nc" ,]$expression_value
    }

    gene.list <- list()
    for (i in unique(gene.df$source)){
      gene.list[[i]] <- gene.df[gene.df$source == i,]$expression_value
      names(gene.list[[i]]) <- gene.df[gene.df$source == i,]$geneid
    }

    extr.list <- extreme.multiple.nc(distr.type.list,gene.list)
    gene.df$extr <- unlist(extr.list)[paste(gene.df$source,
                                            gene.df$geneid,
                                            sep=".")]


    # computing the number of genes per plot
    cls.vec <- unique(distr.df$source)
    n <- c()
    m <- c()
    cl <- c()
    ct <- c()
    ev <- c()
    for ( i in cls.vec){
      for (j in c("nc","coding")){
        n <- c(n,length(distr.type.list[[i]][[j]]))
        cl <- c(cl,i)
        ct <- c(ct,j)
        ev <- c(ev,max(distr.type.list[[i]][[j]]))
      }
    }
    n.df <- data.frame(n = n,
                       source = cl,
                       coding_type = ct,
                       expression_value = ev)

    n.df[order(n.df$coding_type),]$expression_value <-
      c(rep(max(n.df$expression_value),dim(n.df)[1]/2),
        rep(max(n.df$expression_value)+ 0.85,dim(n.df)[1]/2))


    distr.df$coding_type <- factor(distr.df$coding_type,
                                   levels = c("nc","coding"),ordered = TRUE)
    distr.df$source <- factor(distr.df$source,
                                   levels = c("ratioKin",
                                   "ratioKmem",
                                   "ratioKc",
                                   "ratioKnp",
                                   "ratioKno"),
                                   ordered = TRUE)

    ## needed to increase the dodging between the violins and the boxplots
    dodge <- position_dodge(width = 0.8)

    ## ploting
    g <- ggplot(data=distr.df,aes(x=source,y=expression_value,color=coding_type)) +
      ggtitle(name.title) +
      geom_violin(position=dodge) +
      geom_boxplot(width=0.2,outlier.size = 0.05,position=dodge) +
      geom_point(data=distr.df[distr.df$geneid %in% geneID,],
                 aes(shape=geneid),
                 color="black",size=3) +
      scale_shape_manual(values = c(8:(7+length(name))),
                         labels= c(name[geneID])) +
      scale_colour_Publication(labels=c("lncRNA","mRNA")) +
      theme_lncatlas_distrK() +
      labs(x="Compartment",
           y="RCI ") +
      geom_text(data=gene.df, aes(label=paste0(round(extr*100,1),"%")),
                vjust = -1.25,check_overlap = TRUE,
                color = "black", position="dodge",size = 5) +
      geom_text(data=n.df, aes(y=expression_value,
                               label=paste0("n = ",n)),
                position = "identity",size = 5) +
      expand_limits(y=c(1.5,11))  +
      annotate("text", x = cls.vec[length(cls.vec)] , y = -Inf,
               label = "lncATLAS",
               hjust=0, vjust=-1.1, col="black", cex=6,
               fontface = "bold", alpha = 0.8) +
      guides(colour = guide_legend(title = ""),
             shape = guide_legend(title = "genes ")) +
      scale_x_discrete(labels=c("Insoluble \n fraction","Cell \n membrane","Chromatin",
      "Nucleoplasm","Nucleolus"))
    path = paste(sess_id,"plotK.pdf",sep="/")
    ggsave(path, g)
    progress$set(message = "All plot will be displayed now", value = 1)
    g
  })
  output$downloadPlotK <- downloadHandler(
    filename = function() {
      paste("lncATLAS_subnuclear_",Sys.Date(),".pdf",sep = "")
    },
    content = function(file) {
      path = paste(sess_id,"plotK.pdf",sep="/")
     file.copy(path, file, overwrite=TRUE)
    })
    output$downloadPlotR1 <- downloadHandler(
      filename = function() {
        paste("lncATLAS_Ratio_",Sys.Date(),".pdf",sep = "")
      },
      content = function(file) {
        path = paste(sess_id,"plotR1.pdf",sep="/")
        file.copy(path, file, overwrite=TRUE)
      })
      output$downloadPlotR2 <- downloadHandler(
        filename = function() {
          paste("lncATLAS_",Sys.Date(),".pdf",sep = "")
        },
        content = function(file) {
          path = paste(sess_id,"plotR2.pdf",sep="/")
          file.copy(path, file, overwrite=TRUE)
        } )
      output$downloadPlotD1 <- downloadHandler(
        filename = function() {
          paste("lncATLAS_",Sys.Date(),".pdf",sep = "")
        },
        content = function(file) {
          path = paste(sess_id,"plotD1.pdf",sep="/")
          file.copy(path, file, overwrite=TRUE)
        } )
      output$downloadPlotD2 <- downloadHandler(
        filename = function() {
          paste("lncATLAS_",Sys.Date(),".pdf",sep = "")
        },
        content = function(file) {
          path = paste(sess_id,"plotD2.pdf",sep="/")
          file.copy(path, file, overwrite=TRUE)
        })
})
