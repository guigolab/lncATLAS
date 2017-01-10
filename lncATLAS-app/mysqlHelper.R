################################################################################
################################################################################
#                             lncATLAS -- mysqlHelper
#
#         author: David Mas-Ponte @CRG
#         R MySQL helper functions
#
################################################################################
################################################################################

library("RMySQL")

lncatlasConnect <- function(){
  return(dbConnect(MySQL(), default.file="./.mysqlconf",group = "lncatlas"))
}

getTable <- function(conn,table,
                     whole_table=TRUE,
                     column = NULL,
                     condition = NULL,
                     inner_join = FALSE,
                     table2 = NULL,
                     condition_ij = NULL,
                     q = FALSE){
  if(whole_table){
    query <- paste0("SELECT * FROM ",table,";")
  } else if (inner_join) {
    column.str <- paste(column,collapse = ",")
    query <- paste0("SELECT ",column.str," FROM ",table," INNER JOIN ",
                    table2," ON ",condition_ij,";")
  } else {
    if (is.null(column) & is.null(condition)){
      warning('No column or condition were set. Asumming argument whole_table = TRUE')
      query <- paste0("SELECT * FROM ",table,";")
    } else if (is.null(column)) {
      warning("string input stil required")
      query <- paste0("SELECT * FROM ",table," WHERE ",condition,";")
    } else if (is.null(condition)) {

      if (class(column) == "character" & length(column) > 1) {
        column.str <- paste(column,collapse = ",")
        query <- paste0("SELECT ",column.str," FROM ",table,";")
      } else {
        warning("using the string input type")
        query <- paste0("SELECT ",column," FROM ",table,";")
      }

    } else if (!is.null(column) & !is.null(condition)) {
      warning("string input stil required (condition)")
      if (class(column) == "character" & length(column) > 1) {
        column.str <- paste(column,collapse = ",")
        query <- paste0("SELECT ",column.str," FROM ",table," WHERE "
                        ,condition,";")
      } else {
        warning("using the string input type")
        query <- paste0("SELECT ",column," FROM ",table," WHERE ",condition,";")
      }

    }
  }
  if (q){
    print(query)
  }
  table.df <- DBI::fetch(dbSendQuery(conn, query),n=-1)
  return(table.df)
}



resetConnections <- function(){
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    dbDisconnect(con)
  }

  if (length(dbListConnections(MySQL())) == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


getID <- function(name) {
  names <- paste("gene_name = '",name, sep="")
  cond <- paste(names, collapse = "' OR ")
  query <- paste0("SELECT ensembl_gene_id FROM genes WHERE (",
                  cond,"') AND (coding_type = 'nc');")
  cn <- lncatlasConnect()
  geneID <- as.character(DBI::fetch(dbSendQuery(cn, query),n=-1)[,1])
  suppressWarnings(dbDisconnect(cn))
  return(geneID)
}

getGeneName <- function(id) {
  names <- paste("ensembl_gene_id = '",id, sep="")
  cond <- paste(names, collapse = "' OR ")
  query <- paste0("SELECT gene_name FROM genes WHERE ",
                  cond,"';")
  cn <- lncatlasConnect()
  geneID <- as.character(DBI::fetch(dbSendQuery(cn, query),n=-1)[,1])
  suppressWarnings(dbDisconnect(cn))
  return(geneID)
}



getAllfromIDvec <- function(vectorids){
  geneID.cond <- paste("genes_ensembl_gene_id = '",vectorids, sep="")
  geneID.str.cond <- paste(geneID.cond, collapse = "' OR ")
  query <- paste0("SELECT * FROM expression INNER JOIN genes ON genes_ensembl_gene_id = ensembl_gene_id WHERE ",
                  geneID.str.cond,"';")
  cn <- lncatlasConnect()
  table.return <- DBI::fetch(dbSendQuery(cn, query),n=-1)
  suppressWarnings(dbDisconnect(cn))
  return(table.return)
}

# TESTING
# getTable(lncrnaConnect(),"data_source",whole_table = FALSE,
#          column = c("data_types_name","expression_sites_name"),
#          condition = "data_types_name = 'cytosol'")
#
