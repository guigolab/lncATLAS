######## GENERAL HELPER SCRIPT for lncATLAS #######
####
## Author: David Mas
cl <- "A549"
group <- "coding_type"






extreme <- function(vec,values) {
  results <- c()
  total <- length(vec)
  for (v in values) {
    fin <- which(vec == v)
    if (length(fin) == 0){
      ind <- NA
    } else {
      ind <- fin[1]
    }
    pos <- which(order(vec) == ind)
    if (length(pos)==0){
      res = NA
    } else {
      res <-  pos / total
    }
    results <- c(results,res)
  }
  return(results)
}

extreme.multiple <- function(distr.list,distr.values){
  results <- list()
  for (i in names(distr.values)){
    results[[i]] <- extreme(distr.list[[i]],distr.values[[i]])
    names(results[[i]]) <- names(distr.values[[i]])
  }
  return(results)
}

extreme.multiple.nc <- function(distr.list,distr.values){
  results <- list()
  for (i in names(distr.values)){
    results[[i]] <- extreme(distr.list[[i]][["nc"]],distr.values[[i]])
    names(results[[i]]) <- names(distr.values[[i]])
  }
  return(results)
}

Zscore <- function(vec,values) {
  results <- c()
  for (v in values) {
    A <- pnorm(v, mean(vec), sd(vec))
    results <- c(results,A)
  }
  return(results)
}




library(ggplot2)




StatExtreme <- ggproto("StatExtreme", Stat,
                  compute_group = function(data, scales) {
                    data.dn <- density(data$x)
                    data.dn <- data.frame(x=data.dn$x,y=data.dn$y)
                    val <- data$val[1]

                    if (val > mean(data$x)) {
                      if(val > max(data$x)){warning("Value out of the scope")}
                      data.dn <- data.dn[data.dn$x > val, , drop = FALSE]
                    } else {
                      if(val < min(data$x)){warning("Value out of the scope")}
                      data.dn <- data.dn[data.dn$x < val, , drop = FALSE]
                    }
                  },
                  required_aes = c("x","val")
)

## this is the layer

stat_extreme <- function(mapping = NULL, data = NULL, geom = "area",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatExtreme, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill = "#386cb0", alpha=0.3,  ...)
  )
}


#as.numeric(unlist(strsplit(,split=",")))

multipleExtremes <- function(vec){
  layer.list <- list()
  value1 <<- vec[1]
  layer.list <- c(layer.list,stat_extreme(data=distr.nc,aes(x=expression_value
                                                            ,val=value1)))
  if (length(vec) == 2){
    value2 <<- vec[2]
    layer.list <- c(layer.list,stat_extreme(data=distr.nc,aes(x=expression_value,
                                                              val=value2)))
  }
  if (length(vec) == 3){
    value3 <<- vec[3]
    layer.list <- c(layer.list,stat_extreme(data=distr.nc,aes(x=expression_value,
                                                              val=value3)))
  }
  layer.list
}



getPSE <- function(vec){
  if (vec[1]) {
    return(Inf)
  } else {
    return(-Inf)
  }
}

getRest <- function(vec){
  if (vec[1]) {
    return(vec[1])
  } else {
    return(vec[2])
  }
}


getPos <- function(val){
  return(sign(val)*0.5)
}

getPosL <- function(val){
  if (val == "cytosol") {
    return(0.5)
  } else {
    return(-0.5)
  }
}


getCol <- function(vec){
  if (vec[1] > vec[2]) {
    return(c(1,0))
  } else {
    return(c(0,1))
  }
}

setUpPos <- function(vec){
  if (vec[1] > vec[2]) {
    #this is when the gene is cyto
    return(c(log2(vec[1]/vec[2])+1.5,-1.5))
  } else {
    #this is when the gene is nuclear
    return(c(1.5,log2(vec[1]/vec[2])-1.5))
  }
}
