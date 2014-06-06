write.recoding.template <- function(attributes, data, file, yaml = TRUE) {
  
  # get values for combination of attributes
  getValues <- function(multi_attributes, data) {
    comb <- expand.grid(
      sapply( multi_attributes, function(x){ levels(data[,x]) }, simplify = FALSE )
      )
    comb <- apply(comb,1,function(x){paste(x, collapse = " + ")})
    names(comb) <- 1:length(comb)
    return(as.list(comb))
  }
  
  # prepare the template for one attribute
  makeTemplate <- function(attribute, data) {
    if (length(attribute) > 1) {
      originalValues <- getValues(attribute, data)
    } else {
      originalValues <- levels(data[,attribute])
    }
    return(list(
      attribute = NULL,
      values = list(NULL,NULL),
      link = NULL,
      recodingOf = attribute,
      originalValues = originalValues
      ))
  }
  
  # combine all templates
  attributes <- as.list(sapply(attributes,function(x){colnames(data)[x]}))
  result <- list(
    title = NULL,
    author = NULL,
    date = format(Sys.time(),"%Y-%m-%d"),
    original_data = NULL,
    recodings = sapply(attributes, function(x) { makeTemplate(x, data) }, simplify = FALSE)
    )
  
  # return the result, defaults to a yaml-file
  if (yaml) {
    if (is.null(file)) {
      stop("please specify file")
    }
    cat(as.yaml(result), file = file)
  } else {
    return(result)
  }
}

recode <- function(data,recoding) {

  # recodings can be a file as input
  if (is.character(recoding)) {
    recoding <- yaml.load_file(recodings)$recoding
  }
  
  # Allow for various shortcuts in the writing of recodings
  realnames <- c("attribute", "values", "link", "recodingOf", "originalValues", "doNotRecode")
  for (i in 1:length(recoding)) {
    names(recoding[[i]]) <- realnames[pmatch(names(recoding[[i]]),realnames)]    
    
    if (is.null(recoding[[i]]$link)) {
      recoding[[i]]$doNotRecode <- i
    }
    if (is.null(recoding[[i]]$doNotRecode)) {
      if (is.null(recoding[[i]]$recodingOf)) {
        stop(paste("Specify **recodingOf** for recoding number",number,sep = " "))
      }
      if (is.null(recoding[[i]]$attribute)) {
        recoding[[i]]$attribute <- paste("Att", i, sep = "")
      }
      if (is.null(recoding[[i]]$values)) {
        recoding[[i]]$values <- paste("Val", 1:length(recoding[[i]]$link), sep = " ")
      } 
    } else {
      recoding[[i]]$attribute <- colnames(data)[recoding[[i]]$doNotRecode]
    }
  }
  
  # recoding for a new attribute
  makeAttribute <- function(recoding) {

    # when specified, do not recode attributes
    if (!is.null(recoding$doNotRecode)) {
      newAttribute <- data[,recoding$doNotRecode]
    } else {
      
      # simple when it is based on a single old attribute
      if (length(recoding$recodingOf) == 1) {
      newAttribute <- data[,recoding$recodingOf, drop = FALSE]
      levels(newAttribute[,1]) <- recoding$values[recoding$link]
      return(newAttribute)
      } else {
        
        # a bit more complex for combinations of attributes
        # this can probably be made more efficient!
        newAttribute <- data[,recoding$recodingOf]
        newAttribute <- apply(newAttribute,1,function(x){paste(x, collapse = " + ")})
        match <- expand.grid(
          sapply(recoding$recodingOf, function(x){ levels(data[,x]) }, simplify = FALSE )
          )
        match <- apply(match,1,function(x){paste(x, collapse = " + ")})
        newAttribute <- factor(newAttribute, levels = match)
        levels(newAttribute) <- recoding$values[recoding$link]
        return(newAttribute)
      }
    }
  }
  
  # Make the recoding and return result
  result <- as.data.frame(sapply(recoding, makeAttribute,simplify=F))
  colnames(result) <- unlist(sapply(recoding, function(x){x$attribute}))
  rownames(result) <- rownames(data)
  return(result)
}

