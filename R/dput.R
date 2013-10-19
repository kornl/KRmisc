dputMatrix <- function(m, name, indent=6, rowNames=FALSE) {
    s <- "rbind("
    if (!missing(name)) s <- paste(name,"<- rbind(") 
    for (i in 1:(dim(m)[1])) {
        nameLater <- FALSE
        if (any(make.names(row.names(m))!=row.names(m))) {
            rowNames <- FALSE
            nameLater <- TRUE
        }
        rName <- ifelse(rowNames, paste(row.names(m)[i],"=",sep=""), "")
        s <- paste(s, 
                   ifelse(i==1,"",paste(rep(" ",indent),collapse="")),
                   rName,
                   dput2(unname(m[i,])),
                   ifelse(i==dim(m)[1],")\n",",\n"),
                   sep="")            
    }
    if (nameLater) {
        if (missing(name)) {
            warning("Can set row names if no name for matrix is given.")
            return(s)
        }
        s <- paste(s, 
                   "row.names(",name,") <- ", dput2(row.names(m)), "\n", sep="")
    }
    return(s)
}

dputS <- function(x) {
    paste(capture.output(dput(x)), collapse=" ")
}