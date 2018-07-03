# percent2num(c("0.1%","5%","0,9%"))

percent2num <- function(x) {
  x <- gsub("%", "*0.01", as.character(x))
  x <- gsub(",", ".", x)
  f <- function(x) {eval(parse(text=x))}
  x <- Vectorize(f)(x)
  return(unname(unlist(x)))
}

 

# remove.dots(c("12.121.321,67", "1.678"))

remove.dots <- function(x) {
  x <- gsub("\\.", "", as.character(x))
  x <- gsub(",", ".", x)
  return(as.numeric(unlist(x)))
}
