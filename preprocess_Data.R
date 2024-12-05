library(data.table)

df <- fread("/Users/anyadecarlo/ctg_studies.csv", sep = "," , na.string = "")  
str(df)

clean_names <- function(x) { 
  x <- gsub("^[^[:alpha:]]+","", x)
  x <- gsub("[^[:alnum:]]+$","",x)
  x <- gsub ("[[:blank:][:punct:][:space:]]+","_", x)
  x <- gsub("_+" , "_", x)
}

read_data <- function(path, sep = ",", ...) {
    df <- fread(path, sep = sep, na.strings = "")
    setDT(df)
    names(df) <- clean_names(names(df))
    return(df)
  }


