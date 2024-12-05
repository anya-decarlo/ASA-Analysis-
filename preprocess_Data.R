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


describe <- function(x) { 
  if(!inherits(x, "data.table")) { 
    stop("Error: Input must be a data.table")
    }
  numeric_cols <- names(x)[sapply(x, is.numeric)]
  categorical_cols <- names(x)[sapply(x, function(col)
    is.factor(col) || is.character(col))]
  date_cols <- names(x)[sapply(x, function(col)
    inherits(col, c("IDate", "POSIXct", "PISIClt")))]
  
  Numeric <- data.table(t(x[, sapply(.SD, function(col) {
    c(Min = min(col, na.rm = TRUE), 
      Max = max(col, na.rm = TRUE), 
      Mean = mean(col, na.rm =TRUE), 
      Median = median(col, na.rm = TRUE),
      SD = sd(col, na.rm = TRUE), 
      N_missing = sum(is.na(col)))
     }), .SDcols = numeric_cols])) 
  
    Numeric[, Variable := numeric_cols]
    setcolorder(Numeric, "Variable")
    
    Categorical <- data.table( 
      Variable = categorical_cols, 
      Mode = x[, sapply(.SD, function(col) get_mode(col)), .SDcols = categorical_cols],
      N_missing = x[, sapply(.SD, function(col) sum(is.na(col))), .SDcols = categorical_cols])
      
      Date <- data.table(t(x[, sapply(.SD, function(col) { 
        c(Min = as.character(min(col, na.rm = TRUE)), 
        Max = as.character(max(col, na.rm = TRUE)), 
        Mean = as.character(mean(col, na.rm = TRUE)), 
        N_missing = sum(is.na(col)))
      }), .SDcols = date_cols]))
      Date[, Variable := date_cols]
      setcolorder(Date, "Variable")
      
      results <- list(
        Numeric = Numeric,
        Categorical = Categorical,
        Date = Date)
        
      return(results)
  }
