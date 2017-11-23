import_data <- function() {
    df <- read.csv(data_path)
    df[df == "NaN"] <- NA
    return(df)
}
