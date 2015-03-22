str_feature <- function(x){ 
    x <- gsub("[\\(\\)]", "", x)
    x <- gsub("-X$", "DimX", x)
    x <- gsub("-Y$", "DimY", x)
    x <- gsub("-Z$", "DimZ", x)
    x <- gsub("-mean", "Mean", x)
    gsub("-std", "Std", x)
}
