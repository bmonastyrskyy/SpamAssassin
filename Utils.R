# Author : Bohdan Monastyrskyy
# Date : 2015-12-25

loadLib <- function(lib){
  if (! require(lib, character.only = TRUE, quietly = FALSE, warn.conflicts = FALSE)){
    install.packages(lib, quiet = TRUE)
    if (! require(lib, character.only = TRUE,quietly = FALSE, warn.conflicts = FALSE)){
      stop(paste("Package", lib, "failed to install!"))
    } else {
      return(TRUE);
    }
  } else {
   return(TRUE); 
  }
}

