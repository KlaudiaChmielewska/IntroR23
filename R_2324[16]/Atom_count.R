get_count <- function(s) {
  mp <- list()
  
  i <- 1
  while (i <= nchar(s)) {
    count <- 0
    
    # Convert the character at i to a string
    c <- substr(s, i, i)
    
    # Check if c is an uppercase letter
    if (grepl("[A-Z]", c)) {
      a <- c
      
      j <- i + 1
      while (j <= nchar(s)) {
        d <- substr(s, j, j)
        
        # Check if d is a lowercase letter
        if (grepl("[a-z]", d)) {
          a <- paste0(a, d)
          
          if (!a %in% names(mp)) {
            mp[[a]] <- 1
          } else {
            mp[[a]] <- mp[[a]] + 1
          }
          count <- 1
          
          # Check if d is a digit
        } else if (grepl("[0-9]", d)) {
          k <- as.numeric(d)
          mp[[a]] <- k
          count <- 1
          
        } else {
          i <- j - 1
          break
        }
        j <- j + 1
      }
      
      if (count == 0) {
        if (!a %in% names(mp)) {
          mp[[a]] <- 1
        } else {
          mp[[a]] <- mp[[a]] + 1
        }
      }
    }
    
    i <- i + 1
  }
  
  cat("\nAtom count:\n")
  for (key in names(mp)) {
    cat(key, mp[[key]], "\n")
  }
}

while (TRUE) {
  
# Read input atomic symbol
mol_formula <- readline(prompt = "Enter the atomic formula: ")
count1 <- get_count(mol_formula)
cat("Given molecule", mol_formula, "has been parsed into numbers")
}
