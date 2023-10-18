#guessing game when we set the number

random_number <- 7
x <- 1

while (TRUE) {
x <- readline(prompt = "Enter a number:")
x <- as.integer(x)

if (x != random_number) {
  print("Wrong number, try again.")
}
else if (x == random_number) {
  print("Bravo!")
  break
  
  }
}


# Guessing game with random number



while (TRUE) {
  x <- 1
  random_number <- floor(runif(1, min=1, max = 99))
  x <- readline(prompt = "Enter a number from 1 to 99:")
  x <- as.integer(x)
  
  if (x != random_number) {
    cat("Wrong number, it was", random_number, ", try again.")
  }
  else if (x == random_number) {
    print("Bravo!")
    break
    
  }
}
