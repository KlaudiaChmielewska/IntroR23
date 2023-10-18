atomic_mass <- c("H"= 1.0079, "He"= 4.0026, "Li"= 6.941, "Be"= 9.0122,"B"= 10.811, "C"= 12.011, "N"= 14.007, "O"= 15.999, "F"= 18.998,  "Ne"= 20.180, "Na"= 22.990, "Mg"= 24.305, "Al"= 26.982,  "Si"= 28.086, "P"= 30.974, "S"= 32.065, "Cl"= 35.453,  "Ar"= 39.948, "K"= 39.098, "Ca"= 40.078, "Sc"= 44.956,  "Ti"= 47.867, "V"= 50.942, "Cr"= 51.996, "Mn"= 54.938,  "Fe"= 55.845, "Co"= 58.933, "Ni"= 58.693, "Cu"= 63.546,  "Zn"= 65.39, "Ga"= 69.723, "Ge"= 72.61, "As"= 74.922,  "Se"=78.96, "Br"= 79.904, "Kr"= 83.80, "Rb"= 85.468, "Sr"= 87.62,  "Y"= 88.906, "Zr"= 91.224, "Nb"= 92.906, "Mo"= 95.94,  "Tc"= 97.61, "Ru"= 101.07, "Rh"= 102.91, "Pd"= 106.42,  "Ag"= 107.87, "Cd"= 112.41, "In"= 114.82, "Sn"= 118.71,  "Sb"= 121.76, "Te"= 127.60, "I"= 126.90, "Xe"= 131.29,  "Cs"= 132.91, "Ba"= 137.33, "La"= 138.91, "Ce"= 140.12,  "Pr"= 140.91, "Nd"= 144.24, "Pm"= 145.0, "Sm"= 150.36, "Eu"= 151.96,  "Gd"= 157.25, "Tb"= 158.93, "Dy"= 162.50, "Ho"= 164.93, "Er"= 167.26,  "Tm"= 168.93, "Yb"= 173.04, "Lu"= 174.97, "Hf"= 178.49, "Ta"= 180.95,  "W"= 183.84, "Re"= 186.21, "Os"= 190.23, "Ir"= 192.22, "Pt"= 196.08,  "Au"= 196.08, "Hg"= 200.59, "Tl"= 204.38, "Pb"= 207.2, "Bi"= 208.98,  "Po"= 209.0, "At"= 210.0, "Rn"= 222.0, "Fr"= 223.0, "Ra"= 226.0,  "Ac"= 227.0, "Th"= 232.04, "Pa"= 231.04, "U"= 238.03, "Np"= 237.0,  "Pu"= 244.0, "Am"= 243.0, "Cm"= 247.0, "Bk"= 247.0, "Cf"= 251.0, "Es"= 252.0,  "Fm"= 257.0, "Md"= 258.0, "No"= 259.0, "Lr"= 262.0, "Rf"= 261.0, "Db"= 262.0,"Sg"= 266.0, "Bh"= 264.0, "Hs"= 269.0, "Mt"= 268.0)

# first, we have to define a function, which will be an element of a later 'parse' function
# this function would seek the closing parenthesis ')'

find_closing_paren <- function(tokens) {
  #whenever this function starts, so does the counter (this helps the function assess when to stop)
  count <- 0
  # now we have to define that we should seek the ')' in the sequence provided from the variable 'tokens'
  for (index in seq_along(tokens)) {
    tok <- tokens[index]   # indexes inside 'tokens' should be defined as 'tok' for simplicity
    if (tok == ")") {
      count <- count - 1   # now reduce the counter to show the function it should stop working
      if (count == 0) {    # so now, if the count is 0 the function can finish. If not, it means, that was a set of parenthesis inside of another parenthesis, like C(C(CO)2)2
        return(index)      # now to finish, show found indexes inside parenthesis | (OH)2 => index = OH
      }                    # else, if there's a new parenthesis open, understand it as a different one and seek another closing
    } else if (tok == "(") {
      count <- count + 1
    }
  }
  NA
}

parse <- function(tokens, stack, atomic_mass) {
  if (length(tokens) == 0) {
    return(sum(stack)) # if there are no tokens to parse, meaning if we take a blank input
  }
  tok <- tokens[1] # now we go to divide tokens into fragments

  if (tok == "(") {
    end <- find_closing_paren(tokens)   # whenever you find an open parenthesis, function should seek its end
    # if the end is found, next the tokens should be called, from the 2nd element (first chemical inside parenthesis. First element is = '('))
    if (!is.na(end)) {
      sub_tokens <- tokens[2:(end - 1)] # atoms should be received until the last one (-1 because ')' is that last element
      sub_mass <- parse(sub_tokens, numeric(0), atomic_mass) # take what you selected in the last line and now parse those found elements into numbers, taken from the atomic_mass vector
      stack <- c(stack, sub_mass) # stack should be updated by the mass we obtained in sub_mass
      return(parse(tokens[(end + 1):length(tokens)], stack, atomic_mass))
    } # else checks if the variable tok contains a string that consists only of digits (so if we have single chemical elements)
  } else if (grepl("^\\d+$", tok)) { 
    stack[length(stack)] <- stack[length(stack)] * as.numeric(tok)
  } else {
    stack <- c(stack, atomic_mass[[tok]])
  }
  return(parse(tokens[-1], stack, atomic_mass))
}


while (TRUE) {
  formula <- readline(prompt = "Enter molecular formula: ")
  
  # our input formula will now be tokenized : changed into subscripts, which the computer will have to understand
  # we provide the regular expression in order to specify the pattern the computer should seek inside input
  # if the function regmatches() finds any other element - it won't be recognized
  
  tokens <- regmatches(formula, gregexpr("[A-Z][a-z]*|\\d+|\\(|\\)", formula))[[1]]
  cat("The molecular mass of", formula, "is", round(parse(tokens, numeric(0), atomic_mass), 3), "\n\n")
}
