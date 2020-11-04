install.packages("gtools", repos="https://cran.rstudio.com/")
install.packages("tidyverse", repos="https://cran.rstudio.com/")
install.packages("dplyr", repos="https://cran.rstudio.com/")
install.packages("arrangements")

library(tidyverse)
library(dplyr)
library(gtools)
library(ggplot2)
library(reshape)
library(arrangements)

#tenzies

B <- 10000

S <- replicate(B, {
  
  sides <- 6
  n <- 10
  round_number <- 0
  
  choice <- sample(c(1,2,3,4,5,6),1)
  #print(paste("And the number is... ",choice))
  
  while (n!=0) {   
    round_number = round_number + 1
    round <- sample(1:sides, n, replace = TRUE)
    points <- sum(round==choice)
    #print(paste("Round: ",round_number," / Dices: ",n," / Points: ", points," --> ", paste("|",round,"|", collapse = '')))
    n <- n - points
  }
  
  #print(paste("||||||---------->>> TOTAL TRIES = ",round_number," <<<----------||||||"))
  
  round_number
  
})

hist(S)
mean(S)
sd(S)

# variable number of dices

sides_main <- 6

tenzies <- function(dices, sides = sides_main, B = 5000) {
  
  S <- replicate(B, {
    
    sides <- 6
    n <- dices
    round_number <- 0
    
    choice <- sample(1:sides,1)
    
    while (n!=0) {   
      round_number = round_number + 1
      round <- sample(1:sides, n, replace = TRUE)
      points <- sum(round==choice)
      n <- n - points
    }
    
    round_number
    
  })
  
  average <- mean(S)  
  
}

x <- seq(0,50,5)
x[1] <- 2
x[2] <- 4

montecarlo <- sapply(x, tenzies)

plot(x, montecarlo)

gamma <- 0.5772156649015328606065120900824024310421

H_n <- log(x) + gamma + 1/(2*x)

proxy <- H_n / log(sides_main/(sides_main-1))



expected_N <- function(N, sides = sides_main){
  
  cum <- 0
  a <- 0
  k <- 1
  as.double(cum)
  as.numeric(a)
  
  while (k<=N){
    a <- ( factorial(N) / ( factorial(k) * factorial(N-k)) * (-1)^(k+1) ) / (1-((sides-1)/sides)^k)
    cum <- cum + a
    k <- k+1
    #print(k,a)
  }
  cum
}

expected <- sapply(x, expected_N)

results <- data.frame(x,montecarlo,proxy, expected)

results.melted <- melt(results, id = "x")

results.melted %>% ggplot(aes(x=x, y=value, color = variable)) + geom_line(size = 1)
