print(precip)

#Q1

precip[precip>39]
precip[precip<31]
precip[precip == 40.2]
precip[precip<=46 & precip>=15]
precip[precip>40 | precip<13]


#Q2

mean(precip)
median(precip)
sd(precip)
var(precip)
quantile(precip)
max(precip)
min(precip)

? names
less_than_med <- names(precip[precip<median(precip)])
most <- names(precip[precip == max(precip)])
top_quantile <- names(precip[precip>=42.775])
extreme <- names(precip[precip>=(mean(precip)+(2*sd(precip)))])



#Q3

animals <-
  structure(c(1.35, 465, 36.33, 27.66, 1.04, 11700, 2547, 187.1, 
              521, 10, 3.3, 529, 207, 62, 6654, 9400, 6.8, 35, 0.12, 0.023, 
              2.5, 55.5, 100, 52.16, 0.28, 87000, 0.122, 192, 8.1, 423, 119.5, 
              115, 5.5, 50, 4603, 419, 655, 115, 25.6, 680, 406, 1320, 5712, 
              70, 179, 56, 1, 0.4, 12.1, 175, 157, 440, 1.9, 154.5, 3, 180), .Dim = c(28L, 
                                                                                      2L), .Dimnames = list(c("Mountain beaver", "Cow", "Grey wolf", 
                                                                                                              "Goat", "Guinea pig", "Dipliodocus", "Asian elephant", "Donkey", 
                                                                                                              "Horse", "Potar monkey", "Cat", "Giraffe", "Gorilla", "Human", 
                                                                                                              "African elephant", "Triceratops", "Rhesus monkey", "Kangaroo", 
                                                                                                              "Golden hamster", "Mouse", "Rabbit", "Sheep", "Jaguar", "Chimpanzee", 
print(animals)                                                                                                             "Rat", "Brachiosaurus", "Mole", "Pig"), c("body", "brain")))
nrow(animals) #28 animals
getwd()


