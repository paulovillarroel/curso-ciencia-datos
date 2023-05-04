library(tidyverse)

# Vectores

vec <- c(1, 5, 3, 10)

vec
sum(vec)
str(vec)
class(vec)

vec[1]
vec[3]
vec[1:3]
vec[-1]

# Último item
vec[length(vec)]
tail(vec, n = 1)
dplyr::last(vec)
rev(vec)[1]

sort(vec)
sort(vec, decreasing = TRUE)


# Dataframes

df <- data.frame(edad = c(12, 43, 23),
                 nombre = c("Paulo", "Ana", "Pedro"),
                 ciudad = c("Santiago", "Chiloe", "Arica"))
df

nombre <- c("Paulo", "Maria", "Jose")
edad <- c(23, 43, NA)

df2 <- data.frame(nombre, edad)

sum(df) #error

df$edad
df$nombre
df$ciudad

sum(df$edad)
str(df)
class(df)

df[1, ]
df[1:2, ]
df[, 1]
df[, 1:2]
df[1, 2]

dplyr::slice(df, 1:2)

df |> 
  filter(edad > 30)

df |> 
  select(2)

df |> 
  select(nombre)

df3 <- df |> 
  arrange(desc(edad))


# Listas

my_list <- list("Paulo" = 22, "María" = 33, "Pedro" = 26)

my_list2 <- list(nombre, edad)
names(my_list2) <- c("nombre", "edad")


str(my_list2)
class(my_list)

my_list2$nombre

my_list2[[1]]
my_list2[["edad"]]
my_list2[[1]][2]

sum(my_list2) # error
sum(my_list2[[2]])

as.data.frame(my_list2)
