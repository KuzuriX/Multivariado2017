#menu<-read.csv(file.choose(), header = T)
menu<-read.csv('menu.csv', header = T)

#plot( base$Calories, base$Calories.from.Fat)
#plot( base$Sugars, base$Calories)

library(car)
#scatterplotMatrix(base,pch=".",cex=1.5)


if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if("readr" %in% rownames(installed.packages()) == FALSE) {install.packages("readr")}
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
if("ggthemes" %in% rownames(installed.packages()) == FALSE) {install.packages("ggthemes")}
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
if("repr" %in% rownames(installed.packages()) == FALSE) {install.packages("repr")}
if("MVA" %in% rownames(installed.packages()) == FALSE) {install.packages("MVA")}
if("Hmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("Hmisc")}
if("dtplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dtplyr")}
if("corrplot" %in% rownames(installed.packages()) == FALSE) {install.packages("corrplot")}

library(car)
library(ggplot2) 
library(readr) 
library(tidyverse)
library(ggthemes) 
library(stringr) 
library(repr)
library(MVA)
library(data.table)
library(Hmisc)
library(corrplot)

### RECODIFICACION


new_names <- gsub(pattern = "*....Daily.Value.", replacement=".DV", names(menu))
names(menu) <- new_names

menu$diet[str_detect(menu$Item, "Diet|fat|Fat|Grilled|^Coffe|Free")]<-1
menu$subcategory[str_detect(menu$Item, "Coca-Cola|Coke")]<-"cocacola"
menu$subcategory[str_detect(menu$Item, "Dr Pepper")]<-"pepper"
menu$subcategory[str_detect(menu$Item, "Sprite")]<-"sprite"
menu$subcategory[str_detect(menu$Item, "Minute Maid")]<-"minutemaid"
menu$subcategory[str_detect(menu$Item, "Sandwich")]<-"sandwich"
menu$subcategory[str_detect(menu$Item, "McWrap")]<-"wrap"
menu$subcategory[str_detect(menu$Item, "Nuggets")]<-"nuggets"
menu$subcategory[str_detect(menu$Item, "McMuffin")]<-"McMuffin"
menu$subcategory[str_detect(menu$Item, "Biscuit")]<-"Biscuit"
menu$subcategory[str_detect(menu$Item, "McGriddles")]<-"McGriddles"
menu$subcategory[str_detect(menu$Item, "Big Breakfast")]<-"Big Breakfast"
menu$subcategory[str_detect(menu$Item, "Tea")]<-"Tea"
menu$subcategory[str_detect(menu$Item, "Coffee")]<-"Coffee"
menu$subcategory[str_detect(menu$Item, "Latte")]<-"Latte"
menu$subcategory[str_detect(menu$Item, "Mocha")]<-"Mocha"
menu$subcategory[str_detect(menu$Item, "Chocolate")]<-"Chocolate"
menu$subcategory[str_detect(menu$Item, "Iced")]<-"Iced Coffe"
menu$subcategory[str_detect(menu$Item, "Frapp")]<-"Frappe"
menu$subcategory[str_detect(menu$Item, "Shake")]<-"Milk Shake"
menu$subcategory[str_detect(menu$Item, "Smoothie")]<-"Smoothie"
menu$subcategory[str_detect(menu$Item, "McFlurry")]<-"McFlurry"



menu$diet[is.na(menu$diet)] <- 0
menu$subcategory[is.na(menu$subcategory)] <- "Others"


menu$Total.Fat.DV<-menu$Total.Fat.DV/65
menu$Saturated.Fat.DV<-menu$Saturated.Fat.DV/20
menu$Cholesterol.DV<-menu$Cholesterol.DV/300
menu$Sodium.DV<-menu$Sodium.DV/2400
menu$Carbohydrates.DV=menu$Carbohydrates.DV/300
menu$Dietary.Fiber.DV=menu$Dietary.Fiber.DV/25
menu$Vitamin.A.DV=menu$Vitamin.A.DV/5000
menu$Vitamin.C.DV=menu$Vitamin.C.DV/60
menu$Calcium.DV=menu$Calcium.DV/1000
menu$Iron.DV=menu$Iron.DV/18


#drinks - select only fields that contain "fl oz" string and sperately 'carton' string
drinks.oz <- menu[str_detect(menu$Serving.Size, " fl oz.*"),]
drinks.ml <- menu[str_detect(menu$Serving.Size, 'carton'),]

#drinks - keep the numbers and convert ounces to mililiters (1 oz = 29.5735 ml)
#round the values to zero decimal places 
drinks.oz$Serving.Size <- 
  round(as.numeric(gsub(" fl oz.*", "", drinks.oz$Serving.Size))*29.5735,0)
drinks.ml$Serving.Size <- 
  round(as.numeric(gsub(".*\\((.*)\\ ml).*", "\\1", drinks.ml$Serving.Size)),0)

#food - select only fields that contain "g" string
#keep the numbers and round the values to zero decimal places
food.g <- menu[str_detect(menu$Serving.Size, 'g'),] 
food.g$Serving.Size <- 
  round(as.numeric(gsub(".*\\((.*)\\ g).*", "\\1", food.g$Serving.Size)),0)

#combine all those data frames by rows into new data frame
#create new column with Type of Item as either 'drink' or 'food'
menu2 <- rbind(drinks.oz,drinks.ml)
menu2$Type <- rep("drinks.ml", nrow(menu2))
food.g$Type <- rep("food.g", nrow(food.g))
menu2 <- rbind(menu2,food.g)

table(menu2$Category, menu2$subcategory)
table(menu2$Type)

hist(menu2$Calories)

#attach(menu2)
# plot(menu2$Serving.Size, menu2$Calories)
# 
# bvbox(cbind(menu2$Serving.Size,menu2$Calories),mtitle="",
#       cex.lab=0.7,pch=18)
# fuera=identify(menu2$Serving.Size,menu2$Calories,rownames(menu2))
# menu2$Item[fuera]
# 
# #### Esto es una pureba para quitar los valores extremos
# 
# ## desoues de ver que la correlacion era baja y que no cambia mucho
# ## se mantienen esos valores extremos
# 
# menu3<-menu2[-fuera,]
# 
# bvbox(cbind(menu3$Serving.Size,menu3$Calories),mtitle="",
#       cex.lab=0.7,pch=18)

### grafico de matriz de correlaciones

scatterplotMatrix(menu2[,2:25],pch=".",cex=1.5)

M <- cor(menu2[,3:25])
corrplot(M, method="circle")

## Calorias por tamanno de porcion (por categoria)

e <- ggplot(menu2, aes(Serving.Size, Calories))
e+geom_label(aes(label = Category), nudge_x = 1,
             nudge_y = 1)
e+geom_point(aes(colour = Category))

xyplot(Calories ~ Serving.Size|Category, pch=18, menu2)

qplot(Calories, Serving.Size, data = menu2, facets = ~Category, colour = Type)

qplot(Calories, Serving.Size, data = menu2, facets = ~Category, colour = factor(diet))

cor(menu2$Serving.Size,menu2$Calories)

##Calorias de grasa por calorias (talvez muy obvio) 
### parece que no

e <- ggplot(menu2, aes(Calories.from.Fat, Calories))
e+geom_point()

qplot(Calories, Calories.from.Fat, data = menu2, facets = ~Category, colour = Type)
qplot(Calories, Calories.from.Fat, data = menu2, facets = ~Category, colour = factor(diet))


cor(Calories.from.Fat,Calories) ##Demasiado alto 


##Grasa total por calorias
### parece que no

e <- ggplot(menu2, aes(Total.Fat, Calories))
e+geom_point()

qplot(Calories, Total.Fat, data = menu2, facets = ~Category, colour = Type)

cor(Calories,Total.Fat) ##Demasiado alto tambien


##Grasa saturada por tamanno de porcion

e <- ggplot(menu2, aes(Saturated.Fat, Serving.Size))
e+geom_point()

qplot(Saturated.Fat, Serving.Size, data = menu2, facets = ~Category, colour = Type)
# los McFLurrys son lo que a pesar de estar en smoothies se mide la porcion en gramos

##Colesteros por calorias

e <- ggplot(menu2, aes(Cholesterol, Calories))
e+geom_point()

qplot(Cholesterol, Calories, data = menu2, facets = ~Category, colour = Type)

cor(Cholesterol,Calories) 


##COlesteros por grasa total

e <- ggplot(menu2, aes(Cholesterol, Total.Fat))
e+geom_point()

qplot(Cholesterol, Total.Fat, data = menu2, facets = ~Category, colour = Type)

xyplot(Cholesterol ~ Total.Fat|Category, pch=18)

cor(Cholesterol,Total.Fat) 


## Sodio por Tamanno
e <- ggplot(menu2, aes(Sodium, Serving.Size))
e+geom_point()

xyplot(Sodium ~ Serving.Size|Category, pch=18)

cor(Sodium,Serving.Size) 

## Sodio por calorias
e <- ggplot(menu2, aes(Sodium, Calories))
e+ geom_point(shape=Type)

e <- ggplot(menu2, aes(Sodium, Calories))

e+ geom_point(aes(color=Category)) + scale_color_brewer(type="seq", palette="Accent")

xyplot(Sodium ~ Calories|Category, pch=18)

cor(Sodium,Calories) 


## calorias por carbohidratos
e <- ggplot(menu2, aes(Calories, Carbohydrates))
e+ geom_point(shape=Type)

e <- ggplot(menu2, aes(Calories, Carbohydrates))

e+ geom_point(aes(color=Category)) + scale_color_brewer(type="seq", palette="Accent")

xyplot(Calories ~ Carbohydrates|Category, pch=18)

cor(Sodium,Calories) 

#maria.


#cambio de Miguel y asi 
