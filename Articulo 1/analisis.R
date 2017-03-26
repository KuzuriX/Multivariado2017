menu<-read.csv(file.choose(), header = T)

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


library(ggplot2) 
library(readr) 
library(tidyverse)
library(ggthemes) 
library(stringr) 
library(repr)
library(MVA)
library(data.table)
library(Hmisc)


new_names <- gsub(pattern = "*....Daily.Value.", replacement=".DV", names(menu))
names(menu) <- new_names

diet<- menu[str_detect(menu$Item, "Diet|fat|Fat|Grilled"),]
cocacola<-menu[str_detect(menu$Item, "Coca-Cola|Coke"),]
pepper<-menu[str_detect(menu$Item, "Dr Pepper"),]
sprite<-menu[str_detect(menu$Item, "Sprite"),]
minutemaid<-menu[str_detect(menu$Item, "Minute Maid"),]
sandwich<-menu[str_detect(menu$Item, "Sandwich"),]
wrap<-menu[str_detect(menu$Item, "McWrap"),]
nuggets<-menu[str_detect(menu$Item, "Nuggets"),]

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

table(menu2$Category)
table(menu2$Type)

hist(menu2$Calories)

attach(menu2)
plot(Serving.Size, Calories)

bvbox(cbind(Serving.Size,Calories),mtitle="",
      cex.lab=0.7,pch=18)
fuera=identify(Serving.Size,Calories,rownames(menu2))
Item[fuera]

#### Esto es una pureba para quitar los valores extremos

menu2<-menu2[-fuera,]

## Calorias por tamanno de porcion

e <- ggplot(menu2, aes(Serving.Size, Calories))
e+geom_label(aes(label = Category), nudge_x = 1,
             nudge_y = 1, check_overlap = TRUE)
e+geom_point()


xyplot(Calories ~ Serving.Size|Category, pch=18)

##Calorias de grasa por calorias (talvez muy obvio)

e <- ggplot(menu2, aes(Calories.from.Fat, Calories))
e+geom_point()

xyplot(Calories.from.Fat ~ Calories|Category, pch=18)

cor(Calories.from.Fat,Calories) ##Demasiado alto 


##Grasa total por calorias

e <- ggplot(menu2, aes(Total.Fat, Calories))
e+geom_point()

xyplot(Total.Fat ~ Calories|Category, pch=18)

cor(Calories,Total.Fat) ##Demasiado alto tambien


##Grasa saturada por tamanno de porcion

e <- ggplot(menu2, aes(Saturated.Fat, Serving.Size))
e+geom_point()

xyplot(Total.Fat ~ Saturated.Fat|Category, pch=18)



##Colesteros por calorias

e <- ggplot(menu2, aes(Cholesterol, Calories))
e+geom_point()

xyplot(Cholesterol ~ Calories|Category, pch=18)

cor(Cholesterol,Calories) 


##COlesteros por grasa total

e <- ggplot(menu2, aes(Cholesterol, Total.Fat))
e+geom_point()

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


