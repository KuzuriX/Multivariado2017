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


#menu$Total.Fat.DV<-menu$Total.Fat.DV/65
#menu$Saturated.Fat.DV<-menu$Saturated.Fat.DV/20
#menu$Cholesterol.DV<-menu$Cholesterol.DV/300
#menu$Sodium.DV<-menu$Sodium.DV/2400
#menu$Carbohydrates.DV=menu$Carbohydrates.DV/300
#menu$Dietary.Fiber.DV=menu$Dietary.Fiber.DV/25
#menu$Vitamin.A.DV=menu$Vitamin.A.DV/5000
#menu$Vitamin.C.DV=menu$Vitamin.C.DV/60
#menu$Calcium.DV=menu$Calcium.DV/1000
#menu$Iron.DV=menu$Iron.DV/18


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

is.factor(menu2$subcategory)
menu2$subcategory= as.factor(menu2$subcategory)
levels(menu2$subcategory)

menu2$Type= as.factor(menu2$Type)
levels(menu2$Type)

###################### analisis ###########################################3

###ricardo les dijo -correlaciones de elementos nutricionales en el alimento. Lo que se correlaciona lo contrastamos con teoria





#componentes puros: Sodio- carbohidratos y azucar(Glucidos)- grasa(insaturada-saturada-trans) 



#########3#Glucidos

cor(menu2$Sugars,menu2$Carbohydrates) #0.76

a <- qplot(menu2$Carbohydrates, geom = c("density"), fill = I("tomato"), xlab = "Carbohidratos", ylab = "Densidad", data = menu2)
a

b<- qplot(menu2$Sugars, geom = c("density"), fill = I("tomato"), xlab = "Azúcar", ylab = "Densidad", data = menu2)
b


q1=qplot(Sugars,Carbohydrates, data = menu2,xlab = deparse(substitute(Azúcar)),  ylab = deparse(substitute(Carbohidratos)))
q1

q=qplot(Sugars,Carbohydrates, data = menu2, facets = ~ Category, colour = subcategory)
q


library(gridExtra)
grid.arrange(a,b,q1,q, ncol = 2)



###### Resultados > HAY CORRELACION ALTA ENTRE AZUCAR Y CARBOH.. especificamente beverages, coffee and Smothies 



###########Grasas

#vamos a crear la insaturada

menu2$insaturada= menu2$Total.Fat-(menu2$Trans.Fat+menu2$Saturated.Fat)



library(corrgram)

minibase=cbind(menu2$insaturada,menu2$Saturated.Fat,menu2$Trans.Fat,menu2$Cholesterol)
colnames(minibase)=c("insaturada","saturada","trans","colesterol")
corrgram(minibase, upper.panel=panel.conf,order=T,abs=T,diag.panel=panel.density)


M <- cor(minibase)
corrplot(M, method="circle",order ="AOE") 

#corr alta entre insaturada y colest , insaturada y saturada, colesterol y sat, saturada y trans



#insat y colest
cor(menu2$insaturada,menu2$Cholesterol)#0.62
plot(menu2$insaturada,menu2$Cholesterol)

qplot(insaturada, Cholesterol, data = menu2, facets = ~Category, colour = subcategory)

#   comportamiento lineal en Beef & pork , coffee & tea, smoothies & shakes, chicken and fish

cor(menu2$insaturada[menu2$Category=="Coffee & Tea" ],menu2$Cholesterol[menu2$Category=="Coffee & Tea"])
cor(menu2$insaturada[menu2$Category=="Beef & Pork" ],menu2$Cholesterol[menu2$Category=="Beef & Pork"])
cor(menu2$insaturada[menu2$Category=="Smoothies & Shakes" ],menu2$Cholesterol[menu2$Category=="Smoothies & Shakes"])
cor(menu2$insaturada[menu2$Category=="Chicken & Fish" ],menu2$Cholesterol[menu2$Category=="Chicken & Fish"])



#insaturada y saturada
cor(menu2$insaturada,menu2$Saturated.Fat)#0.64
plot(menu2$insaturada,menu2$Saturated.Fat)  # en el plot pareciera que no hay una relacion lineal

qplot(insaturada, Saturated.Fat, data = menu2, facets = ~Category, colour = subcategory)

cor(menu2$insaturada[menu2$Category=="Chicken & Fish" ],menu2$Saturated.Fat[menu2$Category=="Chicken & Fish"])
cor(menu2$insaturada[menu2$Category=="Beef & Pork" ],menu2$Saturated.Fat[menu2$Category=="Beef & Pork"])
cor(menu2$insaturada[menu2$Category=="Smoothies & Shakes" ],menu2$Saturated.Fat[menu2$Category=="Smoothies & Shakes"])
cor(menu2$insaturada[menu2$Category=="Coffee & Tea" ],menu2$Saturated.Fat[menu2$Category=="Coffee & Tea"])
cor(menu2$insaturada[menu2$Category=="Breakfast" ],menu2$Saturated.Fat[menu2$Category=="Breakfast"])


#RESULTADOS:  COR ALTA  "Chicken & Fish" "Beef & Pork" Smoothies & Shakes  "Coffee & Tea  Breakfast"


#colesterol y saturada

cor(menu2$Cholesterol,menu2$Saturated.Fat)#0.63

plot(menu2$Saturated.Fat,menu2$Cholesterol)  # en el plot pareciera que no hay una relacion lineal

qplot( Saturated.Fat,Cholesterol, data = menu2, facets = ~Category, colour = subcategory)

#Resultados: corr alta en Beef & Pork, Chicken and fish, Coffee and tea , Smoothies and salads 


#Saturadas y trans 

cor(menu2$Trans.Fat,menu2$Saturated.Fat)#0.62

plot(menu2$Saturated.Fat,menu2$Trans.Fat)  # en el plot pareciera que no hay una relacion lineal

qplot( Saturated.Fat,Trans.Fat, data = menu2, facets = ~Category, colour = subcategory)

cor(menu2$Trans.Fat[menu2$Category=="Beef & Pork" ],menu2$Saturated.Fat[menu2$Category=="Beef & Pork"])
cor(menu2$Trans.Fat[menu2$Category=="Coffee & Tea" ],menu2$Saturated.Fat[menu2$Category=="Coffee & Tea"])
cor(menu2$Trans.Fat[menu2$Category=="Smoothies & Shakes" ],menu2$Saturated.Fat[menu2$Category=="Smoothies & Shakes"])




#############Proteinas, vitamina A, vitamina C, Hierro, Calcio, Dietary Fiber


minibase=cbind(menu2$Protein,menu2$Vitamin.A.DV,menu2$Vitamin.C.DV,menu2$Iron.DV,menu2$Calcium.DV,menu2$Dietary.Fiber)
colnames(minibase)=c("proteina","vitamina A","vitamina C","Hierro","Calcio","Fibra")
corrgram(minibase, upper.panel=panel.conf,order=T,abs=T,diag.panel=panel.density)

#Correlaciones altas:
#Hierro con Fibra :0.74
#Hierro proteina : 0.79
#Fibra con Proteina: 0.64



#HIERRO CON fIBRA 

###REVISAR 

plot(menu2$Iron.DV,menu2$Dietary.Fiber)  #no hay relacion lineal

qplot( menu2$Iron.DV,menu2$Dietary.Fiber, data = menu2, facets = ~Category, colour = subcategory)



#Hierro con prot

#REVISAR

plot(menu2$Iron.DV,menu2$Protein)  #no hay relacion lineal

qplot( menu2$Iron.DV,menu2$Protein, data = menu2, facets = ~Category, colour = subcategory)



#Fibra con proteina 
#parece que no hay relacion 

plot(menu2$Dietary.Fiber,menu2$Protein)  #no hay relacion lineal
qplot( menu2$Dietary.Fiber,menu2$Protein, data = menu2, facets = ~Category, colour = subcategory)





############### sodio 
minibase=cbind(menu2$Protein,menu2$Vitamin.A.DV,menu2$Vitamin.C.DV,menu2$Iron.DV,menu2$Calcium.DV,menu2$Dietary.Fiber,menu2$Sodium)
colnames(minibase)=c("proteina","vitamina A","vitamina C","Hierro","Calcio","Fibra","sod")
corrgram(minibase, upper.panel=panel.conf,order=T,abs=T,diag.panel=panel.density)


#corr de sodio con proteina (.87)
#corr de sodio con hierro (.87)
#corr de sodio con fibra (.69)

plot(menu2$Sodium,menu2$Protein)  #lineal
plot(menu2$Sodium,menu2$Iron.DV)#maso lineal
plot(menu2$Sodium,menu2$Dietary.Fiber)  #no es muy lineal


qplot( menu2$Sodium,menu2$Protein, data = menu2, facets = ~Category, colour = subcategory)

#sodio con proteina:  relacion lineal en coffee and tea, smoothies and shakes, snacks


cor(menu2$Sodium[menu2$Category=="Snacks & Sides" ],menu2$Protein[menu2$Category=="Snacks & Sides"])
cor(menu2$Sodium[menu2$Category=="Coffee & Tea" ],menu2$Protein[menu2$Category=="Coffee & Tea"])
cor(menu2$Sodium[menu2$Category=="Smoothies & Shakes" ],menu2$Protein[menu2$Category=="Smoothies & Shakes"])



#### no se  creo que no hay relacon lineal
qplot( menu2$Sodium,menu2$Iron.DV, data = menu2, facets = ~Category, colour = subcategory)
qplot( menu2$Sodium,menu2$Dietary.Fiber, data = menu2, facets = ~Category, colour = subcategory)




#####Calorias 




############calorias con glucidos 


minibase=cbind(menu2$Carbohydrates,menu2$Sugars,menu2$Calories)
colnames(minibase)=c("carb","azucar","calorias")
corrgram(minibase, upper.panel=panel.conf,order=T,abs=T,diag.panel=panel.density)
#corr alta calorias con carbohidratos


e <- ggplot(menu2, aes(Calories, Carbohydrates))
e+ geom_point(aes(color=Category)) + scale_color_brewer(type="seq", palette="Accent")

qplot( menu2$Calories,menu2$Carbohydrates, data = menu2, facets = ~Category, colour = subcategory)


# azucar y calorias


qplot( menu2$Calories,menu2$Sugars, data = menu2, facets = ~Category, colour = subcategory)


e <- ggplot(menu2, aes(Sugars, Calories))
e+geom_point()


d<-qplot(Sugars, Calories, data = menu2, facets = ~Category, colour = factor(diet))
d + stat_summary(fun.y = "mean", colour = "blue", geom = "point")


cor(menu2$Sugars,menu2$Calories) #Correlacion 26

#?




##### Sodio y calorias
e <- ggplot(menu2, aes(Sodium, Calories))
e+geom_point()

qplot(Sodium, Calories, data = menu2, facets = ~Category, colour = Type)
qplot(Sodium, Calories, data = menu2, facets = ~Category, colour = factor(diet))


cor(menu2$Sodium,menu2$Calories) #Correlacion 71%





############333calorias con grasas 


##Grasa TOTAL por calorias

e <- ggplot(menu2, aes(Total.Fat, Calories))
e+geom_point()

qplot(Calories, Total.Fat, data = menu2, facets = ~Category, colour = Type)

cor(menu2$Calories,menu2$Total.Fat) ##Demasiado alto tambien



#insaturada
e <- ggplot(menu2, aes(insaturada, Calories))
e+geom_point()

qplot(Calories,insaturada, data = menu2, facets = ~Category, colour = Type)

cor(menu2$Calories,menu2$insaturada) 



#saturada
e <- ggplot(menu2, aes(Saturated.Fat , Calories))
e+geom_point()

qplot(Calories,Saturated.Fat, data = menu2, facets = ~Category, colour = Type)

cor(menu2$Calories,menu2$Saturated.Fat) 


#trans > no hay corr

e <- ggplot(menu2,aes(Trans.Fat, Calories))
e+geom_point()

qplot(Calories,Trans.Fat, data = menu2, facets = ~Category, colour = Type)

cor(menu2$Calories,menu2$Trans.Fat) 




#colesterol > dudas
e <- ggplot(menu2,aes(Cholesterol, Calories))
e+geom_point()

qplot(Calories,Cholesterol, data = menu2, facets = ~Category, colour = Type)

cor(menu2$Calories,menu2$Cholesterol) 






########tamañoo de porcion 















#############################################################################################################


##Grasa saturada por tamanno de porcion

e <- ggplot(menu2, aes(Saturated.Fat, Serving.Size))
e+geom_point()

qplot(Saturated.Fat, Serving.Size, data = menu2, facets = ~Category, colour = Type)
# los McFLurrys son lo que a pesar de estar en smoothies se mide la porcion en gramos



## Sodio por Tamanno
e <- ggplot(menu2, aes(Sodium, Serving.Size))
e+geom_point()

xyplot(Sodium ~ Serving.Size|Category, pch=18)

cor(menu2$Sodium,menu2$Serving.Size) 




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


