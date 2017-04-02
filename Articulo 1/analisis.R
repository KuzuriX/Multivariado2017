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
library(corrgram)
library(gridExtra)

### RECODIFICACION


new_names <- gsub(pattern = "*....Daily.Value.", replacement=".DV", names(menu))
names(menu) <- new_names

menu$diet[str_detect(menu$Item, "Diet|fat|Fat|Grilled|^Coffe|Free")]<-1
menu$Productos[str_detect(menu$Item, "Coca-Cola|Coke")]<-"coca cola"
menu$Productos[str_detect(menu$Item, "Dr Pepper")]<-"pepper"
menu$Productos[str_detect(menu$Item, "Sprite")]<-"sprite"
menu$Productos[str_detect(menu$Item, "Minute Maid")]<-"minute maid"
menu$Productos[str_detect(menu$Item, "Sandwich")]<-"sandwich"
menu$Productos[str_detect(menu$Item, "McWrap")]<-"wrap"
menu$Productos[str_detect(menu$Item, "Nuggets")]<-"nuggets"
menu$Productos[str_detect(menu$Item, "McMuffin")]<-"McMuffin"
menu$Productos[str_detect(menu$Item, "Biscuit")]<-"Biscuit"
menu$Productos[str_detect(menu$Item, "McGriddles")]<-"McGriddles"
menu$Productos[str_detect(menu$Item, "Big Breakfast")]<-"Big Breakfast"
menu$Productos[str_detect(menu$Item, "Tea")]<-"Té"
menu$Productos[str_detect(menu$Item, "Coffee")]<-"Café"
menu$Productos[str_detect(menu$Item, "Latte")]<-"Latte"
menu$Productos[str_detect(menu$Item, "Mocha")]<-"Mocha"
menu$Productos[str_detect(menu$Item, "Chocolate")]<-"Chocolate"
menu$Productos[str_detect(menu$Item, "Iced")]<-"Iced Coffe"
menu$Productos[str_detect(menu$Item, "Frapp")]<-"Frappe"
menu$Productos[str_detect(menu$Item, "Shake")]<-"Milk Shake"
menu$Productos[str_detect(menu$Item, "Smoothie")]<-"Smoothie"
menu$Productos[str_detect(menu$Item, "McFlurry")]<-"McFlurry"


menu$diet[is.na(menu$diet)] <- 0
menu$Productos[is.na(menu$Productos)] <- "Otro"




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

table(menu2$Category, menu2$Productos)
table(menu2$Type)

is.factor(menu2$Productos)
menu2$Productos= as.factor(menu2$Productos)
levels(menu2$Productos)

menu2$Type= as.factor(menu2$Type)
levels(menu2$Type)

menu2$Productos= menu2$Productos


levels(menu2$Category)[1]= "Carne y Cerdo" 
levels(menu2$Category)[2]= "Bebidas"
levels(menu2$Category)[3]= "Desayunos"
levels(menu2$Category)[4]= "Pollo y Pescado" 
levels(menu2$Category)[5]= "Café y Té" 
levels(menu2$Category)[6]= "Postres" 
levels(menu2$Category)[7]= "Ensaladas" 
levels(menu2$Category)[8]= "Smoothies"
levels(menu2$Category)[9]= "Snacks" 


###################### analisis ###########################################3

###ricardo les dijo -correlaciones de elementos nutricionales en el alimento. Lo que se correlaciona lo contrastamos con teoria





#componentes puros: Sodio- carbohidratos y azucar(Glucidos)- grasa(insaturada-saturada-trans) y colesterol
#componentes antioxidantes: Proteinas, vitamina A, vitamina C, Hierro, Dietary Fiber



#1 -- Lipidicos, glucidos, sodio contra calorias 
#2-- Lipidicos, glucidos, sodios contra antioxidantes 



menu3=subset(menu2,menu2$Sodium.DV<100)

####### 1 

##########Glucidos con calorias 



#col1=c("antiquewhite1","aquamarine4","black", "blue4","brown3","cadetblue1","chartreuse1","chocolate1","cyan","darkgoldenrod1","darkmagenta")
#col2=c("deeppink","lightcoral","lightgoldenrod1","mediumorchid1","orangered","seagreen1","yellowgreen","slategray4","thistle1","peru","palevioletred")
#col=c(col1,col2)



#HISTOGRAMAS


q=qplot(menu2$Calories, geom = c("histogram"), fill = I("sienna1"), xlab = "Calorías (Kcal)", ylab = "Densidad", data = menu2,facets = ~ Category)+theme(axis.title = element_text( face="bold",size=rel(0.8)))
q1=qplot(menu2$Carbohydrates, geom = c("histogram"), fill = I("plum4"), xlab = "Carbohidratos (g)", ylab = "Densidad", data = menu2,facets = ~ Category)+theme(axis.title = element_text( face="bold",size=rel(0.8)))
q2=qplot(menu2$Protein, geom = c("histogram"), fill = I("turquoise3"), xlab = "Proteínas (g)", ylab = "Densidad", data = menu2,facets = ~ Category)+theme(axis.title = element_text( face="bold",size=rel(0.8)))
q3=qplot(menu2$Sugars, geom = c("histogram"), fill = I("seagreen"), xlab = "Azúcar (g)", ylab = "Densidad", data = menu2,facets = ~ Category)+theme(axis.title = element_text( face="bold",size=rel(0.8)))
  
grid.arrange(q,q1,q2,q3, ncol = 2)


q3=qplot(menu3$Sodium.DV, geom = c("histogram"), fill = I("seagreen"), xlab = "Sodio VDR (%)", ylab = "Densidad",data=menu3,facets = ~Category)+theme(axis.title = element_text( face="bold",size=rel(0.8)))


#glucidos

q=qplot( Calories,Carbohydrates,data = menu3, facets = ~Category,col=Productos)

q+geom_rug(sides = "bl", col="Black")+labs( x ="Calorías(Kcal)", y = "Carbohidratos(g)")+
  theme(axis.title = element_text( face="bold",size=rel(0.8)))+
  theme( legend.title = element_text(face = "bold",size=9),legend.text = element_text(size = 8))

q=qplot( Protein,Carbohydrates,data = menu3, facets = ~Category,col=Productos)

q+geom_rug(sides = "bl", col="Black")+labs( x ="Proteínas (g)", y = "Carbohidratos (g)")+
  theme(axis.title = element_text( face="bold",size=rel(0.8)))+
  theme( legend.title = element_text(face = "bold",size=9),legend.text = element_text(size = 8))




q=qplot( menu2$Protein,menu2$Sugars,data = menu2, facets = ~Category,col=Productos)

q+geom_rug(sides = "bl")+labs( x ="Proteínas (g)", y = "az (g)")+
  theme(axis.title = element_text( face="bold",size=rel(0.8)))+
  theme( legend.title = element_text(face = "bold",size=9),legend.text = element_text(size = 8))

categoria=menu3$Category

q=qplot(menu3$Calories,menu3$Sugars,data = menu3,col=categoria)
q+geom_rug(sides = "bl",col="Black")+labs( x ="Calorías (Kcal)", y = "Azúcar (g)")+
  theme(axis.title = element_text( face="bold",size=rel(0.8)))+
  theme( legend.title = element_text(face = "bold",size=9),legend.text = element_text(size = 8))




##########SODIO



##### Sodio y calorias


q=qplot( menu2$Sodium,menu2$Protein,data = menu2, facets = ~Category,col=Productos)

q+geom_rug(sides = "bl")+labs( x ="Sodio (mg)", y = "Proteína (g)")+
  theme(axis.title = element_text( face="bold",size=rel(0.8)))+
  theme( legend.title = element_text(face = "bold",size=9),legend.text = element_text(size = 8))




###########lipidos 


#vamos a crear la insaturada

menu2$insaturada= menu2$Total.Fat-(menu2$Trans.Fat+menu2$Saturated.Fat)






############333calorias con grasas 


##Grasa TOTAL por calorias

e <- ggplot(menu2, aes(Total.Fat, Calories))
e+geom_point()


q=qplot(Calories,Total.Fat,data = menu2, facets = ~Category,col=Productos)

q+geom_rug(sides = "bl")+labs( x ="Calorías(Kcal)", y = "Grasa Total (mg)")+
  theme(axis.title = element_text( face="bold",size=rel(0.8)))+
  theme( legend.title = element_text(face = "bold",size=9),legend.text = element_text(size = 8))



#insaturada
e <- ggplot(menu2, aes(insaturada, Calories))
e+geom_point()

qplot(Calories,insaturada, data = menu2, facets = ~Category, colour = Productos)




#saturada
e <- ggplot(menu2, aes(Saturated.Fat , Calories))
e+geom_point()

q=qplot(Calories,Saturated.Fat,data = menu2, facets = ~Category,col=Productos)

q+geom_rug(sides = "bl")+labs( x ="Calorías(Kcal)", y = "Grasa Saturada (mg)")+
  theme(axis.title = element_text( face="bold",size=rel(0.8)))+
  theme( legend.title = element_text(face = "bold",size=9),legend.text = element_text(size = 8))




#trans > no hay corr

e <- ggplot(menu2,aes(Trans.Fat, Calories))
e+geom_point()

qplot(Calories,Trans.Fat, data = menu2, facets = ~Category, colour = Type)

plot(menu2$Calories,menu2$Trans.Fat,col=menu2$Productos)




#grasas con antioxidantes 



# no hay relacion lineal en ninguna 
qplot( menu2$Trans.Fat,menu2$Protein, data = menu2, facets = ~Category, colour = Productos)
qplot( menu2$Trans.Fat,menu2$Iron.DV, data = menu2, facets = ~Category, colour = Productos)
qplot( menu2$Trans.Fat,menu2$Dietary.Fiber, data = menu2, facets = ~Category, colour = Productos)
qplot( menu2$Trans.Fat,menu2$Vitamin.A.DV, data = menu2, facets = ~Category, colour = Productos)



qplot( menu2$Saturated.Fat,menu2$Protein, data = menu2, facets = ~Category, colour = Productos)
qplot( menu2$Saturated.Fat,menu2$Dietary.Fiber, data = menu2, facets = ~Category, colour = Productos)


q=qplot(Protein,Saturated.Fat,data = menu2, facets = ~Category,col=Productos)

q+geom_rug(sides = "bl")+labs( x ="Proteínas (g)", y = "Grasa Saturada (mg)")+
  theme(axis.title = element_text( face="bold",size=rel(0.8)))+
  theme( legend.title = element_text(face = "bold",size=9),legend.text = element_text(size = 8))







qplot( menu2$insaturada,menu2$Protein, data = menu2, facets = ~Category, colour = Productos)
qplot( menu2$insaturada,menu2$Iron.DV, data = menu2, facets = ~Category, colour = Productos)
qplot( menu2$insaturada,menu2$Dietary.Fiber, data = menu2, facets = ~Category, colour = Productos)
qplot( menu2$insaturada,menu2$Vitamin.A.DV, data = menu2, facets = ~Category, colour = Productos)







#insat y colest

qplot(insaturada, Cholesterol, data = menu2, facets = ~Category, colour = Productos)


#colesterol y saturada

qplot( Saturated.Fat,Cholesterol, data = menu2, facets = ~Category, colour = Productos)

q=qplot(Cholesterol,Saturated.Fat,data = menu2, facets = ~Category,col=Productos)

q+geom_rug(sides = "bl")+labs( x ="Colesterol (mg)", y = "Grasa Saturada (mg)")+
  theme(axis.title = element_text( face="bold",size=rel(0.8)))+
  theme( legend.title = element_text(face = "bold",size=9),legend.text = element_text(size = 8))




#colesterol con antiox


qplot( menu2$Cholesterol,menu2$Protein, data = menu2, facets = ~Category, colour = Productos)
qplot( menu2$Cholesterol,menu2$Iron.DV, data = menu2, facets = ~Category, colour = Productos)
qplot( menu2$Cholesterol,menu2$Dietary.Fiber, data = menu2, facets = ~Category, colour = Productos)
qplot( menu2$Cholesterol,menu2$Vitamin.A.DV, data = menu2, facets = ~Category, colour = Productos)





















############

## Calorias por tamanno de porcion (por categoria)

e <- ggplot(menu2, aes(Serving.Size, Calories))
e+geom_label(aes(label = Category), nudge_x = 1,
             nudge_y = 1)
e+geom_point(aes(colour = Category))

qplot(Calories, Serving.Size, data = menu2, facets = ~Category, colour = Type)

qplot(Calories, Serving.Size, data = menu2, facets = ~Category, colour = factor(diet))

cor(menu2$Serving.Size,menu2$Calories)





########tamaño de porcion 


##Grasa saturada por tamanno de porcion

e <- ggplot(menu2, aes(Saturated.Fat, Serving.Size))
e+geom_point()

qplot(Saturated.Fat, Serving.Size, data = menu2, facets = ~Category, colour = Type)
# los McFLurrys son lo que a pesar de estar en smoothies se mide la porcion en gramos



e <- ggplot(menu2, aes(Total.Fat, Serving.Size))
e+geom_point()

qplot(Total.Fat, Serving.Size, data = menu2, facets = ~Category, colour = Type)
# Breakfast , beef and pork




## Sodio por Tamanno
e <- ggplot(menu2, aes(Sodium, Serving.Size))
e+geom_point()

qplot(Sodium, Serving.Size, data = menu2, facets = ~Category, colour = Type)
cor(menu2$Sodium,menu2$Serving.Size) 

# Breakfast , beef and pork, chicken and fish





### grafico de matriz de correlaciones

M <- cor(menu2[,3:25])
corrplot(M, method="circle")


summary(menu2$Calories[menu2$Category=="Carne y Cerdo"])
sd(menu2$Calories[menu2$Category=="Carne y Cerdo"])
summary(menu2$insaturada[menu2$Category=="Carne y Cerdo"])
sd(menu2$insaturada[menu2$Category=="Carne y Cerdo"])
summary(menu2$Saturated.Fat[menu2$Category=="Carne y Cerdo"])
sd(menu2$Saturated.Fat[menu2$Category=="Carne y Cerdo"])
summary(menu2$Trans.Fat[menu2$Category=="Carne y Cerdo"])
sd(menu2$Trans.Fat[menu2$Category=="Carne y Cerdo"])
summary(menu2$Cholesterol[menu2$Category=="Carne y Cerdo"])
sd(menu2$Cholesterol[menu2$Category=="Carne y Cerdo"])
summary(menu2$Sodium[menu2$Category=="Carne y Cerdo"])
sd(menu2$Sodium[menu2$Category=="Carne y Cerdo"])
summary(menu2$Carbohydrates[menu2$Category=="Carne y Cerdo"])
sd(menu2$Carbohydrates[menu2$Category=="Carne y Cerdo"])
summary(menu2$Sugars[menu2$Category=="Carne y Cerdo"])
sd(menu2$Sugars[menu2$Category=="Carne y Cerdo"])
summary(menu2$Protein[menu2$Category=="Carne y Cerdo"])
sd(menu2$Protein[menu2$Category=="Carne y Cerdo"])
#summary(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])
#sd(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])





summary(menu2$Calories[menu2$Category=="Pollo y Pescado"])
sd(menu2$Calories[menu2$Category=="Pollo y Pescado"])
summary(menu2$insaturada[menu2$Category=="Pollo y Pescado"])
sd(menu2$insaturada[menu2$Category=="Pollo y Pescado"])
summary(menu2$Saturated.Fat[menu2$Category=="Pollo y Pescado"])
sd(menu2$Saturated.Fat[menu2$Category=="Pollo y Pescado"])
summary(menu2$Trans.Fat[menu2$Category=="Pollo y Pescado"])
sd(menu2$Trans.Fat[menu2$Category=="Pollo y Pescado"])
summary(menu2$Cholesterol[menu2$Category=="Pollo y Pescado"])
sd(menu2$Cholesterol[menu2$Category=="Pollo y Pescado"])
summary(menu2$Sodium[menu2$Category=="Pollo y Pescado"])
sd(menu2$Sodium[menu2$Category=="Pollo y Pescado"])
summary(menu2$Carbohydrates[menu2$Category=="Pollo y Pescado"])
sd(menu2$Carbohydrates[menu2$Category=="Pollo y Pescado"])
summary(menu2$Sugars[menu2$Category=="Pollo y Pescado"])
sd(menu2$Sugars[menu2$Category=="Pollo y Pescado"])
summary(menu2$Protein[menu2$Category=="Pollo y Pescado"])
sd(menu2$Protein[menu2$Category=="Pollo y Pescado"])
#summary(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])
#sd(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])



summary(menu2$Calories[menu2$Category=="Desayunos"])
sd(menu2$Calories[menu2$Category=="Desayunos"])
summary(menu2$insaturada[menu2$Category=="Desayunos"])
sd(menu2$insaturada[menu2$Category=="Desayunos"])
summary(menu2$Saturated.Fat[menu2$Category=="Desayunos"])
sd(menu2$Saturated.Fat[menu2$Category=="Desayunos"])
summary(menu2$Trans.Fat[menu2$Category=="Desayunos"])
sd(menu2$Trans.Fat[menu2$Category=="Desayunos"])
summary(menu2$Cholesterol[menu2$Category=="Desayunos"])
sd(menu2$Cholesterol[menu2$Category=="Desayunos"])
summary(menu2$Sodium[menu2$Category=="Desayunos"])
sd(menu2$Sodium[menu2$Category=="Desayunos"])
summary(menu2$Carbohydrates[menu2$Category=="Desayunos"])
sd(menu2$Carbohydrates[menu2$Category=="Desayunos"])
summary(menu2$Sugars[menu2$Category=="Desayunos"])
sd(menu2$Sugars[menu2$Category=="Desayunos"])
summary(menu2$Protein[menu2$Category=="Desayunos"])
sd(menu2$Protein[menu2$Category=="Desayunos"])
#summary(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])
#sd(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])



summary(menu2$Calories[menu2$Category=="Ensaladas"])
sd(menu2$Calories[menu2$Category=="Ensaladas"])
summary(menu2$insaturada[menu2$Category=="Ensaladas"])
sd(menu2$insaturada[menu2$Category=="Ensaladas"])
summary(menu2$Saturated.Fat[menu2$Category=="Ensaladas"])
sd(menu2$Saturated.Fat[menu2$Category=="Ensaladas"])
summary(menu2$Trans.Fat[menu2$Category=="Ensaladas"])
sd(menu2$Trans.Fat[menu2$Category=="Ensaladas"])
summary(menu2$Cholesterol[menu2$Category=="Ensaladas"])
sd(menu2$Cholesterol[menu2$Category=="Ensaladas"])
summary(menu2$Sodium[menu2$Category=="Ensaladas"])
sd(menu2$Sodium[menu2$Category=="Ensaladas"])
summary(menu2$Carbohydrates[menu2$Category=="Ensaladas"])
sd(menu2$Carbohydrates[menu2$Category=="Ensaladas"])
summary(menu2$Sugars[menu2$Category=="Ensaladas"])
sd(menu2$Sugars[menu2$Category=="Ensaladas"])
summary(menu2$Protein[menu2$Category=="Ensaladas"])
sd(menu2$Protein[menu2$Category=="Ensaladas"])
#summary(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])
#sd(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])



summary(menu2$Calories[menu2$Category=="Postres"])
sd(menu2$Calories[menu2$Category=="Postres"])
summary(menu2$insaturada[menu2$Category=="Postres"])
sd(menu2$insaturada[menu2$Category=="Postres"])
summary(menu2$Saturated.Fat[menu2$Category=="Postres"])
sd(menu2$Saturated.Fat[menu2$Category=="Postres"])
summary(menu2$Trans.Fat[menu2$Category=="Postres"])
sd(menu2$Trans.Fat[menu2$Category=="Postres"])
summary(menu2$Cholesterol[menu2$Category=="Postres"])
sd(menu2$Cholesterol[menu2$Category=="Postres"])
summary(menu2$Sodium[menu2$Category=="Postres"])
sd(menu2$Sodium[menu2$Category=="Postres"])
summary(menu2$Carbohydrates[menu2$Category=="Postres"])
sd(menu2$Carbohydrates[menu2$Category=="Postres"])
summary(menu2$Sugars[menu2$Category=="Postres"])
sd(menu2$Sugars[menu2$Category=="Postres"])
summary(menu2$Protein[menu2$Category=="Postres"])
sd(menu2$Protein[menu2$Category=="Postres"])
#summary(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])
#sd(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])


summary(menu2$Calories[menu2$Category=="Bebidas"])
sd(menu2$Calories[menu2$Category=="Bebidas"])
summary(menu2$insaturada[menu2$Category=="Bebidas"])
sd(menu2$insaturada[menu2$Category=="Bebidas"])
summary(menu2$Saturated.Fat[menu2$Category=="Bebidas"])
sd(menu2$Saturated.Fat[menu2$Category=="Bebidas"])
summary(menu2$Trans.Fat[menu2$Category=="Bebidas"])
sd(menu2$Trans.Fat[menu2$Category=="Bebidas"])
summary(menu2$Cholesterol[menu2$Category=="Bebidas"])
sd(menu2$Cholesterol[menu2$Category=="Bebidas"])
summary(menu2$Sodium[menu2$Category=="Bebidas"])
sd(menu2$Sodium[menu2$Category=="Bebidas"])
summary(menu2$Carbohydrates[menu2$Category=="Bebidas"])
sd(menu2$Carbohydrates[menu2$Category=="Bebidas"])
summary(menu2$Sugars[menu2$Category=="Bebidas"])
sd(menu2$Sugars[menu2$Category=="Bebidas"])
summary(menu2$Protein[menu2$Category=="Bebidas"])
sd(menu2$Protein[menu2$Category=="Bebidas"])
#summary(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])
#sd(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])


summary(menu2$Calories[menu2$Category=="Café y Té"])
sd(menu2$Calories[menu2$Category=="Café y Té"])
summary(menu2$insaturada[menu2$Category=="Café y Té"])
sd(menu2$insaturada[menu2$Category=="Café y Té"])
summary(menu2$Saturated.Fat[menu2$Category=="Café y Té"])
sd(menu2$Saturated.Fat[menu2$Category=="Café y Té"])
summary(menu2$Trans.Fat[menu2$Category=="Café y Té"])
sd(menu2$Trans.Fat[menu2$Category=="Café y Té"])
summary(menu2$Cholesterol[menu2$Category=="Café y Té"])
sd(menu2$Cholesterol[menu2$Category=="Café y Té"])
summary(menu2$Sodium[menu2$Category=="Café y Té"])
sd(menu2$Sodium[menu2$Category=="Café y Té"])
summary(menu2$Carbohydrates[menu2$Category=="Café y Té"])
sd(menu2$Carbohydrates[menu2$Category=="Café y Té"])
summary(menu2$Sugars[menu2$Category=="Café y Té"])
sd(menu2$Sugars[menu2$Category=="Café y Té"])
summary(menu2$Protein[menu2$Category=="Café y Té"])
sd(menu2$Protein[menu2$Category=="Café y Té"])
#summary(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])
#sd(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])


summary(menu2$Calories[menu2$Category=="Smothies"])
sd(menu2$Calories[menu2$Category=="Smothies"])
summary(menu2$insaturada[menu2$Category=="Smothies"])
sd(menu2$insaturada[menu2$Category=="Smothies"])
summary(menu2$Saturated.Fat[menu2$Category=="Smothies"])
sd(menu2$Saturated.Fat[menu2$Category=="Smothies"])
summary(menu2$Trans.Fat[menu2$Category=="Smothies"])
sd(menu2$Trans.Fat[menu2$Category=="Smothies"])
summary(menu2$Cholesterol[menu2$Category=="Smothies"])
sd(menu2$Cholesterol[menu2$Category=="Smothies"])
summary(menu2$Sodium[menu2$Category=="Smothies"])
sd(menu2$Sodium[menu2$Category=="Smothies"])
summary(menu2$Carbohydrates[menu2$Category=="Smothies"])
sd(menu2$Carbohydrates[menu2$Category=="Smothies"])
summary(menu2$Sugars[menu2$Category=="Smothies"])
sd(menu2$Sugars[menu2$Category=="Smothies"])
summary(menu2$Protein[menu2$Category=="Smothies"])
sd(menu2$Protein[menu2$Category=="Smothies"])
#summary(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])
#sd(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])



summary(menu2$Calories[menu2$Category=="Snacks"])
sd(menu2$Calories[menu2$Category=="Snacks"])
summary(menu2$insaturada[menu2$Category=="Snacks"])
sd(menu2$insaturada[menu2$Category=="Snacks"])
summary(menu2$Saturated.Fat[menu2$Category=="Snacks"])
sd(menu2$Saturated.Fat[menu2$Category=="Snacks"])
summary(menu2$Trans.Fat[menu2$Category=="Snacks"])
sd(menu2$Trans.Fat[menu2$Category=="Snacks"])
summary(menu2$Cholesterol[menu2$Category=="Snacks"])
sd(menu2$Cholesterol[menu2$Category=="Snacks"])
summary(menu2$Sodium[menu2$Category=="Snacks"])
sd(menu2$Sodium[menu2$Category=="Snacks"])
summary(menu2$Carbohydrates[menu2$Category=="Snacks"])
sd(menu2$Carbohydrates[menu2$Category=="Snacks"])
summary(menu2$Sugars[menu2$Category=="Snacks"])
sd(menu2$Sugars[menu2$Category=="Snacks"])
summary(menu2$Protein[menu2$Category=="Snacks"])
sd(menu2$Protein[menu2$Category=="Snacks"])
#summary(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])
#sd(menu2$Dietary.Fiber[menu2$Category=="Carne y Cerdo"])

table(menu2$Item,menu2$Category)






























