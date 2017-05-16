load("base.Rdata")
library(lattice)
library(dplyr)
library(cluster)
library(ggplot2)

##Analisis valores extremos
table(base2$musica)

table(base2$genero.antes)

table(base2$genero.ahora)

table(base2$instrumento)

table(base2$importancia.musica)

table(base2$musica.actividades)

table(base2$influencia.musica)

#Distancia tomando en cuenta todas las variables de interes

# dis<-(daisy(base2[-1],"gower",
#             type = list(symm = c(1, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27),
#                         ordratio=c(9,13,14))))

dis<-(daisy(base2[,10:28],"gower",
            type = list(symm = c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
                        ordratio=c(1,5,6))))

#Para ver de qut tipo quedaron las variables. Quedaron como se necesitaban
summary(dis)

disdisMat<-as.matrix(dis)

#Los mas parecidos
parecidos<-base2[which(disdisMat == min(disdisMat[disdisMat != min(disdisMat)]),arr.ind = TRUE)[1, ], ]

#Los menos parecidos
diferentes<-base2[which(disdisMat == max(disdisMat[disdisMat != max(disdisMat)]),arr.ind = TRUE)[1, ], ]



#dendogramas (se elije el del vecino mas lejano)
par(mfrow=c(1,3))

cs = hclust(dis,method = "single")	
plot(cs,xlab="",main="Vecino más cercano",ylab="Distancia",sub="")

cs = hclust(dis,method = "average")	
plot(cs,xlab="",main="Promedio",ylab="Distancia",sub="")

#Este es el que se elije
cs = hclust(dis,method = "complete")	
plot(cs,xlab="",main="Vecino más lejano",ylab="Distancia",sub="")


#ordenando base segun distancias para graficas
orden<-cs$order
base3<-base2[orden,]

disOrden<-(daisy(base2[,10:28],"gower",
                 type = list(symm = c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
                             ordratio=c(1,5,6))))

disOrden_M<-as.matrix(disOrden)

levelplot(disOrden_M)



##############################################################



###########################################################


#Distancia sin tomar en cuenta las variables de personalidad

# dis<-(daisy(base2[-1],"gower",
#             type = list(symm = c(1, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27),
#                         ordratio=c(9,13,14))))

dis2<-(daisy(base2[,10:16],"gower",
            type = list(ordratio=c(1,5,6))))

#Para ver de qut tipo quedaron las variables. Quedaron como se necesitaban
summary(dis2)

disdisMat<-as.matrix(dis2)

#Los mas parecidos
parecidos2<-base2[which(disdisMat == min(disdisMat[disdisMat != min(disdisMat)]),arr.ind = TRUE)[1, ], ]

#Los menos parecidos
diferentes2<-base2[which(disdisMat == max(disdisMat[disdisMat != max(disdisMat)]),arr.ind = TRUE)[1, ], ]



#dendogramas (se elije el del vecino mas lejano)
par(mfrow=c(1,3))

cs2 = hclust(dis2,method = "single")	
plot(cs2,xlab="",main="Vecino más cercano",ylab="Distancia",sub="")

cs2 = hclust(dis2,method = "average")	
plot(cs2,xlab="",main="Promedio",ylab="Distancia",sub="")

#Este es el que se elije
cs2 = hclust(dis2,method = "complete")	
plot(cs2,xlab="",main="Vecino más lejano",ylab="Distancia",sub="")


#ordenando base segun distancias para graficas
orden<-cs2$order
base4<-base2[orden,]

disOrden2<-(daisy(base4[,10:16],"gower",
               type = list(ordratio=c(1,5,6))))

disOrden_M2<-as.matrix(disOrden2)

levelplot(disOrden_M2)

#Guardando los clusters
base4<-mutate(base4, cluster =c(rep("G1",4),rep("G2",17),rep("G3",8),rep("G4",9)))


###############################################
##  Graficos

names(base4[,10:16])

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

(sapply(base4, class)=="numeric")

base5[,c(10,14,15)]<-range01(base4[,c(10,14,15)])

for(i in 10:16){
  
  print( ggplot(base4, aes(base4[,29], base4[,i]))
          + geom_col()  )
 
  
}

f <- ggplot(base4, aes(base4[,29], base4[,10]))
f + geom_col()


