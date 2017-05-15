load("base.Rdata")


##Analisis valores extremos


library(cluster)

dis<-(daisy(base2[-1],"gower",
            type = list(symm = c(1, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27),
                        ordratio=c(9,13,14))))

#Para ver de qut tipo quedaron las variables. Quedaron como se necesitaban
summary(dis)

disdisMat<-as.matrix(dis)

#Los mas parecidos
parecidos<-base2[which(disdisMat == min(disdisMat[disdisMat != min(disdisMat)]),arr.ind = TRUE)[1, ], ]

#Los menos parecidos
diferentes<-base2[which(disdisMat == max(disdisMat[disdisMat != max(disdisMat)]),arr.ind = TRUE)[1, ], ]


#ordenando base segun distancias para graficas
orden<-cs$order
base3<-base2[orden,]

disOrden<-(daisy(base3[-1],"gower",
            type = list(symm = c(1, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27),
                        ordratio=c(9,13,14))))
disOrden_M<-as.matrix(disOrden)

levelplot(disOrden_M)

#dendogramas
par(mfrow=c(1,3))

cs = hclust(dis,method = "single")	
plot(cs,xlab="",main="Vecino más cercano",ylab="Distancia",sub="")

cs = hclust(dis,method = "complete")	
plot(cs,xlab="",main="Vecino más lejano",ylab="Distancia",sub="")

cs = hclust(dis,method = "average")	
plot(cs,xlab="",main="Promedio",ylab="Distancia",sub="")

