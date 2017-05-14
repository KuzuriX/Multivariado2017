base=read.csv("Resultados cuestionario2.csv",sep=";")
library(tidyr)
library(dplyr)


colnames(base)<-c("marca.temporal","seudonimo", "sexo", "edad", "kg", "estatura", "zona", "trabajo", "beca", "otra.carrera", "hogar", 
                  "admision", "ponderado", "carreras", "hora.estudio", "escolaridad.jefe", "colegio", "ocio", 
                  "dinero.servicios", "mas.tiempo", "menos.tiempo", "dinero.recreativas", "compania",
                  "importancia.recreativa", "dias.fisica", "tiempo.fisica", "razon.fisica", "tipo.fisica", 
                  "motivo.fisica", "sentado", "musica", "genero.antes", "genero.ahora",  "personalidad","instrumento", 
                  "importancia.musica", "musica.actividades", "influencia.musica", "campos.estadistica", "marco.muestral",
                  "tamano.muestra", "cuestionarios", "diseno.exp", "asistente", "clases", "inferencial", "descriptivo",
                  "indicacores", "bases.datos", "discretos", "continuos", "teoria", "compu1", "compu2", "sistemas", 
                  "metodos", "regresion", "experimentos1", "experimentos2", "muestreo", "encuestas",  "info.problema", 
                  "estrategia.datos", "tecnicas", "codigo", "interpretar", "presentar", "redactar", "estetica", "bibliografia", 
                  "telefono", "televisor", "camara", "portatil", "escritorio", "electrodomes", "restaurante", "cine.favorita", 
                  "visitas.cine", "cine.preferencia", "preferencia", "cine.duracion", "drama", "comedia", "accion", "ficcion", "fantasia", 
                  "terror", "romance", "musical", "melodrama", "suspenso", "promedio.comidas", "nutricionista", "harinas", 
                  "verduras", "frutas", "lacteos", "proteinas", "dulces", "frituras", "agua", "alcohol", "chatarra", "redes", 
                  "principal.red", "uso.red", "solicitudes", "peligro.red", "fotos.red", "horas.red", "describe.red")
base$admision= as.numeric(base$admision)
base$admision[26]<-762

#Corrigiendo separador decimal.
base$ponderado<-gsub(",", ".",base$ponderado)
#Conviertiendo la variable en continua.
base$ponderado<-as.numeric(base$ponderado)
#Corrigiendo notas mayores a 10.
base$ponderado<-ifelse(base$ponderado>10, base$ponderado/10, base$ponderado)


#generara base con las  variables nuestras

base2=base[,c(2:4,7,11:13,15,17,31:38)]

#recodificar sexo
base2$sexo= 0*(base2$sexo=="Femenino")+1*(base2$sexo=="Masculino")

#recodificar variable musica
base2$musica=1*(base2$musica=="1 o 2 veces por semana")+2*(base2$musica=="3 o 4 veces por semana")+3*(base2$musica=="5 o más veces por semana")

#genero.antes y geero.ahora eran multiples pero en ningun caso marcaron mas de una entonces no la limpiamos


#personalidad es multiple
base2<-separate(base2, personalidad, into=letters[1:7], sep=", ")


#cambiar las categorias de personalidad a columnas 

#Función 
personalidad1<-function(datos, frase){
  datos%>%
    apply(2, function(x){
      ifelse(x==paste(frase), 1, 0)
    })%>%
    apply(1,sum,na.rm=TRUE)
}


#Creando las variables.   
base2<-base2 %>%
  mutate(autoestimaA=personalidad1(base2[, 13:19], "Alta autoestima"),
         autoestimaB=personalidad1(base2[, 13:19], "Baja autoestima"),
         creativo=personalidad1(base2[, 13:19], "Creativo (a)" ),
         introvertido=personalidad1(base2[, 13:19],"Introvertido (a)"),
         ninguna=personalidad1(base2[, 13:19], "Ninguna"),
         sensible=personalidad1(base2[, 13:19], "Sensible"),
         docil=personalidad1(base2[, 13:19], "Dócil"),
         extrovertido=personalidad1(base2[, 13:19], "Extrovertido (a)"),
         vago=personalidad1(base2[, 13:19], "Vago (a)"),
         desordenado=personalidad1(base2[, 13:19], "Desordenado (a)"),
         pasivo=personalidad1(base2[, 13:19], "Pasivo (a)"),
         enojo=personalidad1(base2[, 13:19], "Se enoja con facilidad")
        )%>%
  select(-c(a:g))

#importancia. musica 
base2$importancia.musica=3*(base2$importancia.musica=="Muy importante")+2*(base2$importancia.musica=="Poco importante")+1*(base2$importancia.musica=="Nada importante")

#musica.actividades
base2$musica.actividades= 4*(base2$musica.actividades=="Siempre")+3*(base2$musica.actividades=="Bastantes veces")+2*(base2$musica.actividades=="Pocas veces")+1*(base2$musica.actividades=="Nunca")

save(base2,file="base.Rdata")
