#Autor: Pedro Herrero Petisco
#email: pedroherreropetisco (arroba) gmail.com



# CREACIÓN DE LAS FUNCIONES

DrawBoard<-function(             XminLim=0, 
                                 XmaxLim=1000, 
                                 YminLim=-3, 
                                 YmaxLim=3,
                                 DrawAxis=TRUE,
                                 Labels=FALSE,
                                 Ticks=FALSE){
               # XminLim = Límite inferior del eje X
               # XmaxLim = Límite superior del eje X
               # YminLim = Límite inferior del eje Y
               # YmaxLim = Límite superior del eje Y
               # DrawAxis = Indica si se dibuja el Eje principal de tiempo
               # Labels = Indica si se ponen etiquetas a eje principal Puede ser un conjunto de caracteres indicando las etiquetas
               # Ticks = indica si aparecen las marcas de eje
                                 
                                 
                plot(1,1,axes=FALSE, xlab="",ylab="",xlim=c(XminLim,XmaxLim),ylim=c(YminLim,YmaxLim),type="n")
                if(DrawAxis) axis(1, pos=(0),labels=Labels,tick=Ticks)}


DrawTimeLine<- function(x, y, z, 
                                 XminLim=min(x), 
                                 XmaxLim=max(x), 
                                 YminLim=0, 
                                 YmaxLim=(max(y)+1), 
                                 color="blue",
                                 DrawBoard=TRUE, 
                                 Lines=TRUE, 
                                 Points=TRUE, 
                                 Text=FALSE, 
                                 FirstText=TRUE, 
                                 FirstTextChar=z[1]){
               # x = Variable con la posición en el eje de tiempo
               # y = Nivel del la variable
               # Z = Texto a poner en pantalla
               # XminLim = Límite inferior del eje X
               # XmaxLim = Límite superior del eje X
               # YminLim = Límite inferior del eje Y
               # YmaxLim = Límite superior del eje Y
               # DrawBoard = Dice si se dibuja el tablero. Si no se ha usado antes DrawBoard y es el primer TimeLine de la petición hay que dejar le valor por defecto (TRUE). Si en otro que queremos dibujar encima de otra ya creada cambiar a FALSE
               # Lines = Indica si se dibujarán las líneas
               # Points = Indica si se dibujarán los puntos
               # Text = Indica si se pondrá un texto en todos los puntos
               # FirstText = Indica si se pone un texto en el primer punto
               # FirstTextChar = Indica el texto a poner en el primer punto. Por defecto es el mismo texto que z[1]

               #comprobamos si hay que dibujar el eje
               if (DrawBoard==TRUE) plot(1,1,axes=FALSE, xlab="",ylab="",xlim=c(XminLim,XmaxLim),ylim=c(YminLim,YmaxLim),type="n")
               if (Lines) lines(x,y,col=color)
               if (Points) points(x,y,col=color,pch=2)
               if (Text) for (i in 1:length(x)) text(x[i],y[i]+0.5,label=z[i],col=color)
               if (FirstText) text(x[1],y[1]+0.5,label=FirstTextChar,col=color)
               }

WriteEvent<-function(x,y,z){
               # x = Variable con la posición en el eje de tiempo
               # y = Nivel del la variable (se situará en negativo, por debajo del eje X)
               # Z = Texto a poner en pantalla
               text(x,-(y),paste("¬ ",z),pos=4) #pos=4 hace que el texto se sitúe a la derecha del punto elegido 
               }

#################################################3Prueba


# Generamos el fichero Uncanny X Men de Prueba
UncannyXMen<-data.frame(c(1:66,96:345,-1,346:544),"Uncanny X Men",1,1)
names(UncannyXMen)<-c("Numero","Nombre","Orden","Nivel")

for (i in 1:nrow(UncannyXMen)){
if (as.numeric(UncannyXMen$Numero[i])<346) UncannyXMen$Orden[i]<-((UncannyXMen$Numero[i]))*1000
if (as.numeric(UncannyXMen$Numero[i])>=346) UncannyXMen$Orden[i]<-((UncannyXMen$Numero[i])+1)*1000
if (UncannyXMen$Numero[i]=="-1") UncannyXMen$Orden[i]<-346000
}

# Generamos el fichero Avengers de Prueba
Avengers<-data.frame(c(3,16,53),"Avengers",c(3500,16500,45500),1)
names(Avengers)<-c("Numero","Nombre","Orden","Nivel")

  # Generamos el fichero Falso XMen de Prueba

XMen<-data.frame(c(1:66),"X Men",1,2)
names(XMen)<-c("Numero","Nombre","Orden","Nivel")


UXMenSpecial<-data.frame(1,"Giant Size X-Men",95000,1)
names(UXMenSpecial)<-c("Numero","Nombre","Orden","Nivel")


for (i in 1:nrow(XMen)){
if (as.numeric(XMen$Numero[i])<346) XMen$Orden[i]<-((XMen$Numero[i]))*1000
if (as.numeric(XMen$Numero[i])>=346) XMen$Orden[i]<-((XMen$Numero[i])+1)*1000
if (XMen$Numero[i]=="-1") XMen$Orden[i]<-346000
}
for (i in 1:nrow(XMen)){
if (as.numeric(XMen$Numero[i])<20) XMen$Nivel[i]<-2
if (as.numeric(XMen$Numero[i])>=21) XMen$Nivel[i]<-1
if (XMen$Numero[i]==20) XMen$Nivel[i]<-3}








DrawBoard(XminLim=0,XmaxLim=200000)
DrawTimeLine(UncannyXMen$Orden,UncannyXMen$Nivel,UncannyXMen$Numero,XmaxLim=66000,FirstTextChar="UXMen",DrawBoard=FALSE)
DrawTimeLine(Avengers$Orden,Avengers$Nivel,Avengers$Numero,color="green",DrawBoard=FALSE, Lines=FALSE,FirstTextChar="Aveng", Text=TRUE)
DrawTimeLine(XMen$Orden,XMen$Nivel,XMen$Numero,color="red",DrawBoard=FALSE,FirstTextChar="XMen", Text=FALSE)
DrawTimeLine(UXMenSpecial$Orden,UXMenSpecial$Nivel,UXMenSpecial$Numero,color="grey",DrawBoard=FALSE,FirstTextChar="Giant Size XMen")
WriteEvent(95000,1,"Comienzo de la segunda genesis")


#FUTURO:
# Dibujar un área para indicar suceso que se aargan en un periodo de tiempo (como una saga de cómics)
# Dibujar textos en la parte inferior
