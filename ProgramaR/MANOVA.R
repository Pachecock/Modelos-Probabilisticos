library(plotly)
Dataset <- read.table("C:/Users/Alan/Desktop/datos.txt", header=TRUE, 
                      sep="", na.strings="NA", dec=".", strip.white=TRUE)
Dataset <- within(Dataset, {
delito <- factor(delito, labels=c('I','II','III','IV'))
estado <- factor(estado, labels=c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17',
                                  '18','19','20','21','22','23','24','25','26','27','28','29','30','31','32'))
})
summary(Dataset)
hist(Dataset$confianza,breaks = c(0.5,1.5,2.5,3.5,4.5),xlim = c(0,5), xlab = "Confianza", ylab = "Frecuencia", 
     main="Histograma")
shapiro.test(Dataset$confianza)
boxplot(confianza~delito, data=Dataset, ylab="Confianza", xlab="Delito")
boxplot(confianza~estado, data=Dataset, ylab="Confianza", xlab="Estados")
Model.1 <- lm(confianza ~ delito + estado + delito:estado -1, data=Dataset)
summary(Model.1)
anova(Model.1)
Delito <- Dataset$delito
Estados <- Dataset$estado
Confianza <- Dataset$confianza
interaction.plot(Estados,Delito,Confianza,col = 2:5)
par(mfrow=c(1,1))
plot(Model.1)
z=matrix(Dataset$confianza,nrow=32,ncol=4)
filled.contour(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32),c(1,2,3,4),z,
               ylab="Delito", xlab="Estados", key.title = title(main = "Confianza"), plot.title = title(main = "Lineas de contorno"))
persp(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32),c(1,2,3,4),z,
      phi=25,theta=35,col= c("blue", "green","orange","red"),zlim = c(-4,8),xlim=c(1,32),ylim=c(1,4),xlab = "Estados",
      ylab = "Delito",zlab="confianza",nticks = 32,axes=TRUE)
