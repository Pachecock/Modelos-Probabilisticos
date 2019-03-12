datos <- read.table("ultimo.txt", header = TRUE)

optimizador<- as.factor(datos[,1])
final <- as.factor(datos[,2])
ocu <- as.factor(datos[,3])
acc <- datos[,4]
g <- aov(acc ~ optimizador + final + ocu + optimizador*final + optimizador*ocu +
          final*ocu + optimizador*final*ocu, data = datos)
summary(g)

fml <- update(g, .~optimizador)
summary(fml)
       
anova(g, fml)
fin <- aov(acc~ optimizador, data = datos)

summary(fin)
op <- par(mfrow = c(2, 2))
plot(fin)
par(op)