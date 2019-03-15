datos <- read.table("todos.txt", header = TRUE)

opt<- as.factor(datos[,1])
fin <- as.factor(datos[,2])
ocu <- as.factor(datos[,3])
acc <- datos[,4]
datos$ocu = factor(datos$ocu)
datos$fin = factor(datos$fin)
head(datos)
str(datos)
plot.design(acc ~ ., data = datos)

op <- par(mfrow = c(3, 1))
with(datos, {
  interaction.plot(opt, fin, acc)
  interaction.plot(opt, ocu, acc)
  interaction.plot(fin, ocu, acc)
  }
)
par(op)
fm <- aov(acc ~ opt + fin + ocu + opt*fin + opt*ocu +
          fin*ocu + opt*fin*ocu, data = datos)
summary(g)
fm <- update(g, . ~ . - opt:fin:ocu )
summary(fml)

fml <- update(fml, .~opt+fin+ocu)
summary(fm2)

anova(fm, fml)

model.tables(fml, type = "effects")

model.tables(fml, type = "means")

fm <- aov(acc ~ opt+fin+ocu, data = datos)
#fm <- aov(acc~ opt, data = datos)
summary(fm)

op <- par(mfrow = c(2, 2))
plot(fm)
par(op)