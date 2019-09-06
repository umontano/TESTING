library(rgl)

# alberto_base <- read_sav("Box Sync/Dropbox/SY2/SYNC2SHARED/Manuscrito-Temperamento-Adquisición-Desarrollo/DATA/BLINDNESSkaren/DATOS/-alberto-base.sav")
View(alberto_base)
#View(karentodo)

alberto_base <- alberto_base[-c(357:362), ]
alberto_base <- alberto_base[-356, ]
alberto_base <- alberto_base[-355, ]

#QUITAR ROWS WITH NA


names(alberto_base)
head(alberto_base)
traitsOnly = alberto_base[ , 16:31]
head(traitsOnly)

#QUITAR ROWS WITH NA
traitsOnly = traitsOnly[complete.cases(traitsOnly), ]


#DATOS
#ANTDATA= antmeasures

#View(Sur_AN_CE_Division2grupos)
CATEGORICALGROUP= risk$profile
#DATABASE = risk
#DATABASE$profile

alberto_base
data

DATABASE = alberto_base
#QUITAR ROWS WITH NA
DATABASE <- DATABASE[complete.cases(DATABASE$Niveldeactividad), ]

DATABASE$ConteocorrectoyTarget

DATABASE$ConteocorrectoyTarget = as.factor(DATABASE$ConteocorrectoyTarget)

DATABASE$Target_indep_conteo = as.factor(DATABASE$Target_indep_conteo)

DATABASE$Target_indep_conteo

#training <- traits
training <- traitsOnly

#adds the categorical innattentional blindness column
training$conteoYtarget <- DATABASE$ConteocorrectoyTarget

# STANDARDIZED MATRIX VALUES
#training <- traits
training <- scale.default(training)

#clean train matrix, delete NA rows
training <- training[complete.cases(training$conteoYtarget), ]

# k means clustering algorithm
library(psych)

resultsk <- kmeans(training, 3)

summary(resultsk)

print(resultsk$centers)

library(ggplot2)
resultsk$cluster <- as.factor(resultsk$cluster)
ggplot(training, aes(training$Frustracin , training$conteoYtarget , color = resultsk$cluster)) + geom_point()

ggplot(training, aes(training$Frustracin , training$conteoYtarget , color = resultsk$cluster)) + geom_point()


ggplot(training, aes(training$Autotranquilizacion , training$conteoYtarget , color = resultsk$cluster)) + geom_point()

ggplot(training, aes(training$controlInhibitorio , training$conteoYtarget , color = resultsk$cluster)) + geom_point()
ggplot(training, aes(training$Placerbajaintensidad , training$conteoYtarget , color = resultsk$cluster)) + geom_point()
ggplot(training, aes(training$Sensibilidadperceptual , training$conteoYtarget , color = resultsk$cluster)) + geom_point()
ggplot(training, aes(training$Atencionfoc , training$conteoYtarget , color = resultsk$cluster)) + geom_point()
ggplot(training, aes(training$distractibilidad , training$conteoYtarget , color = resultsk$cluster)) + geom_point()




Karen_base$CEAltosBajos <- as.factor(Karen_base$CEAltosBajos)

#T TESTS grupo 1; CE ALTO
t.test(Karen_base$ConteocorrectoyTarget1213[Karen_base$CEAltosBajos==1]
       , mu=0.5, alternative="greater")
#T TESTS grupo 0; CE BAJP
t.test(Karen_base$ConteocorrectoyTarget1213[Karen_base$CEAltosBajos==0]
       , mu=0.5, alternative="less")

#chisq.test(Karen_base$ConteocorrectoyTarget1213[Karen_base$CEAltosBajos==0])
#chisq.test(Karen_base$CEAltosBajos, 
       Karen_base$ConteocorrectoyTarget1213)

#GENERATEs CONTINGENCY TABLE FOR GROUP 0, LOW EFFORTFUL CONTROL
tabulacionTargetCE <- table(Karen_base$ConteocorrectoyTarget1213[
      Karen_base$CEAltosBajos==0] )

#BINOMIAL TEST for GRUOP 0, LOW EC, TWO SIDED
binom.test(tabulacionTargetCE, p=0.5, alternative="two.sided", conf.level=0.95)

#BINOMIAL TEST ONE SIDED, ONE TAILED
binom.test(tabulacionTargetCE, p=0.5, alternative="greater", conf.level=0.95)



#GENERATEs CONTINGENCY TABLE FOR GROUP 1, HIGH EFFORTFUL CONTROL
tabulH <- table(Karen_base$ConteocorrectoyTarget1213[
  Karen_base$CEAltosBajos==1] )

#BINOMIAL TEST for GRUOP 1, HIGH EC, TWO SIDED
binom.test(tabulH, p=0.5, alternative="two.sided", conf.level=0.95)
#BINOMIAL TEST for GRUOP 1, HIGH EC, one SIDED less
binom.test(tabulH, p=0.5, alternative="less", conf.level=0.95)



t.test(Karen_base$ConteocorrectoyTarget1213 ~
         Karen_base$CEAltosBajos, alternative="two.sided")
t.test(Karen_base$ConteocorrectoyTarget1213 ~
         Karen_base$CEAltosBajos, alternative="greater")
t.test(Karen_base$ConteocorrectoyTarget1213 ~
         Karen_base$CEAltosBajos, alternative="less")


t.test(Karen_base$ConteocorrectoyTarget1213 ~
         Karen_base$ANAltosBajos, alternative="greater")
t.test(Karen_base$ConteocorrectoyTarget1213 ~
         Karen_base$ANAltosBajos, alternative="less")


t.test(Karen_base$ConteocorrectoyTarget1213 ~
         Karen_base$SurgencyAltosBajos, alternative="greater")

t.test(Karen_base$ConteocorrectoyTarget1213 ~
         Karen_base$SurgencyAltosBajos, alternative="less")

#CHI SQUARED TEST
CHISQRESULTS <- chisq.test(Karen_base$ConteocorrectoyTarget1213 ~
             Karen_base$CEAltosBajos, mu=0.5)
summary(CHISQRESULTS)

#ANOVA
fit <- aov(Karen_base$ConteocorrectoyTarget1213 ~
             Karen_base$CEAltosBajos, data =Karen_base, na.exclude=TRUE)

summary(fit)
TukeyHSD(fit)

#INSTALL RGL library for 3D plotting
install.packages("rgl")

###We’ll plot the scores along the  principal components
library(rgl)


###The random seed is set for reprodicibility 
set.seed(42)
cl <- kmeans(iris[,1:4],3)
iris$cluster <- as.factor(cl$cluster)


and then we save the cluster assignments from k-means as a new column in the data frame. 

###We can take a look at how well this works, visually and by tabulation.

plot3d(pc$scores[,1:3], col=iris$cluster, main="k-means clusters")
plot3d(pc$scores[,1:3], col=iris$Species, main="actual species")