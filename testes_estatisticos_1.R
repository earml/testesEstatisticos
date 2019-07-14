######################################################################################
### ============================================================================= ###
###                  Teste para verificar a igualdade de variância                ###
### ============================================================================= ###
######################################################################################

#########  Cochran  ###########
library(outliers)

genes = paste('Bloco', sprintf("%d",1:10), sep="")
dados = expand.grid(bloco=genes, tratamento=c('T1', 'T2', 'T3', 'T4'))
dados$value = sample(10:20,40,re=T)
dados

cochran.test(bloco~value,dados)#ou
cochran.test(tratamento~value,dados)

library(reshape)
cast(dados, bloco ~ tratamento)

resultado<-aov(value~tratamento+bloco,dados);resultado
modelo<-anova(resultado);modelo

# Verificando as pressuposições do modelo
# Obtenção do resíduo
residuo = rstudent(resultado);residuo

#########  Levene  ###########
library(reshape2)
library(car)

leveneTest(value~bloco, dados)#ou
leveneTest(value~tratamento, dados)

######################################################################################
### ============================================================================= ###
###                  Teste para verificar a Normalidade                           ###
### ============================================================================= ###
######################################################################################

#########  Kolmogorov-Smirnov  ###########

ks.test(residuo, "pnorm") # two-sided, exact

#########     Shapiro-Wilks    ###########

shapiro.test(residuo)


######################################################################################
### ============================================================================= ###
###                  Teste para comparar médias                                   ###
### ============================================================================= ###
######################################################################################

#########     Tukey      ###########
# Compute the analysis of variance
#res.aov <- aov(x ~ group, data = d2)
# Summary of the analysis
#summary(res.aov)

TukeyHSD(resultado)


plot(TukeyHSD(resultado, conf.level=.95))

#obs: diff: difference between means of the two groups
#https://rabovo.wordpress.com/2015/11/25/anova-e-teste-de-medias-pacote-agricolae/
#http://www.sthda.com/english/wiki/one-way-anova-test-in-r

#########     Scheffe      ###########
library(agricolae)
#data(sweetpotato)
#model<-aov(yield~virus, data=sweetpotato)
comparison <- scheffe.test(resultado,"bloco", group=TRUE,console=TRUE,
                           main="Yield of sweetpotato\nDealt with different virus")

#saidaSheffe<-scheffe.test(model,"virus", group=TRUE,console=TRUE,
#                          main="Yield of sweetpotato\nDealt with different virus")

#OBS: Tratamentos com diferentes letras no grupo indicam diferença significativa na média

#########     Dunnet      ###########
library(multcomp)
Dunnet <- glht(resultado, tratamento=mcp(Group="Dunnett"))
summary(Dunnet)

