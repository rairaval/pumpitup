## Entrega Raul. 

####### Tratamiento de los datos. 

set.seed(42)
paquetes = c('data.table', 'inspectdf', 'dplyr', 'ranger', 'caret', 'ggplot2', 'lubridate', 'stringr', 'geosphere', 'Rborist', 'MLmetrics', 'questionr', 'psych', 'car', 'corrplot', 'qgraph', 'devtools', 'FactoMineR', 'RcmdrMisc', 'GPArotation', 'remotes', 'caret', 'ggplot2', 'lmSupport', 'factoextra', 'cluster', 'fpc', 'clValid', 'missForest', 'mice', 'VIM', 'rpart', 'rpart.plot', 'readr', 'knitr', 'fastDummies')

lapply(paquetes, require, character.only = TRUE)


######## CARGO LOS DATOS

setwd("C:/Users/Usuario/Desktop/MASTER/MINERIA DE DATOS(3erdatos)")

dat_label <- as.data.frame(fread("01_labels.csv", nThread = 4 )) ; dim(dat_label)
## La variable objetivo como status_group.
## [1] 59400     2
dat_train <- as.data.frame(fread("02_trainset.csv", nThread = 4 )); dim(dat_train)
## [1] 59400    40
dat_test <- as.data.frame(fread('03_testset.csv', nThread = 4 )) ; dim(dat_test)
## [1] 14850    40

######## Uno el label y train 

all.equal(dat_label[,1], dat_train[,1])
## [1] TRUE
datEnd <- merge(dat_train, dat_label, by.x = "id", by.y="id", all = TRUE) #cuenta con la variable objetivo.

######## Preservo datEnd como los datos en bruto

datEnd2 = datEnd
str(datEnd2)
#Mayormente datos como character, procedo a pasarlas a factor.
colnames(datEnd2)

######## Paso variables a Factor.
#Convertimos: waterpoint_type_group, basin, region, scheme_management, extraction_type, extraction_type_group, extraction_type_class, management, management_group, payment, payment_type, water_quality, quality_group, quantity, quantity_group, source, source_type, source_class, waterpoint_type, status_group, permit, public_meeting
datEnd2[,c(11, 13, 19, 21, 23, 25:41)] = lapply(datEnd2[,c(11, 13, 19, 21, 23, 25:41)], factor)
colnames(dat_test)
dat_test[,c(11, 13, 19, 21, 23, 25:40)] = lapply(dat_test[,c(11, 13, 19, 21, 23, 25:40)], factor)

######## Creacion de las variables Dummmies. 
#PERMIT, PUBLIC_meeting, Managment_group.

######## PERMIT
# train
datEnd2 = datEnd2 %>% mutate(permit_SI = ifelse (permit == 'TRUE', 1, 0)) 
datEnd2 = datEnd2 %>% mutate(permit_SI = ifelse (is.na(permit), 0, permit_SI)) 
datEnd2 = datEnd2 %>% mutate(permit_NO = ifelse (permit == 'FALSE', 1, 0)) 
datEnd2 = datEnd2 %>% mutate(permit_NO = ifelse (is.na(permit), 0, permit_NO)) 

# test
dat_test = dat_test %>% mutate(permit_SI = ifelse (permit == 'TRUE', 1, 0)) 
dat_test = dat_test %>% mutate(permit_SI = ifelse (is.na(permit), 0, permit_SI)) 
dat_test = dat_test %>% mutate(permit_NO = ifelse (permit == 'FALSE', 1, 0)) 
dat_test = dat_test %>% mutate(permit_NO = ifelse (is.na(permit), 0, permit_NO)) 

######## PUBLIC
# train
datEnd2 = datEnd2 %>% mutate(public_SI = ifelse (public_meeting == 'TRUE', 1, 0)) 
datEnd2 = datEnd2 %>% mutate(public_SI = ifelse (is.na(public_meeting), 0, public_SI)) 
datEnd2 = datEnd2 %>% mutate(public_NO = ifelse (public_meeting == 'FALSE', 1, 0)) 
datEnd2 = datEnd2 %>% mutate(public_NO = ifelse (is.na(public_meeting), 0, public_NO)) 

# test
dat_test = dat_test %>% mutate(public_SI = ifelse (public_meeting == 'TRUE', 1, 0)) 
dat_test = dat_test %>% mutate(public_SI = ifelse (is.na(public_meeting), 0, public_SI)) 
dat_test = dat_test %>% mutate(public_NO = ifelse (public_meeting == 'FALSE', 1, 0)) 
dat_test = dat_test %>% mutate(public_NO = ifelse (is.na(public_meeting), 0, public_NO)) 

######### MANAGEMENT_GROUP
#summary(datEnd2$management_group)
#commercial      other parastatal    unknown user-group 
# 3638            943       1768        561      52490 

# train
datEnd2 = datEnd2 %>% mutate(management_groupc = ifelse (management_group == 'commercial', 1, 0)) 
datEnd2 = datEnd2 %>% mutate(management_groupo = ifelse (management_group == 'other', 1, 0)) 
datEnd2 = datEnd2 %>% mutate(management_groupp = ifelse (management_group == 'parastatal', 1, 0)) 
datEnd2 = datEnd2 %>% mutate(management_groupu = ifelse (management_group == 'user-group', 1, 0)) 
datEnd2 = datEnd2 %>% mutate(management_groupg = ifelse (management_group == 'unknown', 1, 0)) 

# test
dat_test = dat_test %>% mutate(management_groupc = ifelse (management_group == 'commercial', 1, 0)) 
dat_test = dat_test %>% mutate(management_groupo = ifelse (management_group == 'other', 1, 0)) 
dat_test = dat_test %>% mutate(management_groupp = ifelse (management_group == 'parastatal', 1, 0)) 
dat_test = dat_test %>% mutate(management_groupu = ifelse (management_group == 'user-group', 1, 0)) 
dat_test = dat_test %>% mutate(management_groupg = ifelse (management_group == 'unknown', 1, 0)) 

######################################
#### Creación de variables adicionales
######################################

### Registro del pozo.
### Antiguedad del pozo en dias a partir de 01-01-2014. Dado que el ultimo pozo registrado data de dic. 2013

# train

datEnd2$dif_days <- as.numeric(as.Date("2014-01-01") - as.Date(datEnd2$date_recorded))
datEnd2$reg_month <- month(ymd(datEnd2$date_recorded))
datEnd2$reg_year <- year(ymd(datEnd2$date_recorded))

# test
dat_test$dif_days <- as.numeric(as.Date("2014-01-01") - as.Date(dat_test$date_recorded))
dat_test$reg_month <- month(ymd(dat_test$date_recorded))
dat_test$reg_year <- year(ymd(dat_test$date_recorded))

### Localizacion del pozo. 

# train
datEnd2$dist_geo <- distGeo(as.matrix(datEnd2[,c('longitude','latitude')]), c(0,0))

# test
dat_test$dist_geo <- distGeo(as.matrix(dat_test[,c('longitude','latitude')]), c(0,0))

###################################
#### Transformaciones de variables. 
###################################


### Longitud.

# train
longsummary <- aggregate(longitude~region,data=datEnd2[(datEnd2$longitude!=0),], FUN=mean)
datEnd2$longitud_ok = datEnd2$longitud
nrow(datEnd2[datEnd2$longitud_ok==0,]) 

listado_regiones = levels(datEnd2$region)
for (r in listado_regiones){
  datEnd2$longitud_ok[datEnd2$region== r & datEnd2$longitude==0] <- longsummary$longitude[longsummary== r]}


# test
dat_test$longitud_ok = dat_test$longitud
nrow(dat_test[dat_test$longitud_ok==0,]) 

listado_regiones_test = levels(dat_test$region)
for (r in listado_regiones_test){
  dat_test$longitud_ok[dat_test$region== r & dat_test$longitude==0] <- longsummary$longitude[longsummary== r]}

### Poblacion. 

datEnd2$population_sc = scale(datEnd2$population)
dat_test$population_sc = scale(dat_test$population)

############################
#AGRUPACIONES DE VARIABLES##
############################

varObjCont_num = datEnd2$status_group

###scheme_management

tree<-rpart(varObjCont_num~scheme_management, data=datEnd2) ;tree
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE) #Vemos el arbol
tree

da<-cbind(var=datEnd2$scheme_management,tree=tree$where,obj=varObjCont_num)

aggregate(var~tree, data=da, mean) ; aggregate(obj~tree, data=da, mean)
datEnd2$scheme_management_agrup<-factor(tree$where); levels(datEnd2$scheme_management_agrup)

tabla.agrupada = datEnd2 %>% dplyr::group_by(scheme_management_agrup, scheme_management) %>% dplyr::summarize(total = n())

agrupador1 <- merge(dat_test, tabla.agrupada, by.x = "scheme_management", by.y="scheme_management", all.x =TRUE)
agrupador1 = agrupador1 <- subset(agrupador1, select = c(id, scheme_management_agrup))

dat_test <- merge(dat_test, agrupador1, by.x = "id", by.y="id", all.x =TRUE, sort= FALSE)

#summary(datEnd2$scheme_management_agrup)

##REGION

freq(datEnd2$region)

tree<-rpart(varObjCont_num~region, data=datEnd2) ;tree

da2<-cbind(var=datEnd2$region,tree=tree$where,obj=varObjCont_num)

rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE) #Vemos el arbol 

aggregate(var~tree, data=da, mean) ; aggregate(obj~tree, data=da, mean)

datEnd2$region_agrup<-factor(tree$where); levels(datEnd2$region_agrup) 

tabla.agrupada = datEnd2 %>% dplyr::group_by(region_agrup, region) %>% dplyr::summarize(total = n())
agrupador1 <- merge(dat_test, tabla.agrupada, by.x = "region", by.y="region", all.x =TRUE) #, by.y="region"
agrupador1 = agrupador1 <- subset(agrupador1, select = c(id, region_agrup))

dat_test <- merge(dat_test, agrupador1, by.x = "id", by.y="id", all.x =TRUE, sort= FALSE)

ImputacionCuali<-function(vv,tipo){#tipo debe tomar los valores moda o aleatorio
  if (tipo=="moda"){
    vv[is.na(vv)]<-names(sort(table(vv),decreasing = T))[1]
  } else if (tipo=="aleatorio"){
    vv[is.na(vv)]<-sample(vv[!is.na(vv)],sum(is.na(vv)),replace = T)
  }
  factor(vv)
}
dat_test[,as.vector(which(sapply(dat_test, class)=="factor"))]<-sapply(Filter(is.factor, dat_test),function(x) ImputacionCuali(x,"aleatorio"))
freq(dat_test$region_agrup)
dat_test$region_agrup = as.factor(dat_test$region_agrup)


#### MODELO ####

library(ranger)
my_mod <- ranger( 
  as.factor(status_group) ~ amount_tsh + longitude + latitude + gps_height + num_private + region_code + district_code + population + construction_year,
  data = datEnd2, importance = 'impurity',mtry=6,
  min.node.size=1.5,
  num.trees = 700,
  max.depth=0,
  verbose = TRUE
)

my_mod$confusion.matrix

#####
my_mod2 <- ranger( 
  as.factor(status_group) ~ amount_tsh +longitud_ok + latitude + 
    gps_height  +  district_code + population + construction_year +
    dif_days + reg_month +  dist_geo +   
    basin + region_agrup + scheme_management_agrup + 
    extraction_type + extraction_type_group +
    extraction_type_class + 
    management + 
    management_groupc +management_groupo + management_groupp +
    management_groupu + payment + payment_type + 
    water_quality + quality_group +      
    quantity + quantity_group + source +
    source_type + source_class +     
    waterpoint_type + waterpoint_type_group + 
    permit_SI + public_SI + funder+ subvillage + ward,
  data = datEnd2, importance = 'impurity',mtry=6,
  min.node.size=1.5,
  num.trees = 700,
  max.depth=0,
  verbose = TRUE
)

my_mod2$confusion.matrix

##

my_mod12 <-  
  # a pesar de la alta correlación o de existencia de 
  # variables iguales el modelo da mejores resultados incluyéndolas
  
  ranger(as.factor(status_group) ~  
           amount_tsh  +longitud_ok + latitude + 
           gps_height  +  district_code  + construction_year +
           dif_days + reg_month + dist_geo +     
           basin + region_agrup + scheme_management_agrup + 
           extraction_type + extraction_type_group +
           extraction_type_class + 
           management + 
           management_groupc +management_groupo + management_groupp +
           management_groupu + payment + payment_type + 
           water_quality + quality_group +      
           quantity + quantity_group + source +
           source_type + source_class +     
           waterpoint_type + waterpoint_type_group + 
           permit_SI + public_SI + subvillage + ward,      
         data = datEnd2, 
         importance = "impurity", 
         mtry=6,
         min.node.size=1.5,
         num.trees = 700,
         max.depth=0,
         verbose = TRUE,      
         seed = 1234) 

pred_mod12 <- predict(my_mod12, datEnd2)
confusionMatrix( pred_mod12$predictions, as.factor(datEnd2$status_group))

###############################################################################################
#### Submission ####

pred_test1 <- as.vector(predict(my_mod, dat_test)$prediction)
my_sub12 <- data.frame(id = dat_test$id, status_group = pred_test1)
fwrite(my_sub12, file="submission_pump_it_up1.csv", sep=",")

#Score: 0.7263

#######################################
pred_test2 <- as.vector(predict(my_mod2, dat_test)$prediction)
my_sub12 <- data.frame(id = dat_test$id, status_group = pred_test2)
fwrite(my_sub12, file="submission_pump_it_up2.csv", sep=",")

#######################################
pred_test12 <- as.vector(predict(my_mod12, dat_test)$prediction)
my_sub12 <- data.frame(id = dat_test$id, status_group = pred_test12)
fwrite(my_sub12, file="submission_pump_it_up.csv", sep=",")

#Score: 0.8199


