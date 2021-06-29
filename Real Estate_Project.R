getwd()
setwd("/Users/akpillai/Desktop/Edvancer/Data/Real_Estate")

house_test=read.csv("housing_test.csv")
house_train=read.csv("housing_train.csv")

house_test$Price=NA
house_test$Data="Test"
house_train$Data="Train"
View(house_test)
View(house_train)

house_all=rbind(house_train,house_test)
View(house_all)

library(dplyr)
glimpse(house_all)

## Missing Value Imputation using Grouped Mean

house_all$BuildingArea<-round(ave(house_all$BuildingArea,house_all$Suburb,house_all$Type,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x)),2)

house_all$Landsize<-round(ave(house_all$Landsize,house_all$Suburb,house_all$Type,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x)),2)

house_all$Bedroom2<-round(ave(house_all$Bedroom2,house_all$Suburb,house_all$Type,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x)),0)

house_all$Bathroom<-round(ave(house_all$Bathroom,house_all$Suburb,house_all$Type,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x)),0)

house_all$Car<-round(ave(house_all$Car,house_all$Suburb,house_all$Type,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x)),0)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

house_all=CreateDummies(house_all ,"Type")

table(house_all$Method)
house_all=CreateDummies(house_all ,"Method")
house_all=CreateDummies(house_all ,"Suburb",freq_cutoff=25)


View(house_all)
glimpse(house_all)

is.na(house_all$Bedroom2)
View(house_all)

house_all=house_all %>% 
  select(-Address,-YearBuilt,-CouncilArea)

house_all=house_all %>% 
  select(-SellerG)

house_train=house_all %>% filter(Data=='Train') %>% select(-Data)
house_test=house_all %>% filter(Data=='Test') %>% select(-Data,-Price)

set.seed(2)
s=sample(1:nrow(house_train),0.7*nrow(house_train))
house_train1=house_train[s,]
house_train2=house_train[-s,]

library(tidyr)

fit=lm(Price~.-Method_S-Postcode,data=house_train1)
summary(fit)
head(house_train1)


library(car)
               
sort(vif(fit),decreasing = T)

summary(fit)
fit=step(fit)
formula(fit)

fit=lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + 
     Car + Landsize + BuildingArea + Type_u + Type_h + Method_VB + 
     Method_SP + Method_PI + Suburb_Kingsville + Suburb_CaulfieldNorth + 
     Suburb_Hughesdale + Suburb_CarltonNorth + Suburb_Southbank + 
     Suburb_MontAlbert + Suburb_Watsonia + Suburb_HamptonEast + 
     Suburb_Heidelberg + Suburb_Oakleigh + Suburb_CaulfieldSouth + 
     Suburb_Fitzroy + Suburb_Braybrook + Suburb_Carlton + Suburb_Canterbury + 
     Suburb_Chadstone + Suburb_CliftonHill + Suburb_Flemington + 
     Suburb_KewEast + Suburb_AlbertPark + Suburb_BoxHill + Suburb_Seddon + 
     Suburb_Ashwood + Suburb_Elsternwick + Suburb_Collingwood + 
     Suburb_Altona + Suburb_Hadfield + Suburb_Abbotsford + Suburb_HeidelbergWest + 
     Suburb_NorthMelbourne + Suburb_OakleighSouth + Suburb_CoburgNorth + 
     Suburb_Murrumbeena + Suburb_HeidelbergHeights + Suburb_Malvern + 
     Suburb_Moorabbin + Suburb_SouthMelbourne + Suburb_Ashburton + 
     Suburb_BrunswickEast + Suburb_Maidstone + Suburb_FitzroyNorth + 
     Suburb_Ormond + Suburb_SunshineNorth + Suburb_WestFootscray + 
     Suburb_AvondaleHeights + Suburb_Fawkner + Suburb_AltonaNorth + 
     Suburb_Armadale + Suburb_Burwood + Suburb_Williamstown + 
     Suburb_Melbourne + Suburb_SunshineWest + Suburb_TemplestoweLower + 
     Suburb_BrunswickWest + Suburb_KeilorEast + Suburb_HawthornEast + 
     Suburb_SurreyHills + Suburb_Kensington + Suburb_Toorak + 
     Suburb_Elwood + Suburb_Maribyrnong + Suburb_Newport + Suburb_Doncaster + 
     Suburb_AscotVale + Suburb_Footscray + Suburb_Thornbury + 
     Suburb_Hampton + Suburb_Yarraville + Suburb_Balwyn + Suburb_MalvernEast + 
     Suburb_Camberwell + Suburb_Carnegie + Suburb_Bentleigh + 
     Suburb_PascoeVale + Suburb_BrightonEast + Suburb_Hawthorn + 
     Suburb_BalwynNorth + Suburb_Coburg + Suburb_Kew + Suburb_Brighton + 
     Suburb_GlenIris + Suburb_Brunswick + Suburb_StKilda + Suburb_Preston + 
     Suburb_Richmond + Suburb_BentleighEast + Suburb_Reservoir, 
   data = house_train1)


val.pred=predict(fit,newdata=house_train2)

errors=house_train2$Price-val.pred

errors**2


errors**2 %>% na.omit(a) %>% mean() %>% sqrt()

val.predActual=predict(fit,newdata=house_test)
val.predActual
