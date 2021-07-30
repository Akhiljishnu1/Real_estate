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

house_all$BuildingArea[is.nan(house_all$BuildingArea)]=mean(house_all$BuildingArea,na.rm = TRUE)

house_all$Landsize<-round(ave(house_all$Landsize,house_all$Suburb,house_all$Type,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x)),2)

house_all$Landsize[is.nan(house_all$Landsize)]=mean(house_all$Landsize,na.rm = TRUE)

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

mean_suburb=round(tapply(house_all$Price,house_all$Suburb,mean,na.rm=T))
sort(mean_suburb)

house_all=house_all %>% 
  mutate(surb40_50=as.numeric(Suburb %in% c("Campbellfield","Jacana","Kealba")),
         surb50_60=as.numeric(Suburb %in% c("Brooklyn","Albion","Sunshine West","Ripponlea","Fawkner","Glenroy")),
         surb60_70=as.numeric(Suburb %in% c("Southbank","Sunshine North","Keilor Park","Heidelberg West","Reservoir","Braybrook","Kingsbury","Gowanbrae","Hadfield","Watsonia","Footscray","South Kingsville","Balaclava","Melbourne","Maidstone","Sunshine")),
         surb70_80=as.numeric(Suburb %in% c("Airport West Heidelberg Heights","Pascoe Vale","West Footscray","Altona North","Williamstown North","Brunswick West","Keilor East","Oak Park","Maribyrnong","Altona","Flemington","Coburg North","Yallambie","Avondale Heights","Bellfield")),
         surb80_90=as.numeric(Suburb %in% c("Strathmore Heights","Glen Huntly","Kensington","Essendon North","St Kilda","Preston","North Melbourne","Coburg","Kingsville","Collingwood","Brunswick East","Gardenvale","Thornbury","Niddrie","West Melbourne","Viewbank")),
         surb90_100=as.numeric(Suburb %in% c("Spotswood","Carnegie","Elwood","Heidelberg","Moorabbin","Oakleigh","Rosanna","Docklands","Yarraville","Cremorne","Seddon","Brunswick","Oakleigh South","Ascot Vale","Windsor","Caulfield","Essendon West","Newport")),
         surb100_110=as.numeric(Suburb %in% c("Chadstone","South Yarra","Essendon","Bentleigh East","Murrumbeena","Hughesdale","Fairfield","Ashwood","Clifton Hill","Caulfield North","Abbotsford","Carlton","Prahran","Fitzroy","Ivanhoe","Hampton East","Caulfield East")),
         surb110_120=as.numeric(Suburb %in% c("Richmond","Travancore","Templestowe Lower","Ormond","Caulfield South","Moonee Ponds","Hawthorn","Box Hill","Bulleen","Burnley","Burwood","Strathmore","Port Melbourne","Fitzroy North","Alphington")),
         surb120_130=as.numeric(Suburb %in% c("Doncaster","South Melbourne","Northcote","Aberfeldie","Elsternwick","Bentleigh","Kooyong","Parkville")),
         surb130_140=as.numeric(Suburb %in% c("Williamstown","East Melbourne","Seaholme")),
         surb140_150=as.numeric(Suburb %in% c("Malvern East","Carlton North","Hawthorn East","Surrey Hills")),
         surb150_160=as.numeric(Suburb %in% c("Princes Hill","Mont Albert","Armadale","Kew East","Glen Iris","Ashburton")),
         surb160_170=as.numeric(Suburb %in% c("Brighton East","Eaglemont","Hampton")),
         surb170_180=as.numeric(Suburb %in% c("Toorak","Ivanhoe East","Camberwell","Balwyn North","Kew")),
         surb180_190=as.numeric(Suburb %in% c("Brighton","Middle Park")),
         surb190_200=as.numeric(Suburb %in% c("Balwyn","Albert Park","Malvern")),
         surb200_222=as.numeric(Suburb %in% c("Canterbury"))) %>% 
  
select(-Suburb)
glimpse(house_all)

View(house_all)
glimpse(house_all)

is.na(house_all$Bedroom2)
View(house_all)

house_all=house_all %>% 
  select(-Address,-YearBuilt,-CouncilArea)

house_all=house_all %>% 
  select(-SellerG)

house_all=house_all %>% 
  select(-Postcode)

house_train=house_all %>% filter(Data=='Train') %>% select(-Data)
house_test=house_all %>% filter(Data=='Test') %>% select(-Data,-Price)

set.seed(2)
s=sample(1:nrow(house_train),0.7*nrow(house_train))
house_train1=house_train[s,]
house_train2=house_train[-s,]

library(tidyr)

fit=lm(Price~.-Method_S,data=house_train1)
summary(fit)
head(house_train1)


library(car)
               
sort(vif(fit),decreasing = T)

summary(fit)
fit=step(fit)
formula(fit)

fit=lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + 
         BuildingArea + Type_u + Type_h + Method_VB + Method_SP + 
         Method_PI + surb40_50 + surb50_60 + surb80_90 + surb90_100 + 
         surb100_110 + surb110_120 + surb120_130 + surb130_140 + surb140_150 + 
         surb150_160 + surb160_170 + surb170_180 + surb180_190 + surb190_200 + 
         surb200_222,data = house_train1)


val.pred=predict(fit,newdata=house_train2)
sum(is.nan(val.pred))

errors=house_train2$Price-val.pred

errors**2


errors**2 %>% mean() %>% sqrt()

val.predActual=predict(fit,newdata=house_test)
val.predActual

write.csv(val.predActual,"real_estate_output11.csv",row.names = F)
