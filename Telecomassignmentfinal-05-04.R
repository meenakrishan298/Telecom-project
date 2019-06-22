getwd()
ftele<-read.csv("sampletelecomfinal.csv", header = T)
str(ftele)
dim(ftele)#dimension of dataset
View(head(ftele))
library(dplyr)
library(dataQualityR)
library(sqldf)

####Data quality report generation
checkDataQuality(ftele,out.file.num = "Numeric.csv",out.file.cat = "Category.csv")

###Data preparation-Continous variable
#retdays
#Missing values for this variable can be assumed to mean there have been no retention calls made by the customer.
summary(ftele$retdays)
#Replacing NA with value as 0 as it is defined 
ftele$retdays[is.na(ftele$retdays)]<-0
unique(ftele$retdays)#verified that the NA values are imputed with values 0


#Dropping categorical variables which has more proportion of NA values
summary(ftele$dwlltype)
mean(is.na(ftele$dwlltype))
#as the missing values are 30% dropping the column "dwlltype" 
ftele<-select(ftele,-dwlltype)

#Dropping the variable "dwllsize" as it proportion is 37% and only ftelels about the size of living
summary(ftele$dwllsize)
mean(is.na(ftele$dwllsize))
ftele<-select(ftele,-dwllsize)

#mailordr
summary(ftele$mailordr)
mean(is.na(ftele$mailordr))#NA value proportion is 63%
ftele<-select(ftele,-mailordr)

#occu1
summary(ftele$occu1)
mean(is.na(ftele$occu1))#NA value proportion is 73%
ftele<-select(ftele,-occu1)

#wrkwoman
#The absence of a positive indicator in this field does NOT indicate there is not a working woman in the household, only that the information was not available.
summary(ftele$wrkwoman)
mean(is.na(ftele$wrkwoman))#87% of data is NA
ftele<-select(ftele,-wrkwoman)

#solflag
#This flag is a service offered for people to opt-out of phone solicitation.  An indicator of Yes only means the household has not opted-out.  Missing indicates an unknown preference (default). 
summary(ftele$solflag)
mean(is.na(ftele$solflag))#98% of data is NA
ftele<-select(ftele,-solflag)

#Protype
summary(ftele$proptype)
mean(is.na(ftele$proptype))#It is 71% missing values,so removing it
ftele<-select(ftele,-proptype)

#Cartype
summary(ftele$cartype)
mean(is.na(ftele$cartype))#It is 67% missing values
ftele<-select(ftele,-cartype)

#mailresp
summary(ftele$mailresp)
mean(is.na(ftele$mailresp))
ftele<-select(ftele,-mailresp)

#Children 
summary(ftele$children)
mean(is.na(ftele$children))#It has 65% missing values
ftele<-select(ftele,-children)

#div_type
summary(ftele$div_type)
mean(is.na(ftele$div_type))#It has 81% missing values
ftele<-select(ftele,-div_type)

#car_buy
summary(ftele$car_buy)
ftele<-select(ftele,-car_buy)#as the data has more number of UNKNOWN values

#csa
summary(ftele$csa)
length(unique(ftele$csa))#as the number of unique values are more creating dummy variables are not possible so removing it
ftele<-select(ftele,-csa)


#Imputing NA values in Conitnues Variables
#mou_Mean
dat1<-ftele%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat1$N<-unclass(ftele%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat1$churn_perc<-dat1$n/dat1$N
dat1$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat1$LessThan<-unclass(ftele%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat1$varname<-rep("mou_Mean",nrow(dat1))
View(dat1)
summary(ftele$mou_Mean)
ftele$mou_Mean[is.na(ftele$mou_Mean)]<-mean(ftele$mou_Mean,na.rm=TRUE)
summary(ftele$mou_Mean)

###totmrc_mean####
dat2<-ftele%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat2$N<-unclass(ftele%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat2$churn_perc<-dat2$n/dat2$N
dat2$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat2$LessThan<-unclass(ftele%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat2$varname<-rep("totmrc_Mean",nrow(dat2))
View(dat2)
summary(ftele$totmrc_Mean)
ftele$totmrc_Mean[is.na(ftele$totmrc_Mean)]<-mean(ftele$totmrc_Mean,na.rm=TRUE)
summary(ftele$totmrc_Mean)

########rev_Range####
dat3<-ftele%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat3$N<-unclass(ftele%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat3$churn_perc<-dat3$n/dat3$N
dat3$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat3$LessThan<-unclass(ftele%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
nrow(dat3)
dat3$varname<-rep("rev_Range",nrow(dat3))
View(dat3)
summary(ftele$rev_Range)
ftele$rev_Range[is.na(ftele$rev_Range)]<-mean(ftele$rev_Range,na.rm=TRUE)
summary(ftele$rev_Range)

#######mou_Range#############
dat4<-ftele%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat4$N<-unclass(ftele%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat4$churn_perc<-dat4$n/dat4$N
dat4$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat4$LessThan<-unclass(ftele%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
nrow(dat4)
dat4$varname<-rep("mou_Range",nrow(dat4))
View(dat4)
summary(ftele$mou_Range)
ftele$mou_Range[is.na(ftele$mou_Range)]<-mean(ftele$mou_Range,na.rm=TRUE)
summary(ftele$mou_Range)

##########change_mou#########
dat5<-ftele%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat5$N<-unclass(ftele%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat5$churn_perc<-dat5$n/dat5$N
dat5$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat5$LessThan<-unclass(ftele%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
nrow(dat5)
dat5$varname<-rep("change_mou",nrow(dat5))
View(dat5)
summary(ftele$change_mou)
ftele$change_mou[is.na(ftele$change_mou)]<-mean(ftele$change_mou,na.rm=TRUE)
summary(ftele$change_mou)

###################ovrrev_mean#############3
dat6<-ftele%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat6$N<-unclass(ftele%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat6$churn_perc<-dat6$n/dat6$N
dat6$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat6$LessThan<-unclass(ftele%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
nrow(dat6)
dat6$varname<-rep("ovrrev_Mean",nrow(dat6))
View(dat6)
summary(ftele$ovrrev_Mean)
ftele$ovrrev_Mean[is.na(ftele$ovrrev_Mean)]<-0
summary(ftele$ovrrev_Mean)

#######################rev_Mean#############
dat7<-ftele%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat7$N<-unclass(ftele%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat7$churn_perc<-dat7$n/dat7$N
dat7$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat7$LessThan<-unclass(ftele%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
nrow(dat7)
dat7$varname<-rep("rev_Mean",nrow(dat7))
View(dat7)
summary(ftele$rev_Mean)
ftele$rev_Mean[is.na(ftele$rev_Mean)]<-mean(ftele$rev_Mean,na.rm=TRUE)
summary(ftele$rev_Mean)

#####################overmou_mean#########
dat8<-ftele%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat8$N<-unclass(ftele%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat8$churn_perc<-dat8$n/dat8$N
dat8$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat8$LessThan<-unclass(ftele%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
nrow(dat8)
dat8$varname<-rep("ovrmou_Mean",nrow(dat8))
View(dat8)
summary(ftele$ovrmou_Mean)
ftele$ovrmou_Mean[is.na(ftele$ovrmou_Mean)]<-mean(ftele$ovrmou_Mean,na.rm=TRUE)
summary(ftele$ovrmou_Mean)

####################avg6mou############
dat9<-ftele%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat9$N<-unclass(ftele%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat9$churn_perc<-dat9$n/dat9$N
dat9$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat9$LessThan<-unclass(ftele%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
nrow(dat9)
dat9$varname<-rep("avg6mou",nrow(dat9))
View(dat9)
summary(ftele$avg6mou)
ftele$avg6mou[is.na(ftele$avg6mou)]<-mean(ftele$avg6mou,na.rm=TRUE)
summary(ftele$avg6mou)

#############avg6qty###################################
dat10<-ftele%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat10$N<-unclass(ftele%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat10$churn_perc<-dat10$n/dat10$N
dat10$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat10$LessThan<-unclass(ftele%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
nrow(dat10)
dat10$varname<-rep("avg6qty",nrow(dat10))
View(dat10)
summary(ftele$avg6qty)
ftele$avg6qty[is.na(ftele$avg6qty)]<-mean(ftele$avg6qty,na.rm=TRUE)
summary(ftele$avg6qty)

############################################roam_Mean#########
dat11<-ftele%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat11$N<-unclass(ftele%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat11$churn_perc<-dat11$n/dat11$N
dat11$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(roam_Mean,n=10))%>%group_by(dec)%>%summarise(min(roam_Mean)))[[2]]
dat11$LessThan<-unclass(ftele%>%mutate(dec=ntile(roam_Mean,n=10))%>%group_by(dec)%>%summarise(max(roam_Mean)))[[2]]
nrow(dat11)
dat11$varname<-rep("roam_Mean",nrow(dat11))
View(dat11)
summary(ftele$roam_Mean)
ftele$roam_Mean[is.na(ftele$roam_Mean)]<-mean(ftele$roam_Mean,na.rm=TRUE)
summary(ftele$roam_Mean)

###############da_Mean############
dat12<-ftele%>%mutate(dec=ntile(da_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat12$N<-unclass(ftele%>%mutate(dec=ntile(da_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat12$churn_perc<-dat12$n/dat12$N
dat12$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(da_Mean,n=10))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat12$LessThan<-unclass(ftele%>%mutate(dec=ntile(da_Mean,n=10))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
nrow(dat12)
dat12$varname<-rep("da_Mean",nrow(dat12))
View(dat12)
summary(ftele$da_Mean)
ftele$da_Mean[is.na(ftele$da_Mean)]<-mean(ftele$da_Mean,na.rm=TRUE)
summary(ftele$da_Mean)

##############da_Range#########3
dat13<-ftele%>%mutate(dec=ntile(da_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat13$N<-unclass(ftele%>%mutate(dec=ntile(da_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat13$churn_perc<-dat13$n/dat13$N
dat13$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(da_Range,n=10))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat13$LessThan<-unclass(ftele%>%mutate(dec=ntile(da_Range,n=10))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
nrow(dat13)
dat13$varname<-rep("da_Range",nrow(dat13))
View(dat13)
summary(ftele$da_Range)
ftele$da_Range[is.na(ftele$da_Range)]<-mean(ftele$da_Range,na.rm=TRUE)
summary(ftele$da_Range)

###################datovr_Mean######3
dat14<-ftele%>%mutate(dec=ntile(datovr_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat14$N<-unclass(ftele%>%mutate(dec=ntile(datovr_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat14$churn_perc<-dat14$n/dat14$N
dat14$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(datovr_Mean,n=10))%>%group_by(dec)%>%summarise(min(datovr_Mean)))[[2]]
dat14$LessThan<-unclass(ftele%>%mutate(dec=ntile(datovr_Mean,n=10))%>%group_by(dec)%>%summarise(max(datovr_Mean)))[[2]]
nrow(dat14)
dat14$varname<-rep("datovr_Mean",nrow(dat14))
View(dat14)
summary(ftele$datovr_Mean)
ftele$datovr_Mean[is.na(ftele$datovr_Mean)]<-mean(ftele$datovr_Mean,na.rm=TRUE)
summary(ftele$datovr_Mean)

#########################datovr_Range#########
dat15<-ftele%>%mutate(dec=ntile(datovr_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat15$N<-unclass(ftele%>%mutate(dec=ntile(datovr_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat15$churn_perc<-dat15$n/dat15$N
dat15$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(datovr_Range,n=10))%>%group_by(dec)%>%summarise(min(datovr_Range)))[[2]]
dat15$LessThan<-unclass(ftele%>%mutate(dec=ntile(datovr_Range,n=10))%>%group_by(dec)%>%summarise(max(datovr_Range)))[[2]]
nrow(dat15)
dat15$varname<-rep("datovr_Range",nrow(dat15))
View(dat15)
summary(ftele$datovr_Range)
ftele$datovr_Range[is.na(ftele$datovr_Range)]<-mean(ftele$datovr_Range,na.rm=TRUE)
summary(ftele$datovr_Range)

###########age1#########
dat16<-ftele%>%mutate(dec=ntile(age1,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat16$N<-unclass(ftele%>%mutate(dec=ntile(age1,n=10))%>%count(dec)%>%unname())[[2]]
dat16$churn_perc<-dat16$n/dat16$N
dat16$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(age1,n=10))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
dat16$LessThan<-unclass(ftele%>%mutate(dec=ntile(age1,n=10))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
nrow(dat16)
dat16$varname<-rep("age1",nrow(dat16))
View(dat16)
ftele$age1[is.na(ftele$age1)]=38 #as the value of 38 matches wiht the NA values
summary(ftele$age1)

####age2
unique(ftele$age2)
dat17<-ftele%>%mutate(dec=ntile(age2,n=10))%>%count(churn,dec)%>%filter(churn==1)
dat17$N<-unclass(ftele%>%mutate(dec=ntile(age2,n=10))%>%count(dec)%>%unname())[[2]]
dat17$churn_perc<-dat17$n/dat17$N
dat17$GreaterThan<-unclass(ftele%>%mutate(dec=ntile(age2,n=10))%>%group_by(dec)%>%summarise(min(age2)))[[2]]
dat17$LessThan<-unclass(ftele%>%mutate(dec=ntile(age2,n=10))%>%group_by(dec)%>%summarise(max(age2)))[[2]]
nrow(dat17)
dat17$varname<-rep("age2",nrow(dat17))
View(dat17)
ftele$age2[is.na(ftele$age2)]=70#as the value 70 matches with the NA percentage
summary(ftele$age2)

############hnd_price##########
ftele%>%count(churn,levels=hnd_price)%>%filter(churn==1)->c1
c1$N<-unclass(ftele%>%filter(hnd_price%in%c1$levels)%>%count(hnd_price))[[2]]
c1$ChurnPerc<-c1$n/c1$N 
c1$Var.Name<-rep("hnd_price",nrow(c1))
View(c1)
summary(ftele$hnd_price)
ftele$hnd_price[is.na(ftele$hnd_price)]<-mean(ftele$hnd_price,na.rm=TRUE)
summary(ftele$hnd_price)
unique(ftele$hnd_price)

#######################forgntvl#########
ftele%>%count(churn,levels=forgntvl)%>%filter(churn==1)->C2
C2$N<-unclass(ftele%>%filter(forgntvl%in%C2$levels)%>%count(forgntvl))[[2]]
C2$ChurnPerc<-C2$n/C2$N 
C2$Var.Name<-rep("forgntvl",nrow(C2))
View(C2)
sqldf("select forgntvl,count(*) from ftele group by forgntvl")
ftele$forgntvl[is.na(ftele$forgntvl)]=1# imputng value as 1 as it is not showing bias in it as count of 0 is more dominant
unique(ftele$forgntvl)

####################mtrcycle############3
ftele%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->c3
c3$N<-unclass(ftele%>%filter(mtrcycle%in%c3$levels)%>%count(mtrcycle))[[2]]
c3$ChurnPerc<-c3$n/c3$N 
c3$Var.Name<-rep("mtrcycle",nrow(c3))
View(c3)
sqldf("select mtrcycle,count(*) from ftele group by mtrcycle")
ftele$mtrcycle[is.na(ftele$mtrcycle)]=1# imputng value as 1 as it is not showing bias in it as count of 0 is more dominant
unique(ftele$mtrcycle)

################################numbcars######
#Deleting the column which has more proportion of missing values
#Deleting the column numbcars
summary(ftele$numbcars)
mean(is.na(ftele$numbcars))
#as the proportion is 49% percentage removing the column "numbcars"
ftele<-select(ftele,-numbcars)

##################truck#######
ftele%>%count(churn,levels=truck)%>%filter(churn==1)->c4
c4$N<-unclass(ftele%>%filter(truck%in%c4$levels)%>%count(truck))[[2]]
c4$ChurnPerc<-c4$n/c4$N 
c4$Var.Name<-rep("truck",nrow(c4))
View(c4)
sqldf("select truck,count(*) from ftele group by truck")
ftele$truck[is.na(ftele$truck)]=1# imputng value as 1 as it is not showing bias in it as count of 0 is more dominant
unique(ftele$truck)

#############income#################3
ftele%>%count(churn,levels=income)%>%filter(churn==1)->c5
c5$N<-unclass(ftele%>%filter(income%in%c5$levels)%>%count(income))[[2]]
c5$ChurnPerc<-c5$n/c5$N 
c5$Var.Name<-rep("income",nrow(c5))
View(c5)
sqldf("select income,count(*) from ftele group by income")
ftele$income[is.na(ftele$income)]=10 #imputng value as 10 as the NA are more in the entire dataset
unique(ftele$income)

##############prizm_social_one##########
ftele%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->c6
c6$N<-unclass(ftele%>%filter(prizm_social_one%in%c6$levels)%>%count(prizm_social_one))[[2]]
c6$ChurnPerc<-c6$n/c6$N 
c6$Var.Name<-rep("prizm_social_one",nrow(c6))
View(c6)
sqldf("select prizm_social_one,count(*) from ftele group by prizm_social_one")
ftele$prizm_social_one=as.character(ftele$prizm_social_one)
ftele$prizm_social_one[is.na(ftele$prizm_social_one)]<-"missing"

############area###############################
ftele%>%count(churn,levels=area)%>%filter(churn==1)->c7
c7$N<-unclass(ftele%>%filter(area%in%c7$levels)%>%count(area))[[2]]
c7$ChurnPerc<-c7$n/c7$N 
c7$Var.Name<-rep("area",nrow(c7))
View(c7)
ftele$area=as.character(ftele$area)
ftele$area[is.na(ftele$area)]<-"missing"

#########################hnd_webcap###########
ftele%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->c8
c8$N<-unclass(ftele%>%filter(hnd_webcap%in%c8$levels)%>%count(hnd_webcap))[[2]]
c8$ChurnPerc<-c8$n/c8$N 
c8$Var.Name<-rep("hnd_webcap",nrow(c8))
View(c8)
ftele$hnd_webcap=as.character(ftele$hnd_webcap)
ftele$hnd_webcap[is.na(ftele$hnd_webcap)]<-"Not Applicable"

#######################marital###########
ftele%>%count(churn,levels=marital)%>%filter(churn==1)->c9
c9$N<-unclass(ftele%>%filter(marital%in%c9$levels)%>%count(marital))[[2]]
c9$ChurnPerc<-c9$n/c9$N 
c9$Var.Name<-rep("marital",nrow(c9))
View(c9)
ftele$marital=as.character(ftele$marital)
ftele$marital[is.na(ftele$marital)]<-"missing"
unique(ftele$marital)

######################ethnic#############
ftele%>%count(churn,levels=ethnic)%>%filter(churn==1)->c10
c10$N<-unclass(ftele%>%filter(ethnic%in%c10$levels)%>%count(ethnic))[[2]]
c10$ChurnPerc<-c10$n/c10$N 
c10$Var.Name<-rep("ethnic",nrow(c10))
View(c10)
ftele$ethnic=as.character(ftele$ethnic)
ftele$ethnic[is.na(ftele$ethnic)]<-"U"
####################################################################
###creation of derived variables#############

ftele$cp<-ftele$comp_vce_Mean/ftele$plcd_vce_Mean
View(ftele$cp)
ftele$comp_pc=ifelse(ftele$comp_vce_Mean>0,ftele$cp,0)
summary(ftele$comp_pc)
ftele<-select(ftele,-cp)
class(ftele$comp_pc)

ftele$Non_optimal<-ftele$ovrrev_Mean/ftele$totrev
ftele$Non_optimal_Rateplan=ifelse(ftele$ovrrev_Mean>0,ftele$Non_optimal,0)
View(head(ftele))
ftele<-select(ftele,-Non_optimal)
################################################
############creation of dummy variables########
####prizm_social_one
unique(ftele$prizm_social_one)
ftele$city<-ifelse(ftele$prizm_social_one=="C",1,0)
ftele$rural<-ifelse(ftele$prizm_social_one=="R",1,0)
ftele$suburban<-ifelse(ftele$prizm_social_one=="S",1,0)
ftele$town<-ifelse(ftele$prizm_social_one=="T",1,0)
ftele$urban<-ifelse(ftele$prizm_social_one=="U",1,0)
ftele$city<-as.factor(ftele$city)
ftele$rural<-as.factor(ftele$rural)
ftele$suburban<-as.factor(ftele$suburban)
ftele$town<-as.factor(ftele$town)
ftele$urban<-as.factor(ftele$urban)
ftele<-select(ftele,-prizm_social_one)
#####################################################33
#######age1###########
unique(ftele$age1)
ftele$age1_1<-ifelse(ftele$age1==0,"Default",ifelse(ftele$age1<=30,"Young",
                                                    ifelse(ftele$age1>30 & ftele$age1<=55,"Mid Age","Old")))

ftele$age1_Mid_Age<-ifelse(ftele$age1_1 == "Mid Age", 1, 0)
ftele$age1_Old<-ifelse(ftele$age1_1 == "Old", 1, 0)
ftele$age1_Young<-ifelse(ftele$age1_1 == "Young", 1, 0)
ftele$age1_Mid_Age<-as.factor(ftele$age1_Mid_Age)
ftele$age1_Young<-as.factor(ftele$age1_Young)
ftele$age1_Old<-as.factor(ftele$age1_Old)
ftele<-select(ftele,-age1_1)
ftele<-select(ftele,-age1)

####################age2##################
unique(ftele$age2)
ftele$age2_1<-ifelse(ftele$age2==0,"Default",ifelse(ftele$age2<=30,"Young",
                                                    ifelse(ftele$age2>30 & ftele$age2<=55,"Mid Age","Old")))
ftele$age2_Mid_Age<-ifelse(ftele$age2_1 == "Mid Age", 1, 0)
ftele$age2_Old<-ifelse(ftele$age2_1 == "Old", 1, 0)
ftele$age2_Young<-ifelse(ftele$age2_1 == "Young", 1, 0)
ftele$age2_Mid_Age<-as.factor(ftele$age2_Mid_Age)
ftele$age2_Old<-as.factor(ftele$age2_Old)
ftele$age2_Young<-as.factor(ftele$age2_Young)
ftele<-select(ftele,-age2_1)
ftele<-select(ftele,-age2)
########################################
#uniqusubs
unique(ftele$uniqsubs)
ftele$uni2<-ifelse(ftele$uniqsubs=="2",1,0)
ftele$uni3<-ifelse(ftele$uniqsubs=="3",1,0)
ftele$uni4<-ifelse(ftele$uniqsubs=="4",1,0)
ftele$uni5<-ifelse(ftele$uniqsubs=="5",1,0)
ftele$uni2<-as.factor(ftele$uni2)
ftele$uni3<-as.factor(ftele$uni3)
ftele$uni4<-as.factor(ftele$uni4)
ftele$uni5<-as.factor(ftele$uni5)
ftele<-select(ftele,-uniqsubs)
###############################################3
#####################################crclscod####################
unique(ftele$crclscod)
class(ftele$crclscod)
ftele$crclscod1<-ifelse(ftele$crclscod=="E4",1,0)
ftele$crclscod2<-ifelse(ftele$crclscod=="EA",1,0)
ftele$crclscod3<-ifelse(ftele$crclscod=="Z",1,0)
ftele$crclsrest<-ifelse(ftele$crclscod %in% c("CA",	"ZA",	"BA",	"AA",		"B",	"Z",	"A",	"DA",	
                                              "C2",	"U",	"V1",	"A2",	"E",	"I",	"Z4",	"G",	
                                              "C",	"J",	"JF",	"D",	"M",	"CY",	"W",	"D5",	"D4",	"EC",	
                                              "CC",	"GA",	"EM",	"K",	"ZY",	"B2",	"U1",	"A3",	"E2",	"H",
                                              "Y",	"O",	"C5",	"GY",	"Z5",	"IF",	"EF",	"P1"),"others",0)
ftele$crclscod1<-as.factor(ftele$crclscod1)
ftele$crclscod2<-as.factor(ftele$crclscod2)
ftele$crclscod3<-as.factor(ftele$crclscod3)
ftele$crclsrest<-as.factor(ftele$crclsrest)
head(ftele)
ftele<-select(ftele,-crclscod)
#############################
#ethnic
unique(ftele$ethnic)
ftele$ethnic_n<-ifelse(ftele$ethnic=="N",1,0)
ftele$ethnic_H<-ifelse(ftele$ethnic=="H",1,0)
ftele$ethnic_s<-ifelse(ftele$ethnic=="S",1,0)
ftele$ethnic_U<-ifelse(ftele$ethnic=="U",1,0)
ftele$ethnic_O<-ifelse(ftele$ethnic=="O",1,0)
ftele$ethnic_rest<-ifelse(ftele$ethnic %in% c("B","Z","I","J","F","missing","R","X","M","P","D","C"),"ethnicunknwn",0)
ftele<-select(ftele,-ethnic)
###########################333
######area
unique(ftele$area)
ftele$nwyrkciry<-ifelse(ftele$area=="NEW YORK CITY AREA",1,0)
ftele$marylndvir<-ifelse(ftele$area=="DC/MARYLAND/VIRGINIA AREA",1,0)
ftele$losanges<-ifelse(ftele$area=="LOS ANGELES AREA",1,0)
ftele$northwestrock<-ifelse(ftele$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
ftele$atlansouth<-ifelse(ftele$area=="ATLANTIC SOUTH AREA",1,0)
ftele$tennese<-ifelse(ftele$area=="TENNESSEE AREA",1,0)
ftele<-select(ftele,-area)  
#####################
#hnd_webcap
unique(ftele$hnd_webcap)
ftele$hndwebcap1<-ifelse(ftele$hnd_webcap=="WC",1,0)
ftele$handwebcap2<-ifelse(ftele$hnd_webcap=="WCMB",1,0)
ftele$handwebcap3<-ifelse(ftele$hnd_webcap=="UNKW",1,0)
ftele<-select(ftele,-hnd_webcap)
################################
#refurb_new
summary(ftele$refurb_new)
ftele$refrubnew<-ifelse(ftele$refurb_new=="R",1,0)
ftele<-select(ftele,-refurb_new)
###################
########
unique(ftele$actvsubs)
ftele$actvsubs1<-ifelse(ftele$actvsubs==1,1,0)
ftele$actvsubs2<-ifelse(ftele$actvsubs==2,1,0)
ftele$actvsubs3<-ifelse(ftele$actvsubs==3,1,0)
ftele$actvsubs4<-ifelse(ftele$actvsubs==4,1,0)
ftele$actvsubs5<-ifelse(ftele$actvsubs==5,1,0)
ftele$actvsubs6<-ifelse(ftele$actvsubs==6,1,0)
ftele<-select(ftele,-actvsubs)

######################################################################
#########Logistic model creation###########################
#splitting the data into testing and training data models

set.seed(200)
samplingftele<-sort(sample(nrow(ftele),nrow(ftele)*.7))
length(samplingftele)
trainftele<-ftele[samplingftele,]
testftele<-ftele[-samplingftele,]
View(trainftele)

table(trainftele$churn)
table(trainftele$churn)/nrow(trainftele)
table(testftele$churn)/nrow(testftele)

#Check for multicollinearity
library(corrgram)
cormat<-corrgram(trainftele)
write.csv(cormat,"correlationtele.csv")
colSums(is.na(trainftele))
colSums(is.na(testftele))
colnames(trainftele)

#####full run#################################
full<-glm(data=trainftele,churn~.,family="binomial")
summary(full)

#####step1 function##########################
step1<-step(full,direction="backward")

###########iterations########################
View(head(trainftele))
step2a<-glm(data=trainftele,churn ~ totmrc_Mean + change_mou + owylis_vce_Range + months + 
              eqpdays + rev_Mean + ovrmou_Mean + avg3mou + avgmou + avg3qty + 
              asl_flag + models + hnd_price + opk_dat_Mean + retdays + 
              blck_dat_Mean + mou_pead_Mean + datovr_Mean + datovr_Range + 
              totrev + adjrev + avgrev + Customer_ID + comp_pc + Non_optimal_Rateplan + 
              city + rural + town + age1_Mid_Age + age1_Old + uni2 + uni3 + 
              uni4 + uni5 + crclscod1 + crclscod2 + crclscod3 + crclsrest + 
              ethnic_n + ethnic_O + ethnic_rest + nwyrkciry + northwestrock + 
              tennese + hndwebcap1 + refrubnew + actvsubs3,family=binomial)
summary(step2a)

step3<-glm(data=trainftele,churn ~ totmrc_Mean + change_mou + owylis_vce_Range + months + 
             eqpdays +  avg3qty + asl_flag +Non_optimal_Rateplan+rev_Mean+avg3mou+avgmou+
             models + hnd_price + retdays + roam_Mean + hndwebcap1+
             datovr_Mean + datovr_Range +totrev + adjrev + avgrev +  comp_pc 
           +  rural +  age1_Mid_Age + age1_Old + uni2 +  
             ethnic_n + ethnic_O + ethnic_rest + northwestrock,family=binomial)
summary(step3)

############calculating vif##############
vif(step4)
###removing variables after finding the vif
step4<-glm(data=trainftele,churn ~ totmrc_Mean + change_mou + owylis_vce_Range +  
             eqpdays +   avg3qty + asl_flag +months+models+
             hnd_price + retdays + roam_Mean + hndwebcap1+Non_optimal_Rateplan+
             avgrev +  comp_pc+  rural +  age1_Mid_Age + age1_Old + uni2 +  
              ethnic_O +  northwestrock,family=binomial)
summary(step4)

library(car)
vif(step4)

#Finding Predicted Values
trainftele$predicted<-step4$fitted.values

#Checking the accuracy using confusion matrix
trainftele$predclass<-ifelse(trainftele$predicted>0.5,1,0)
table(trainftele$predclass,trainftele$churn)
(7055+36)/(7055+36+2160+30)
###Accuracy is 76.39%


#Kappa values
library(caret)
library(irr)
kappa2(data.frame(trainftele$churn,trainftele$predclass))
##kappa value is 0.0183

#ROCR
library(ROCR)
predftele<-prediction(trainftele$predicted,trainftele$churn)
View(predftele)

perftele <- performance(predftele,"acc")
View(perftele)

class(perftele@x.values)
cutoffprobftele <- as.numeric(unlist(perftele@x.values))
cutoffprobftele
class(perftele@y.values)
accuraciesftele <- as.numeric(unlist(perftele@y.values))
cutoffsftele <- data.frame(cutoffprobftele, accuraciesftele)
cutoffsftele <- cutoffsftele[order(cutoffsftele$accuraciesftele, decreasing=TRUE),]
View(cutoffsftele)

trainftele$predclass<-ifelse(trainftele$predicted>0.5097903,1,0)
table(trainftele$predclass,trainftele$churn)
(7063+33)/(7063+33+22+2163)
#76.45% accuracy predicted


#Using e1071 & plotting the graphs ROCR & ABLINE
table(trainftele$predclass,trainftele$churn)
library(e1071)
confusionMatrix(as.factor(trainftele$churn),as.factor(trainftele$predclass), positive = "1")
perf<-performance(predftele,"tpr","fpr")#tpr=TP/P and fpr=FP/N
plot(perf,col="red")
abline(0,1, lty = 8, col = "blue")

auctr<-performance(predftele,"auc")
auctr
#auc 62.41%

library(gains)
gains(as.numeric(trainftele$churn),trainftele$predicted, groups =10)
quantile(trainftele$predicted, seq(0,1,0.1))


#plotting the model to check the accuracy in testdata
testftele$pred <- predict(step4, type = "response",newdata = testftele)

#Checking for accuracy using confusion matrix
testftele$predclass<-ifelse(testftele$pred>0.5,1,0)
table(testftele$predclass,testftele$churn)
(3036+10)/(3036+19+913+19)
#76.39% accuracy

library(ROCR)
predt<-prediction(testftele$pred,testftele$churn)
class(predt)
preft <- performance(predt,"acc")
class(preft)

class(preft@x.values)
cutoffprobt <- as.numeric(unlist(preft@x.values))
cutoffprobt
class(preft@y.values)
accuraciest <- as.numeric(unlist(preft@y.values))

cutoffst <- data.frame(cutoffprobt, accuraciest)
cutoffst <- cutoffst[order(cutoffst$accuraciest, decreasing=TRUE),]
View(cutoffs1)
View(head(testftele))

testftele$predclass2<- ifelse(testftele$pred>0.5884859,1,0)
kappa2(data.frame(testftele$churn,testftele$predclass2))

#Using e1071 & plotting the graphs ROCR & ABLINE
library(e1071)
confusionMatrix(as.factor(testftele$churn),as.factor(testftele$predclass2), positive = "1")
#Kappa : 0.0035#accuarcy 76.77%
prefnew<-performance(predt,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(prefnew,col="red")

abline(0,1, lty = 8, col = "blue")
auct<-performance(predt,"auc")
auct
##auc is 59.24%

gains(as.numeric(testftele$churn),testftele$pred, groups =10)
quantile(testftele$pred, seq(0,1,0.1))


##################Answering questions##########
#What are the top five factors driving likelihood of churn at Mobicom?
summary(step4)
#head(sort(abs(step4$coefficients),decreasing = T),10)
summary(step4)

#Question4
#What would be your recommendation on how to use this churn model for prioritisation of
#customers for a proactive retention campaigns in the future?
library(gains)
gains(testftele$churn,predict(step4,type="response",newdata=testftele),groups = 10)
quantile(testftele$pred, seq(0,1,0.1))
#it shows that 0.20644685/40% percentage of chances are there for 28% of customers to churn

# Selecting Customers with high churn rate
testftele$prob<-predict(step4,type="response",newdata=testftele)
quantile(testftele$prob,prob=seq(0.10,1,0.1))
#View(testftele)

# Applying cutoff value to predict customers who Will Churn
pred4<-predict(step4, type="response", newdata=testftele)
View(pred4)
pred4<-ifelse(pred4>=0.27 , 1, 0)
table(pred4,testftele$churn)

#Top 30% propbabilties lie between 0.2701852 and 0.7992615
Targeted<-testftele[testftele$prob>0.27 & testftele$churn=="1","Customer_ID"]
Targeted<-as.data.frame(Targeted)
nrow(Targeted)
###totally 361 customers are targeted in this range
#answer for q4
write.csv(Targeted,"question4.csv")

#Question5
##Find the top revenue generating customers and how to retain them
# Selecting Customers who generate high revenues
quantile(testftele$totrev,prob=seq(0.10,1,0.1))
revenuelev<-ifelse(testftele$totrev<670.076,"lowrevenue",ifelse(testftele$totrev>=670.076 & 
                                                                  testftele$totrev<1124.269,"mediumrevenue","highrevenue"))
View(revenuelev)
hvcus<-table(revenuelev,testftele$churn)
hvcus<-as.data.frame(hvcus)
View(hvcus)
testftele$revlevels<-revenuelev

###to find the customers with high churn levels
quest5<-predict(step4,type="response",newdata=testftele)
View(quest5)
####to find the churning percentage in  the total population
quantile(testftele$prob,prob=seq(0.10,1,0.1))
pred6<-ifelse(quest5<0.2074363,"lowprob",ifelse(quest5>=0.2074363 & quest5<0.2998735,"medprob","highprob"))
View(pred6)
table(pred6,testftele$churn)
testftele$churnlevels<-pred6

###to find the customers who have high revenue and high churns levels of probability
ques5a<-sqldf("select Customer_ID from testftele  where revlevels=='highrevenue' and churnlevels=='highprob'") 
ques5a
ques5a<-as.data.frame(ques5a)
View(ques5a)##has the list of customers who r to be targeted
###totally 293 customers are found
#creation of table for customers with revenue levels and churn levels
table(pred6,testftele$revlevels)
#answer for the question5
write.csv(ques5a,"question5.csv")









