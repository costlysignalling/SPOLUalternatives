#Load the data as a vector
d<-read.table("words_czech_ANSI.txt") #for czech

#How many words do I have?
nrow(d)

#I will need this library
library(stringr)

#Convert the dataset to character vector
v<-as.character(d[,1])

#Version 1:
#2 shared letters with "ODS", 2 with "TOP", 3 with "KDUČSL" 
odst<-sapply(c("o","d","s"),function(x){str_count(v,x)})
ods<-rowSums(odst)==2&odst[,1]<2&odst[,2]<2&odst[,3]<2

topt<-sapply(c("t","o","p"),function(x){str_count(v,x)})
top<-rowSums(topt)==2&topt[,1]<2&topt[,2]<2&topt[,3]<2

kdut<-sapply(c("k","d","u","č","s","l"),function(x){str_count(v,x)})
kdu<-rowSums(kdut)==3&kdut[,1]<2&kdut[,2]<2&kdut[,3]<2&kdut[,4]<2&kdut[,5]<2&kdut[,6]<2

allt<-sapply(unique(c("o","d","s","t","o","p","k","d","u","č","s","l")),function(x){str_count(v,x)})
only<-rowSums(allt)==nchar(v)

#The result
res1<-v[ods&top&kdu&only]

#How many words do we have?
length(res1)

#Proportion in percent
(length(res1)/length(v))*100

#One word out of...
length(v)/length(res1)

#Result
res1

#As a single uppercase string
toupper(paste(res1,collapse=", "))


#Version 2
#2 shared letters with "ODS", 2 with "TOP", 4 with "KDUČSL" 
kdu2<-rowSums(kdut)==4&kdut[,1]<2&kdut[,2]<2&kdut[,3]<2&kdut[,4]<2&kdut[,5]<2&kdut[,6]<2
res2<-v[ods&top&kdu2&only]

length(res2)

toupper(paste(res2,collapse=", "))


#Version 3
#2 shared letters with "ODS", 2 with "TOP", 2 or 5 with "KDUČSL" 

kdu3<-rowSums(kdut)>1&rowSums(kdut)<6&kdut[,1]<2&kdut[,2]<2&kdut[,3]<2&kdut[,4]<2&kdut[,5]<2&kdut[,6]<2

res3<-v[ods&top&kdu3&only]

#Summary
res1
res2
res3[!(res3%in%res1)&!(res3%in%res2)]

#Do these counts fit the total
length(res1)+
length(res2)+
length(res3[!(res3%in%res1)&!(res3%in%res2)])

length(res3)

#List of the new options
toupper(paste(res3[!(res3%in%res1)&!(res3%in%res2)],collapse=", "))


#Mainly ODS
odsM<-rowSums(odst)==3&odst[,1]<2&odst[,2]<2&odst[,3]<2

resODS<-v[odsM&top&kdu3&only]

length(resODS)
toupper(paste(resODS,collapse=", "))


#Mainly TOP
topM<-rowSums(topt)==3&topt[,1]<2&topt[,2]<2&topt[,3]<2

resTOP<-v[ods&topM&kdu3&only]

length(resTOP)
toupper(paste(resTOP,collapse=", "))

#Mainly KDUČSL
#Consensus with KDUČSL having more than 4 characters is not possible
kduM<-rowSums(kdut)==6&kdut[,1]<2&kdut[,2]<2&kdut[,3]<2&kdut[,4]<2&kdut[,5]<2&kdut[,6]<2

resKDU<-v[ods&top&kduM&only]
resKDU
#Not possible

kduM<-rowSums(kdut)==5&kdut[,1]<2&kdut[,2]<2&kdut[,3]<2&kdut[,4]<2&kdut[,5]<2&kdut[,6]<2

resKDU<-v[ods&top&kduM&only]
resKDU
#Not possible

#ONLY letters in ODS, TOP, KDUČSL, at least two from each
odsB<-rowSums(odst)>1&odst[,1]<2&odst[,2]<2&odst[,3]<2
topB<-rowSums(topt)>1&topt[,1]<2&topt[,2]<2&topt[,3]<2
kduB<-rowSums(kdut)>1&kdut[,1]<2&kdut[,2]<2&kdut[,3]<2&kdut[,4]<2&kdut[,5]<2&kdut[,6]<2

resALL2<-v[odsB&topB&kduB&only]
tosave<-toupper(paste(sort(resALL2)))

length(tosave)

write.table(data.frame(all_at_least_2=tosave),"options1.txt",sep="\t",row.names=F)


#ONLY letters in ODS, TOP, KDUČSL, at least one from each
odsB<-rowSums(odst)>0&odst[,1]<2&odst[,2]<2&odst[,3]<2
topB<-rowSums(topt)>0&topt[,1]<2&topt[,2]<2&topt[,3]<2
kduB<-rowSums(kdut)>0&kdut[,1]<2&kdut[,2]<2&kdut[,3]<2&kdut[,4]<2&kdut[,5]<2&kdut[,6]<2

resALL<-v[odsB&topB&kduB&only]
tosave<-toupper(paste(sort(resALL)))

length(tosave)

paste(toupper(sort(resALL)[!sort(resALL)%in%resALL2]),collapse=", ")

#It fits
151+74

tosave<-toupper(paste(resALL))

write.table(data.frame(all_at_least_1=sort(tosave)),"options2.txt",sep="\t",row.names=F)


#Including duplicities
odsD<-rowSums(odst)>0
topD<-rowSums(topt)>0
kduD<-rowSums(kdut)>0

resALLD<-v[odsD&topD&kduD&only]
length(resALLD)

paste(toupper(sort(resALLD)[!sort(resALLD)%in%resALL]),collapse=", ")

tosave<-toupper(paste(sort(resALLD)))

length(tosave)

write.table(data.frame(duplicity_allowed=sort(tosave)),"options3.txt",sep="\t",row.names=F)











