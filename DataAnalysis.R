library(jsonlite)
library(dplyr)
library(stringr)
library(readr)
x104 <- read_csv("C:/Users/User/Downloads/a.csv")
X107 <- read_csv("C:/Users/User/Downloads/107-career.csv")


x104$大職業別<-gsub("部門|、","",x104$大職業別)
X107$大職業別<-gsub("_","",X107$大職業別)
X107$大職業別<-gsub("建工程","造業",X107$大職業別)
X107$大職業別<-gsub("出版、影音製作、傳播及資通訊服務業","資訊及通訊傳播業",X107$大職業別)
X107$大職業別<-gsub("教育業","教育服務業",X107$大職業別)
X107$大職業別<-gsub("醫療保健業","醫療保健服務業",X107$大職業別)

x104$`大學-薪資`<-as.numeric(gsub("—|…",NA,x104$`大學-薪資`))
X107$`大學-薪資`<-as.numeric(gsub("—|…",NA,X107$`大學-薪資`))



Compare<-(inner_join(x104,X107,by="大職業別"))
View(Compare)

salary<-select(X107,大職業別)
salary$salaryrate<-((X107$`大學-薪資`)/(x104$`大學-薪資`))
salary<-salary[complete.cases(salary$salaryrate),]
rate<-filter( salary , salaryrate > 1.05 )
highsalaryrate<-arrange(rate,desc(salaryrate))
View(head(highsalaryrate,10))
View(highsalaryrate)

table(sapply (strsplit (rate$大職業別,"-") , "[" ,  1))%>%
  View()

#第二題

x104$`大學-女/男`<-as.numeric(gsub("—|…",NA,x104$`大學-女/男`))
a104<-x104[complete.cases(x104$`大學-女/男`),]
a104<-select(a104,大職業別,`大學-女/男`)
a104<-a104[order(a104$`大學-女/男`,decreasing = T),]


X107$`大學-女/男`<-as.numeric(gsub("—|…",NA,X107$`大學-女/男`))
b107<-X107[complete.cases(X107$`大學-女/男`),]
b107<-select(b107,大職業別,`大學-女/男`)
b107<-b107[order(b107$`大學-女/男`,decreasing = T),]


a104$`大學-女/男`<-as.character(a104$`大學-女/男`)
b107$`大學-女/男`<-as.character(b107$`大學-女/男`)

tail(a104,10)
tail(b107,10)


head(a104,10)
head(b107,10)

#第三題
X107$`研究所-薪資`<-as.numeric(gsub("—|…",NA,X107$`研究所-薪資`))
X107$`大學-薪資`<-as.numeric(gsub("—|…",NA,X107$`大學-薪資`))
dataset <- data.frame(name = X107$大職業別)
dataset$salaryrate<-X107$`研究所-薪資`/X107$`大學-薪資`
dataset<-dataset[complete.cases(dataset$salaryrate),]
dataseta<-dataset[order(dataset$salaryrate,decreasing = T),]

head(dataseta,10)


#第四題
dataset2 <- data.frame(name = X107$大職業別,
                           研究所 =X107$`研究所-薪資`,
                           大學 = X107$`大學-薪資`)
dataset2<-dataset2[complete.cases(dataset2$研究所&dataset2$大學),]
dataset2$rate<-dataset2$研究所/dataset2$大學
dataset2$研究所<-sort(dataset2$研究所,decreasing = T)
dataset2[grep("資訊及通訊傳播業-專業人員|資訊及通訊傳播業",dataset$name),]
head(dataset2[grep("資訊及通訊傳播業-專業人員|資訊及通訊傳播業",dataset2$name),],10)











