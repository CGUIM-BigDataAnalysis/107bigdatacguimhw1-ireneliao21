install.packages("knitr")
library(jsonlite)
library(dplyr)
library(readr)
library(knitr)


X103_LevelSalary <- read_csv("C:/Users/Mimi Liao/Desktop/A17000000J-020066-Qod/103_LevelSalary.csv")
X104_LevelSalary <- read_csv("C:/Users/Mimi Liao/Desktop/A17000000J-020066-Qod/104_LevelSalary.csv")
X105_LevelSalary <- read_csv("C:/Users/Mimi Liao/Desktop/A17000000J-020066-Qod/105_LevelSalary.csv")
X106_LevelSalary <- read_csv("C:/Users/Mimi Liao/Desktop/A17000000J-020066-Qod/106_LevelSalary.csv")



university<-data.frame("職業別"=X103_LevelSalary$大職業別,"103"=X103_LevelSalary$`大學-薪資`,"106"=X106_LevelSalary$`大學-薪資`)
university$X103<-gsub("—",NA,university$X103)
university$X106<-gsub("—",NA,university$X106)
university$職業別<-as.character(university$職業別)
university$X103<-as.numeric(university$X103)
university$X106<-as.numeric(university$X106)
universityadd<-cbind(university,increase=university$X106/university$X103)%>%arrange(desc(`increase`))
universityadd[1:10,1]
aa<-filter(universityadd,increase>1.05)
aa[,1]
bb<-sapply(strsplit(aa$職業別,"-"),'[',1)
knitr::kable(table(bb))
-----------------------------------------------------------------------------------------------------------------------
college<-data.frame("職業別"=X103_LevelSalary$大職業別,"103"=X103_LevelSalary$`大學-女/男`,"104"=X104_LevelSalary$`大學-女/男`,"105"=X105_LevelSalary$`大學-女/男`,"106"=X106_LevelSalary$`大學-女/男`)
college$X103<-gsub("—|…",NA,college$X103)
college$X104<-gsub("—|…",NA,college$X104)
college$X105<-gsub("—|…",NA,college$X105)
college$X106<-gsub("—|…",NA,college$X106)
college$X103<-as.numeric(college$X103)
college$X104<-as.numeric(college$X104)
college$X105<-as.numeric(college$X105)
college$X106<-as.numeric(college$X106)

manhigher103<-select(college,職業別,X103)%>%filter(X103<100)%>%arrange(X103)
manhigher103[1:10,]
manhigher104<-select(college,職業別,X104)%>%filter(X104<100)%>%arrange(X104)
manhigher104[1:10,]
manhigher105<-select(college,職業別,X105)%>%filter(X105<100)%>%arrange(X105)
manhigher105[1:10,]
manhigher106<-select(college,職業別,X106)%>%filter(X106<100)%>%arrange(X106)
manhigher106[1:10,]

womanhigher103<-select(college,職業別,X103)%>%filter(X103>100)%>%arrange(desc(X103))
womanhigher103[1:10,]
womanhigher104<-select(college,職業別,X104)%>%filter(X104>100)%>%arrange(desc(X104))
womanhigher104[1:10,]
womanhigher105<-select(college,職業別,X105)%>%filter(X105>100)%>%arrange(desc(X105))
womanhigher105[1:10,]
womanhigher106<-select(college,職業別,X106)%>%filter(X106>100)%>%arrange(desc(X106))
womanhigher106[1:10,]
-----------------------------------------------------------------------------

salary106<-data.frame("職業別"=X106_LevelSalary$大職業別,universe=X106_LevelSalary$`大學-薪資`,research=X106_LevelSalary$`研究所及以上-薪資`)
salary106$universe<-gsub("—",NA,salary106$universe)
salary106$research<-gsub("—",NA,salary106$research)
salary106$universe<-as.numeric(salary106$universe)
salary106$research<-as.numeric(salary106$research)
salary_106<-cbind(salary106,increase=salary106$research/salary106$universe)%>%arrange(desc(`increase`))
salary_106[1:10,1]

-----------------------------------------------------------------------------------------------------

hobit<-filter(salary_106,職業別%in% c("藝術_娛樂及休閒服務業-技術員及助理專業人員","醫療保健服務業-技術員及助理專業人員","教育服務業-技術員及助理專業人員","支援服務業-技術員及助理專業人員","專業_科學及技術服務業-專業人員"))
hobitsalary<-cbind(hobit,dif=hobit$research-hobit$universe)
hobitsalary

