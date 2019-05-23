107-2 大數據分析方法 作業一
================
廖韋慈

搞不清楚各行各業的薪資差異嗎? 念研究所到底對第一份工作的薪資影響有多大? CP值高嗎? 透過分析**初任人員平均經常性薪資**- [開放資料連結](https://data.gov.tw/dataset/6647)，可初步了解台灣近幾年各行各業、各學歷的起薪。

比較103年度和106年度大學畢業者的薪資資料
----------------------------------------

### 資料匯入與處理

``` r
library(jsonlite)
```

    ## Warning: package 'jsonlite' was built under R version 3.5.3

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 3.5.3

``` r
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 3.5.3

``` r
X103_LevelSalary <- read_csv("C:/Users/Mimi Liao/Desktop/A17000000J-020066-Qod/103_LevelSalary.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_double(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
X104_LevelSalary <- read_csv("C:/Users/Mimi Liao/Desktop/A17000000J-020066-Qod/104_LevelSalary.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
X105_LevelSalary <- read_csv("C:/Users/Mimi Liao/Desktop/A17000000J-020066-Qod/105_LevelSalary.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
X106_LevelSalary <- read_csv("C:/Users/Mimi Liao/Desktop/A17000000J-020066-Qod/106_LevelSalary.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

### 106年度薪資較103年度薪資高的職業有哪些?

``` r
university<-data.frame("職業別"=X103_LevelSalary$大職業別,"103"=X103_LevelSalary$'大學-薪資',"106"=X106_LevelSalary$'大學-薪資')
university$X103<-gsub("—",NA,university$X103)
university$X106<-gsub("—",NA,university$X106)
university$職業別<-as.character(university$職業別)
university$X103<-as.numeric(university$X103)
university$X106<-as.numeric(university$X106)
universityadd<-cbind(university,increase=university$X106/university$X103)%>%arrange(desc(increase))
universityadd[1:10,1]
```

    ##  [1] "其他服務業-技術員及助理專業人員"          
    ##  [2] "住宿及餐飲業-服務及銷售工作人員"          
    ##  [3] "用水供應及污染整治業-技術員及助理專業人員"
    ##  [4] "專業、科學及技術服務業-專業人員"          
    ##  [5] "其他服務業-技藝、機械設備操作及組裝人員"  
    ##  [6] "營造業-服務及銷售工作人員"                
    ##  [7] "其他服務業-專業人員"                      
    ##  [8] "資訊及通訊傳播業-專業人員"                
    ##  [9] "不動產業-專業人員"                        
    ## [10] "教育服務業-事務支援人員"

### 提高超過5%的的職業有哪些?

``` r
aa<-filter(universityadd,increase>1.05)
aa[,1]
```

    ##  [1] "其他服務業-技術員及助理專業人員"                    
    ##  [2] "住宿及餐飲業-服務及銷售工作人員"                    
    ##  [3] "用水供應及污染整治業-技術員及助理專業人員"          
    ##  [4] "專業、科學及技術服務業-專業人員"                    
    ##  [5] "其他服務業-技藝、機械設備操作及組裝人員"            
    ##  [6] "營造業-服務及銷售工作人員"                          
    ##  [7] "其他服務業-專業人員"                                
    ##  [8] "資訊及通訊傳播業-專業人員"                          
    ##  [9] "不動產業-專業人員"                                  
    ## [10] "教育服務業-事務支援人員"                            
    ## [11] "住宿及餐飲業-技術員及助理專業人員"                  
    ## [12] "專業、科學及技術服務業-技藝、機械設備操作及組裝人員"
    ## [13] "運輸及倉儲業-技藝、機械設備操作及組裝人員"          
    ## [14] "其他服務業-事務支援人員"                            
    ## [15] "教育服務業-服務及銷售工作人員"                      
    ## [16] "用水供應及污染整治業"                               
    ## [17] "用水供應及污染整治業-專業人員"                      
    ## [18] "資訊及通訊傳播業"                                   
    ## [19] "支援服務業-服務及銷售工作人員"                      
    ## [20] "藝術、娛樂及休閒服務業-技藝、機械設備操作及組裝人員"
    ## [21] "資訊及通訊傳播業-事務支援人員"                      
    ## [22] "教育服務業"                                         
    ## [23] "營造業-專業人員"                                    
    ## [24] "專業、科學及技術服務業"                             
    ## [25] "支援服務業-技藝、機械設備操作及組裝人員"            
    ## [26] "住宿及餐飲業"                                       
    ## [27] "住宿及餐飲業-技藝、機械設備操作及組裝人員"          
    ## [28] "電力及燃氣供應業-服務及銷售工作人員"                
    ## [29] "運輸及倉儲業-技術員及助理專業人員"                  
    ## [30] "運輸及倉儲業-事務支援人員"                          
    ## [31] "醫療保健服務業-技術員及助理專業人員"                
    ## [32] "專業、科學及技術服務業-技術員及助理專業人員"        
    ## [33] "支援服務業-技術員及助理專業人員"                    
    ## [34] "用水供應及污染整治業-服務及銷售工作人員"            
    ## [35] "礦業及土石採取業-技藝、機械設備操作及組裝人員"      
    ## [36] "用水供應及污染整治業-技藝、機械設備操作及組裝人員"  
    ## [37] "服務業部門-技藝、機械設備操作及組裝人員"            
    ## [38] "教育服務業-技術員及助理專業人員"                    
    ## [39] "服務業部門-專業人員"                                
    ## [40] "運輸及倉儲業"                                       
    ## [41] "資訊及通訊傳播業-技術員及助理專業人員"              
    ## [42] "醫療保健服務業-技藝、機械設備操作及組裝人員"        
    ## [43] "用水供應及污染整治業-事務支援人員"                  
    ## [44] "金融及保險業-事務支援人員"                          
    ## [45] "服務業部門-技術員及助理專業人員"                    
    ## [46] "藝術、娛樂及休閒服務業-事務支援人員"                
    ## [47] "藝術、娛樂及休閒服務業"                             
    ## [48] "營造業-事務支援人員"                                
    ## [49] "工業及服務業部門-專業人員"                          
    ## [50] "服務業部門-事務支援人員"                            
    ## [51] "電力及燃氣供應業-技藝、機械設備操作及組裝人員"      
    ## [52] "教育服務業-專業人員"                                
    ## [53] "專業、科學及技術服務業-服務及銷售工作人員"          
    ## [54] "服務業部門"                                         
    ## [55] "其他服務業"                                         
    ## [56] "製造業-專業人員"                                    
    ## [57] "工業部門-專業人員"                                  
    ## [58] "資訊及通訊傳播業-服務及銷售工作人員"

### 主要的職業種別是哪些種類呢?

``` r
bb<-sapply(strsplit(aa$職業別,"-"),'[',1)
knitr::kable(table(bb))
```

| bb                     |  Freq|
|:-----------------------|-----:|
| 工業及服務業部門       |     1|
| 工業部門               |     1|
| 不動產業               |     1|
| 支援服務業             |     3|
| 用水供應及污染整治業   |     6|
| 住宿及餐飲業           |     4|
| 其他服務業             |     5|
| 服務業部門             |     5|
| 金融及保險業           |     1|
| 專業、科學及技術服務業 |     5|
| 教育服務業             |     5|
| 資訊及通訊傳播業       |     5|
| 運輸及倉儲業           |     4|
| 電力及燃氣供應業       |     2|
| 製造業                 |     1|
| 營造業                 |     3|
| 醫療保健服務業         |     2|
| 藝術、娛樂及休閒服務業 |     3|
| 礦業及土石採取業       |     1|

男女同工不同酬現況分析
----------------------

男女同工不同酬一直是性別平等中很重要的問題，分析資料來源為103到106年度的大學畢業薪資。

### 103到106年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?

``` r
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
```

    ##                                                 職業別  X103
    ## 1        礦業及土石採取業-技藝、機械設備操作及組裝人員 84.97
    ## 2              教育服務業-技藝、機械設備操作及組裝人員 88.49
    ## 3                      其他服務業-技術員及助理專業人員 89.36
    ## 4        電力及燃氣供應業-技藝、機械設備操作及組裝人員 91.77
    ## 5                  礦業及土石採取業-服務及銷售工作人員 92.57
    ## 6                                               營造業 95.58
    ## 7                              教育服務業-事務支援人員 95.83
    ## 8                                           教育服務業 95.91
    ## 9  藝術、娛樂及休閒服務業-技藝、機械設備操作及組裝人員 96.13
    ## 10                                          其他服務業 96.21

``` r
manhigher104<-select(college,職業別,X104)%>%filter(X104<100)%>%arrange(X104)
manhigher104[1:10,]
```

    ##                                               職業別  X104
    ## 1      電力及燃氣供應業-技藝、機械設備操作及組裝人員 91.69
    ## 2                      教育服務業-服務及銷售工作人員 91.90
    ## 3              礦業及土石採取業-技術員及助理專業人員 92.42
    ## 4      礦業及土石採取業-技藝、機械設備操作及組裝人員 93.10
    ## 5                                   礦業及土石採取業 95.28
    ## 6                            其他服務業-事務支援人員 95.47
    ## 7                營造業-技藝、機械設備操作及組裝人員 95.64
    ## 8  用水供應及污染整治業-技藝、機械設備操作及組裝人員 95.90
    ## 9                                             營造業 96.35
    ## 10                                        教育服務業 96.44

``` r
manhigher105<-select(college,職業別,X105)%>%filter(X105<100)%>%arrange(X105)
manhigher105[1:10,]
```

    ##                                           職業別  X105
    ## 1          不動產業-技藝、機械設備操作及組裝人員 91.38
    ## 2                        醫療保健服務業-專業人員 94.98
    ## 3              用水供應及污染整治業-事務支援人員 95.04
    ## 4                            營造業-事務支援人員 95.65
    ## 5                          不動產業-事務支援人員 95.66
    ## 6                                         營造業 95.78
    ## 7                                營造業-專業人員 96.52
    ## 8  資訊及通訊傳播業-技藝、機械設備操作及組裝人員 96.64
    ## 9                    不動產業-服務及銷售工作人員 96.68
    ## 10                                    其他服務業 96.72

``` r
manhigher106<-select(college,職業別,X106)%>%filter(X106<100)%>%arrange(X106)
manhigher106[1:10,]
```

    ##                                           職業別  X106
    ## 1  電力及燃氣供應業-技藝、機械設備操作及組裝人員 95.51
    ## 2                      營造業-服務及銷售工作人員 95.93
    ## 3                        其他服務業-事務支援人員 96.23
    ## 4          電力及燃氣供應業-技術員及助理專業人員 96.54
    ## 5                                     其他服務業 96.57
    ## 6      住宿及餐飲業-技藝、機械設備操作及組裝人員 96.58
    ## 7                                         營造業 96.71
    ## 8                            教育服務業-專業人員 96.71
    ## 9                      運輸及倉儲業-事務支援人員 96.83
    ## 10               其他服務業-技術員及助理專業人員 96.84

電力及燃氣供應業-技藝、機械設備操作及組裝人員、營造業至少有三年男生的薪資比女生高。

### 哪些行業女生薪資比男生薪資多?

``` r
womanhigher103<-select(college,職業別,X103)%>%filter(X103>100)%>%arrange(desc(X103))
womanhigher103[1:10,]
```

    ##      職業別 X103
    ## NA     <NA>   NA
    ## NA.1   <NA>   NA
    ## NA.2   <NA>   NA
    ## NA.3   <NA>   NA
    ## NA.4   <NA>   NA
    ## NA.5   <NA>   NA
    ## NA.6   <NA>   NA
    ## NA.7   <NA>   NA
    ## NA.8   <NA>   NA
    ## NA.9   <NA>   NA

``` r
womanhigher104<-select(college,職業別,X104)%>%filter(X104>100)%>%arrange(desc(X104))
womanhigher104[1:10,]
```

    ##                                                   職業別   X104
    ## 1    專業、科學及技術服務業-技藝、機械設備操作及組裝人員 100.26
    ## NA                                                  <NA>     NA
    ## NA.1                                                <NA>     NA
    ## NA.2                                                <NA>     NA
    ## NA.3                                                <NA>     NA
    ## NA.4                                                <NA>     NA
    ## NA.5                                                <NA>     NA
    ## NA.6                                                <NA>     NA
    ## NA.7                                                <NA>     NA
    ## NA.8                                                <NA>     NA

``` r
womanhigher105<-select(college,職業別,X105)%>%filter(X105>100)%>%arrange(desc(X105))
womanhigher105[1:10,]
```

    ##                     職業別   X105
    ## 1    金融及保險業-專業人員 100.11
    ## NA                    <NA>     NA
    ## NA.1                  <NA>     NA
    ## NA.2                  <NA>     NA
    ## NA.3                  <NA>     NA
    ## NA.4                  <NA>     NA
    ## NA.5                  <NA>     NA
    ## NA.6                  <NA>     NA
    ## NA.7                  <NA>     NA
    ## NA.8                  <NA>     NA

``` r
womanhigher106<-select(college,職業別,X106)%>%filter(X106>100)%>%arrange(desc(X106))
womanhigher106[1:10,]
```

    ##                                   職業別   X106
    ## 1    資訊及通訊傳播業-服務及銷售工作人員 100.33
    ## NA                                  <NA>     NA
    ## NA.1                                <NA>     NA
    ## NA.2                                <NA>     NA
    ## NA.3                                <NA>     NA
    ## NA.4                                <NA>     NA
    ## NA.5                                <NA>     NA
    ## NA.6                                <NA>     NA
    ## NA.7                                <NA>     NA
    ## NA.8                                <NA>     NA

女生薪資比男生高的職業雖然比較少，但在103年之後開始增加，且都是比較偏服務性質的職業。

研究所薪資差異
--------------

以106年度的資料來看，哪個職業別念研究所最划算呢 (研究所學歷薪資與大學學歷薪資增加比例最多)?

``` r
salary106<-data.frame("職業別"=X106_LevelSalary$大職業別,universe=X106_LevelSalary$`大學-薪資`,research=X106_LevelSalary$`研究所及以上-薪資`)
salary106$universe<-gsub("—",NA,salary106$universe)
salary106$research<-gsub("—",NA,salary106$research)
salary106$universe<-as.numeric(salary106$universe)
salary106$research<-as.numeric(salary106$research)
salary_106<-cbind(salary106,increase=salary106$research/salary106$universe)%>%arrange(desc(`increase`))
salary_106[1:10,1]
```

    ##  [1] 礦業及土石採取業-事務支援人員      專業_科學及技術服務業             
    ##  [3] 其他服務業-技術員及助理專業人員    專業_科學及技術服務業-事務支援人員
    ##  [5] 批發及零售業                       製造業                            
    ##  [7] 藝術_娛樂及休閒服務業-事務支援人員 工業部門                          
    ##  [9] 工業及服務業部門                   服務業部門                        
    ## 140 Levels: 工業及服務業部門 ... 礦業及土石採取業-專業人員

礦業及土石採取業-事務支援人員 是增加比例最多的職業，多了1.208946%

我有興趣的職業別薪資狀況分析
----------------------------

我有興趣的職業:"藝術\_娛樂及休閒服務業-技術員及助理專業人員","醫療保健服務業-技術員及助理專業人員","教育服務業-技術員及助理專業人員","支援服務業-技術員及助理專業人員","專業\_科學及技術服務業-專業人員" \#\#\# 有興趣的職業別篩選，呈現薪資

``` r
hobit<-filter(salary_106,職業別%in% c("藝術_娛樂及休閒服務業-技術員及助理專業人員","醫療保健服務業-技術員及助理專業人員","教育服務業-技術員及助理專業人員","支援服務業-技術員及助理專業人員","專業_科學及技術服務業-專業人員"))
```

### 這些職業別研究所薪資與大學薪資差多少呢？

``` r
hobitsalary<-cbind(hobit,dif=hobit$research-hobit$universe)
```

雖然最多多了五千多塊，但我還是比較偏向畢業後先就業。
