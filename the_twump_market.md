Finance Data
============

``` r
library(quantmod) 
```

    ## Loading required package: xts

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: TTR

    ## Version 0.4-0 included new data defaults. See ?getSymbols.

``` r
SP500 = getSymbols(na.omit("^GSPC",from = as.Date("2016-01-01")))
```

    ## 'getSymbols' currently uses auto.assign=TRUE by default, but will
    ## use auto.assign=FALSE in 0.5-0. You will still be able to use
    ## 'loadSymbols' to automatically load data. getOption("getSymbols.env")
    ## and getOption("getSymbols.auto.assign") will still be checked for
    ## alternate defaults.
    ## 
    ## This message is shown once per session and may be disabled by setting 
    ## options("getSymbols.warning4.0"=FALSE). See ?getSymbols for details.

    ## 
    ## WARNING: There have been significant changes to Yahoo Finance data.
    ## Please see the Warning section of '?getSymbols.yahoo' for details.
    ## 
    ## This message is shown once per session and may be disabled by setting
    ## options("getSymbols.yahoo.warning"=FALSE).

``` r
DJIA = getSymbols(na.omit("^DJI",from = as.Date("2016-01-01")))
Nasdaq = getSymbols(na.omit("^IXIC",from = as.Date("2016-01-01")))
DRSP500= dailyReturn(GSPC)
GSPC_Ret = merge(GSPC,DRSP500)
DRDJI= dailyReturn(DJI)
DJI_Ret = merge(DJI,DRDJI)
DRNDQ= dailyReturn(IXIC)
IXIC_Ret = merge(IXIC,DRNDQ)
```

The Tweets
==========

``` r
tweets= read.csv("Twitter-Download_Donald-Trump_7375-Tweets.csv")
tweets= tweets[,c(1:3,9,10)]
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
tweets$Cdate = ymd(tweets$Date)
tweets2017= read.csv("trump_tweets_2017.csv")
tweets2017 = tweets2017[,2:6]
names(tweets2017)=c("Date","Time","Tweet_Text","twt_favourites_IS_THIS_LIKE_QUESTION_MARK","Retweets")
tweets2017$Cdate = dmy(tweets2017$Date)
```

``` r
tweets=rbind(tweets,tweets2017)
```

Building A Sector Dictionary
============================

``` r
#sector dictionary
library(jsonlite)
library(plyr)
```

    ## 
    ## Attaching package: 'plyr'

    ## The following object is masked from 'package:lubridate':
    ## 
    ##     here

``` r
word=c("health&care","transportation","finance","technology","capital&goods","consumer&services","consumer&non-durables","public&utilities","miscellaneous","basic&industries","energy")
url= paste("https://api.datamuse.com/words?rel_trg=",word,"&max=200",sep="")
dict=data.frame()

for(i in 1:length(url)){
  temp =fromJSON(url[i])
  dict[1:nrow(temp),i]=temp[1]
}
colnames(dict)=word
```

Mood Score and Sector Split
===========================

``` r
#mood dictionary 
HIDict = readLines("inqdict.txt")
dict_pos = HIDict[grep("Pos",HIDict)]
poswords = NULL
for (s in dict_pos) {
    s = strsplit(s,"#")[[1]][1]
    poswords = c(poswords,strsplit(s," ")[[1]][1])
}
dict_neg = HIDict[grep("Neg",HIDict)]
negwords = NULL
for (s in dict_neg) {
    s = strsplit(s,"#")[[1]][1]
    negwords = c(negwords,strsplit(s," ")[[1]][1])
}
poswords = tolower(poswords)
negwords = tolower(negwords)
#mood score + sector split
Sys.setlocale('LC_ALL','C')
```

    ## [1] "C/C/C/C/C/en_US.UTF-8"

``` r
library(tm)
```

    ## Loading required package: NLP

``` r
library(stringr)
mood=matrix(,length(tweets$Tweet_Text),2)
colnames(mood)=c("positive_score","negative_score")
sectormatrix=matrix(,length(tweets$Tweet_Text),11)
colnames(sectormatrix)=word
for(i in 1:length(tweets$Tweet_Text)) {
    text = as.character(tweets$Tweet_Text[i])
    text = unlist(strsplit(text," "))
    ctext = Corpus(VectorSource(text))
    ctext = tm_map(ctext, removeWords, stopwords("english"))
    ctext = tm_map(ctext, removePunctuation) 
    ctext = tm_map(ctext, tolower)
    text = NULL
        for (j in 1:length(ctext)) {
            temp = ctext[[j]]$content
            if (temp!="") { text = c(text,temp) }
        }
        text = as.array(text)
        text = paste(text,collapse="\n")
        text = str_replace_all(text, "[\r\n]" , " ")
        text = unlist(strsplit(text," "))
posmatch = match(text,poswords)
numposmatch = length(posmatch[which(posmatch>0)])
negmatch = match(text,negwords)
numnegmatch = length(negmatch[which(negmatch>0)])
mood[i,]=c(numposmatch,numnegmatch)

for(j in 1:11){
sectormatch = match(text,as.character(dict[,j]))
sectorcount=length(sectormatch[which(sectormatch>0)])
sectormatrix[i,j]=sectorcount
}
}
mood=as.data.frame(mood)
mood$total_score=mood$positive_score-mood$negative_score
tweets=cbind(tweets,mood)
sectormatrix2=as.data.frame(sectormatrix)
sectormatrix2$num_industies=rowSums(sectormatrix2)
tweets=cbind(tweets,sectormatrix2)
```

The Master Dataset
==================

``` r
#data$Cyear = year(data$Cdate)
#fd = aggregate(count~Cyear,data,sum)
#tw=aggregate() 

#retweet=aggregate(Retweets~Date,tweets,sum)
#Moodscore = aggregate(tweets[,15]~Date,tweets,sum)
#fav= aggregate(tweets[,9]~Date,tweets,sum)
#tweets1=merge(retweet,Moodscore, by = "Date")
#tweets1=merge(tweets1,fav, by = "Date")

#names(tweets1)=c("Date","Retweets","Mood Score","Favs")

masterdata1=aggregate(tweets[,4]~Cdate,tweets,sum)
masterdata1=cbind(masterdata1,aggregate(tweets[,5]~Cdate,tweets,sum)[,2])
for(i in 7:21){
test=aggregate(tweets[,i]~Cdate,tweets,sum)
masterdata1=cbind(masterdata1,test[,2])
}
colnames(masterdata1)=c("Cdate" ,"Favorite","Retweets",colnames(tweets)[7:21])

library(lubridate)
GSPC_Ret= data.frame(date=index(GSPC_Ret), coredata(GSPC_Ret))
GSPC_Ret$Cdate = ymd(GSPC_Ret$date)
GSPC_Ret$Jump<-ifelse(GSPC_Ret[,8]>0,1,0)
DJI_Ret= data.frame(date=index(DJI_Ret), coredata(DJI_Ret))
DJI_Ret$Cdate = ymd(DJI_Ret$date)
DJI_Ret$Jump<-ifelse(DJI_Ret[,8]>0,1,0)
IXIC_Ret= data.frame(date=index(IXIC_Ret), coredata(IXIC_Ret))
IXIC_Ret$Cdate = ymd(IXIC_Ret$date)
IXIC_Ret$Jump<-ifelse(IXIC_Ret[,8]>0,1,0)

masterdata = merge(masterdata1,GSPC_Ret, by ="Cdate")
masterdata = merge(masterdata,IXIC_Ret, by ="Cdate")
masterdata = merge(masterdata,DJI_Ret, by ="Cdate")
masterdata$Favorite = as.numeric(masterdata$Favorite)
masterdata$Retweets = as.numeric(masterdata$Retweets)

#masterdata$Jump<-ifelse(masterdata$daily.returns>0,1,0)
#masterdata$Jump = as.numeric(masterdata$Jump)
idx= complete.cases(masterdata)
masterdata= masterdata[idx,]
```

Plot
====

``` r
par(mfrow=c(2,1))
plot(masterdata$Cdate,masterdata[,6], "l",col="red", ylab = "Mood Score",xlab = "Date")
plot(masterdata$Cdate,masterdata[,26]*1000,"l",col = "blue",ylab = "SP500 Return",xlab = "Date")
```
Please refer to the Comparison of the Mood Score of Trump and the SP500 Return chart.


#SVM
``` r
x=masterdata[,c(2,3,6:17)]
y=masterdata$Jump.x
xtrain=masterdata[1:200,c(2,3,6:17)]
ytrain=masterdata$Jump.x[1:200]
xtest=masterdata[201:401,c(2,3,6:17)]
ytest=masterdata$Jump.x[201:401]
library(e1071)
#on all data
model<-svm(x,y)
model
```

    ## 
    ## Call:
    ## svm.default(x = x, y = y)
    ## 
    ## 
    ## Parameters:
    ##    SVM-Type:  eps-regression 
    ##  SVM-Kernel:  radial 
    ##        cost:  1 
    ##       gamma:  0.07142857 
    ##     epsilon:  0.1 
    ## 
    ## 
    ## Number of Support Vectors:  456

``` r
out<-predict(model,x)
fitted<-round(out,0)
fitted<-matrix(fitted)
table=table(y,fitted)
table
```

    ##    fitted
    ## y     0   1
    ##   0 145  83
    ##   1  51 189

``` r
return_prediction<-ifelse(out >= 0.5,1,0)
misClasificError<-mean(return_prediction != y)
print(paste('Accuracy',1-misClasificError))
```

    ## [1] "Accuracy 0.713675213675214"

``` r
chisq.test(table)
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  table
    ## X-squared = 84.408, df = 1, p-value < 2.2e-16

``` r
#split to train and test
model<-svm(xtrain,ytrain)
model
```

    ## 
    ## Call:
    ## svm.default(x = xtrain, y = ytrain)
    ## 
    ## 
    ## Parameters:
    ##    SVM-Type:  eps-regression 
    ##  SVM-Kernel:  radial 
    ##        cost:  1 
    ##       gamma:  0.07142857 
    ##     epsilon:  0.1 
    ## 
    ## 
    ## Number of Support Vectors:  195

``` r
out<-predict(model,xtrain)
fitted<-round(out,0)
fitted<-matrix(fitted)
table=table(ytrain,fitted)
table
```

    ##       fitted
    ## ytrain  0  1
    ##      0 52 48
    ##      1 12 88

``` r
return_prediction<-ifelse(out >= 0.5,1,0)
misClasificError<-mean(return_prediction != ytrain)
print(paste('Accuracy',1-misClasificError))
```

    ## [1] "Accuracy 0.7"

``` r
chisq.test(table)
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  table
    ## X-squared = 34.949, df = 1, p-value = 3.384e-09

``` r
out<-predict(model,xtest)
fitted<-round(out,0)
fitted<-matrix(fitted)
table=table(ytest,fitted)
table
```

    ##      fitted
    ## ytest  0  1
    ##     0 47 49
    ##     1 65 40

``` r
return_prediction<-ifelse(out >= 0.5,1,0)
misClasificError<-mean(return_prediction != ytest)
print(paste('Accuracy',1-misClasificError))
```

    ## [1] "Accuracy 0.432835820895522"

``` r
chisq.test(table)
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  table
    ## X-squared = 2.9023, df = 1, p-value = 0.08845

Wordcloud
=========

``` r
library(wordcloud)
```

    ## Loading required package: RColorBrewer

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday,
    ##     week, yday, year

    ## The following objects are masked from 'package:xts':
    ## 
    ##     first, last

``` r
#pre
tweets=data.table(tweets,key="Cdate")
cloudtextpre=tweets$Tweet_Text[1:7730]
cloudtextpre = iconv(cloudtextpre,"WINDOWS-1252","UTF-8")
cloudtextpre=Corpus(VectorSource(cloudtextpre))
cloudtextpre = tm_map(cloudtextpre, removeWords, stopwords("english"))
cloudtextpre = tm_map(cloudtextpre, removePunctuation)
tdm = TermDocumentMatrix(cloudtextpre,control=list(minWordLength=1))
tdmmatrix=as.matrix(tdm)
mostcommon = sort(rowSums(tdmmatrix), decreasing=TRUE)
hundredwords=rownames(as.matrix(mostcommon[1:100]))
wordcloud(hundredwords,mostcommon)
```
Please refer to the Wordcloud-pre graph.


``` r
##post 
cloudtext=tweets$Tweet_Text[7730:nrow(tweets)]
cloudtext=Corpus(VectorSource(cloudtext))
cloudtext = tm_map(cloudtext, removeWords, stopwords("english"))
cloudtext = tm_map(cloudtext, removePunctuation) 
tdm = TermDocumentMatrix(cloudtext,control=list(minWordLength=1))
tdmmatrix=as.matrix(tdm)
mostcommon = sort(rowSums(tdmmatrix), decreasing=TRUE)
hundredwords=rownames(as.matrix(mostcommon[1:100]))
wordcloud(hundredwords,mostcommon)
```
Please refer to the Wordcloud-post graph.

#Topics
``` r
library(topicmodels)
tdmtopic = TermDocumentMatrix(cloudtextpre,control=list(minWordLength=1))
dtm=t(tdmtopic)
dtm = dtm[1:500,]
rowTotals = apply(dtm, 1, sum)
dtm = dtm[rowTotals> 0, ]

res=LDA(dtm, 5, method="Gibbs", control = list(nstart = 5, seed = list(2003,5,63,100001,765), best = TRUE, burnin = 4000, iter = 2000, thin = 500))
res.terms = as.matrix(terms(res,25))
print(res.terms)
```

    ##       Topic 1                            Topic 2          
    ##  [1,] "great"                            "amp"            
    ##  [2,] "will"                             "the"            
    ##  [3,] "america"                          "you"            
    ##  [4,] "makeamericagreatagain"            "people"         
    ##  [5,] "make"                             "truth"          
    ##  [6,] "dont"                             "donald"         
    ##  [7,] "trump2016"                        "obama"          
    ##  [8,] "back"                             "country"        
    ##  [9,] "time"                             "like"           
    ## [10,] "president"                        "know"           
    ## [11,] "money"                            "one"            
    ## [12,] "foxnews"                          "big"            
    ## [13,] "military"                         "think"          
    ## [14,] "never"                            "right"          
    ## [15,] "keep"                             "want"           
    ## [16,] "danscavino"                       "many"           
    ## [17,] "american"                         "michaelcohen212"
    ## [18,] "let<e2><80><b0><c3><9b><c2><aa>s" "done"           
    ## [19,] "fix"                              "iowa"           
    ## [20,] "good"                             "anything"       
    ## [21,] "speak"                            "things"         
    ## [22,] "support"                          "show"           
    ## [23,] "day"                              "needs"          
    ## [24,] "say"                              "last"           
    ## [25,] "doesnt"                           "said"           
    ##       Topic 3           Topic 4                    Topic 5      
    ##  [1,] "realdonaldtrump" "trump"                    "border"     
    ##  [2,] "mccain"          "new"                      "just"       
    ##  [3,] "donald"          "thank"                    "get"        
    ##  [4,] "the"             "gop"                      "seanhannity"
    ##  [5,] "going"           "illegal"                  "poll"       
    ##  [6,] "macys"           "true"                     "politicians"
    ##  [7,] "veterans"        "cant"                     "now"        
    ##  [8,] "love"            "again"                    "people"     
    ##  [9,] "thanks"          "country"                  "need"       
    ## [10,] "john"            "polls"                    "amazing"    
    ## [11,] "vets"            "must"                     "cnn"        
    ## [12,] "give"            "like"                     "via"        
    ## [13,] "usa"             "can"                      "talk"       
    ## [14,] "oreillyfactor"   "job"                      "media"      
    ## [15,] "bad"             "years"                    "hes"        
    ## [16,] "take"            "veterans"                 "vets"       
    ## [17,] "talking"         "donaldtrump"              "telling"    
    ## [18,] "care"            "walker"                   "what"       
    ## [19,] "really"          "presidential"             "please"     
    ## [20,] "sharylattkisson" "drudgereport"             "why"        
    ## [21,] "way"             "its"                      "losing"     
    ## [22,] "change"          "http<e2><80><b0><c3><9b>" "thank"      
    ## [23,] "needs"           "this"                     "hear"       
    ## [24,] "week"            "for"                      "tonight"    
    ## [25,] "nothing"         "republican"               "politics"

``` r
tdmtopic2 = TermDocumentMatrix(cloudtext,control=list(minWordLength=1))
dtm=t(tdmtopic2)
dtm = dtm[1:500,]

res=LDA(dtm, 5, method="Gibbs", control = list(nstart = 5, seed = list(2003,5,63,100001,765), best = TRUE, burnin = 4000, iter = 2000, thin = 500))
res.terms = as.matrix(terms(res,25))
print(res.terms)
```

    ##       Topic 1          Topic 2       Topic 3      Topic 4      Topic 5    
    ##  [1,] "the"            "great"       "today"      "will"       "fake"     
    ##  [2,] "media"          "big"         "people"     "jobs"       "news"     
    ##  [3,] "russia"         "thank"       "democrats"  "country"    "time"     
    ##  [4,] "failing"        "just"        "obamacare"  "america"    "great"    
    ##  [5,] "foxandfriends"  "american"    "house"      "trump"      "one"      
    ##  [6,] "new"            "meeting"     "nytimes"    "make"       "united"   
    ##  [7,] "going"          "election"    "healthcare" "many"       "they"     
    ##  [8,] "even"           "enjoy"       "bad"        "back"       "prime"    
    ##  [9,] "forward"        "now"         "white"      "win"        "security" 
    ## [10,] "very"           "general"     "honor"      "must"       "wrong"    
    ## [11,] "first"          "wonderful"   "president"  "looking"    "tax"      
    ## [12,] "look"           "judge"       "get"        "minister"   "much"     
    ## [13,] "trump"          "happy"       "whitehouse" "always"     "states"   
    ## [14,] "together"       "women"       "again"      "court"      "president"
    ## [15,] "deal"           "wall"        "day"        "decision"   "obama"    
    ## [16,] "story"          "national"    "the"        "support"    "badly"    
    ## [17,] "welcome"        "border"      "long"       "getting"    "good"     
    ## [18,] "tonight"        "republicans" "now"        "washington" "china"    
    ## [19,] "real"           "order"       "others"     "two"        "north"    
    ## [20,] "everyone"       "made"        "never"      "job"        "can"      
    ## [21,] "amazing"        "like"        "right"      "going"      "years"    
    ## [22,] "administration" "come"        "since"      "see"        "billion"  
    ## [23,] "whitehouse"     "optimism"    "way"        "government" "better"   
    ## [24,] "executive"      "talk"        "far"        "morning"    "well"     
    ## [25,] "business"       "trade"       "vote"       "illegal"    "last"

Mood
====

``` r
par(mfrow=c(1,1))
plot(masterdata$Cdate,masterdata[,6], "l",col="red", ylab = "Mood Score",xlab = "Date")
abline(lm(masterdata[,6]~ masterdata$Cdate ))
```
Please refer to the Fluctuation Plot of Trump's Mood Score by Time chart.

``` r
library(rbokeh)
tweets$Count=rep(1,1,nrow(tweets))
tweets$Day = wday(tweets$Cdate)
tweetsag = aggregate(Count~Cdate,tweets,sum)
names(tweetsag)= c("Cdate","Count")
tweets$Count = 1
figure(width=900,height=900) %>% ly_points(x="Cdate",y="Count",data=tweetsag,hover=c(Cdate,Count))%>% ly_lines(x="Cdate",y="Count",data=tweetsag)
```
#######Please refer to the 

``` r
#by day of week total, we need to split this for pre and post president
library(ggplot2)
```

    ## 
    ## Attaching package: 'ggplot2'

    ## The following object is masked from 'package:NLP':
    ## 
    ##     annotate

``` r
figure(width=900,height=900) %>% ly_points(x="Cdate",y="total_score",data=masterdata,hover=c(Cdate,total_score))%>% ly_lines(x="Cdate",y="total_score",data=masterdata,hover=c(Cdate,total_score))
```
#######Please refer to the 

``` r
figure(width=900,height=900) %>% ly_points(x="Cdate",y="total_score",data=masterdata,hover=c(Cdate,total_score))%>% ly_lines(x="Cdate",y="total_score",data=masterdata,hover=c(Cdate,total_score))%>% ly_lines(x="Cdate",y="Count",data=tweetsag,col = "red")
```
#######Please refer to the
As you can see from the chart, the lowest mood score was on the day of ####colorado trump rally####


Avg Tweets Per Day of Week Pre and Post Presidency
==================================================

``` r
idx = tweets$Cdate > "2017-01-19"
tweetspost = tweets[idx,]
idx = tweets$Cdate < "2017-01-20"
tweetspre = tweets[idx,]

tweetspresum = aggregate(tweetspre$Count~Cdate+Day,tweetspre,sum)
names(tweetspresum)= c("Cdate","Day","Count")
tweetspreavg = aggregate(Count~Day,tweetspresum,mean)
tweetspostsum = aggregate(tweetspost$Count~Cdate+Day,tweetspost,sum)
names(tweetspostsum)= c("Cdate","Day","Count")
tweetspostavg = aggregate(Count~Day,tweetspostsum,mean)

TweetsPrevsPostAvg =merge(tweetspreavg,tweetspostavg, by = "Day")
names(TweetsPrevsPostAvg) = c("Day","PrePresAvg","PostPresAvg")
TweetsPrevsPostAvg
```

    ##   Day PrePresAvg PostPresAvg
    ## 1   1   12.89744    4.350000
    ## 2   2   14.08974    4.842105
    ## 3   3   16.41026    4.368421
    ## 4   4   14.79487    5.722222
    ## 5   5   13.94937    5.052632
    ## 6   6   13.23377    6.157895
    ## 7   7   13.58974    4.473684

Correlation
===========

``` r
x = as.matrix(tweets[,9:21])
y= as.matrix(tweets[,4])
res =lm(y~x)
summary(res)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -20430  -9294  -4578   4752 614419 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             12315.4      201.6  61.082  < 2e-16 ***
    ## xtotal_score             -880.4      116.4  -7.564 4.31e-14 ***
    ## xhealth&care             -120.8     1216.0  -0.099 0.920888    
    ## xtransportation          4915.8     2206.9   2.228 0.025940 *  
    ## xfinance                  381.5      806.7   0.473 0.636317    
    ## xtechnology              1631.4     3435.1   0.475 0.634856    
    ## xcapital&goods           1715.7     1898.1   0.904 0.366073    
    ## xconsumer&services        954.6      857.9   1.113 0.265912    
    ## xconsumer&non-durables       NA         NA      NA       NA    
    ## xpublic&utilities        -870.0     1457.9  -0.597 0.550662    
    ## xmiscellaneous           3425.1      969.2   3.534 0.000412 ***
    ## xbasic&industries       -1462.3     1431.3  -1.022 0.306978    
    ## xenergy                  2204.2     1275.3   1.728 0.083972 .  
    ## xnum_industies               NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16790 on 8401 degrees of freedom
    ## Multiple R-squared:  0.01051,    Adjusted R-squared:  0.009219 
    ## F-statistic: 8.116 on 11 and 8401 DF,  p-value: 2.8e-14

``` r
x = as.matrix(tweets[,9:21])
y= as.matrix(tweets[,5])
res =lm(y~x)
summary(res)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -64871 -11270  -9746  -4601 381297 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             12289.5      367.5  33.436  < 2e-16 ***
    ## xtotal_score             -252.6      212.2  -1.190 0.233908    
    ## xhealth&care             9241.3     2216.7   4.169 3.09e-05 ***
    ## xtransportation         15158.9     4023.1   3.768 0.000166 ***
    ## xfinance                 9530.9     1470.7   6.481 9.65e-11 ***
    ## xtechnology              9426.9     6262.1   1.505 0.132262    
    ## xcapital&goods           9342.9     3460.2   2.700 0.006946 ** 
    ## xconsumer&services       8570.2     1564.0   5.480 4.39e-08 ***
    ## xconsumer&non-durables       NA         NA      NA       NA    
    ## xpublic&utilities       -1615.0     2657.7  -0.608 0.543423    
    ## xmiscellaneous          10312.9     1766.8   5.837 5.51e-09 ***
    ## xbasic&industries       -3368.3     2609.3  -1.291 0.196786    
    ## xenergy                  2663.4     2324.9   1.146 0.252001    
    ## xnum_industies               NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 30610 on 8401 degrees of freedom
    ## Multiple R-squared:  0.02897,    Adjusted R-squared:  0.0277 
    ## F-statistic: 22.78 on 11 and 8401 DF,  p-value: < 2.2e-16

``` r
x = as.matrix(tweetspre[,9:21])
y= as.matrix(tweetspre[,4])
res =lm(y~x)
summary(res)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -19703  -8935  -5012   4364 614955 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             11759.7      211.9  55.491  < 2e-16 ***
    ## xtotal_score             -900.2      123.7  -7.279  3.7e-13 ***
    ## xhealth&care             -152.8     1469.7  -0.104   0.9172    
    ## xtransportation          5345.2     2802.1   1.908   0.0565 .  
    ## xfinance                 -339.4      939.5  -0.361   0.7179    
    ## xtechnology               608.8     3705.7   0.164   0.8695    
    ## xcapital&goods           1343.9     2172.0   0.619   0.5361    
    ## xconsumer&services        866.2     1009.9   0.858   0.3911    
    ## xconsumer&non-durables       NA         NA      NA       NA    
    ## xpublic&utilities       -1350.4     1620.5  -0.833   0.4047    
    ## xmiscellaneous           3724.4     1141.2   3.264   0.0011 ** 
    ## xbasic&industries       -1844.5     1499.2  -1.230   0.2186    
    ## xenergy                  1654.2     1395.0   1.186   0.2357    
    ## xnum_industies               NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16940 on 7708 degrees of freedom
    ## Multiple R-squared:  0.009782,   Adjusted R-squared:  0.008369 
    ## F-statistic: 6.922 on 11 and 7708 DF,  p-value: 9.458e-12


``` r
x = as.matrix(tweetspre[,9:21])
y= as.matrix(tweetspre[,5])
res =lm(y~x)
summary(res)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -23889  -6054  -4663   -843 345808 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              7171.6      228.8  31.344  < 2e-16 ***
    ## xtotal_score             -377.0      133.5  -2.824  0.00476 ** 
    ## xhealth&care             2377.2     1586.7   1.498  0.13413    
    ## xtransportation          6139.4     3025.3   2.029  0.04246 *  
    ## xfinance                 1550.7     1014.3   1.529  0.12635    
    ## xtechnology              4445.0     4000.8   1.111  0.26659    
    ## xcapital&goods           2485.5     2345.0   1.060  0.28921    
    ## xconsumer&services       4429.1     1090.4   4.062 4.91e-05 ***
    ## xconsumer&non-durables       NA         NA      NA       NA    
    ## xpublic&utilities       -2768.6     1749.6  -1.582  0.11360    
    ## xmiscellaneous           6391.0     1232.1   5.187 2.19e-07 ***
    ## xbasic&industries       -3719.7     1618.6  -2.298  0.02158 *  
    ## xenergy                    28.5     1506.1   0.019  0.98490    
    ## xnum_industies               NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18290 on 7708 degrees of freedom
    ## Multiple R-squared:  0.01107,    Adjusted R-squared:  0.009659 
    ## F-statistic: 7.844 on 11 and 7708 DF,  p-value: 1.075e-13

``` r
x = as.matrix(tweetspost[,9:21])
y= as.matrix(tweetspost[,4])
res =lm(y~x)
summary(res)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -22487  -7901  -2954   3642 103518 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             19943.9      590.4  33.782  < 2e-16 ***
    ## xtotal_score             -811.1      297.8  -2.724  0.00663 ** 
    ## xhealth&care            -3627.4     2110.9  -1.718  0.08620 .  
    ## xtransportation           654.2     3080.6   0.212  0.83188    
    ## xfinance                -1155.5     1329.3  -0.869  0.38503    
    ## xtechnology              7381.0     7655.5   0.964  0.33534    
    ## xcapital&goods            657.1     3203.2   0.205  0.83752    
    ## xconsumer&services        380.5     1590.3   0.239  0.81097    
    ## xconsumer&non-durables       NA         NA      NA       NA    
    ## xpublic&utilities        1181.3     2775.0   0.426  0.67046    
    ## xmiscellaneous            196.6     1509.0   0.130  0.89637    
    ## xbasic&industries        7319.4     5033.5   1.454  0.14639    
    ## xenergy                  2422.5     2644.1   0.916  0.35991    
    ## xnum_industies               NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13040 on 651 degrees of freedom
    ## Multiple R-squared:  0.02636,    Adjusted R-squared:  0.009906 
    ## F-statistic: 1.602 on 11 and 651 DF,  p-value: 0.0937

``` r
x = as.matrix(tweetspost[,9:21])
y= as.matrix(tweetspost[,5])
res =lm(y~x)
summary(res)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -89384 -29377  -7512  23474 308752 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             85038.7     2293.1  37.084   <2e-16 ***
    ## xtotal_score             -456.6     1156.7  -0.395    0.693    
    ## xhealth&care           -10871.8     8199.1  -1.326    0.185    
    ## xtransportation          1433.3    11965.7   0.120    0.905    
    ## xfinance                -1655.2     5163.3  -0.321    0.749    
    ## xtechnology             32397.1    29735.5   1.090    0.276    
    ## xcapital&goods           4397.0    12441.9   0.353    0.724    
    ## xconsumer&services       3205.5     6177.0   0.519    0.604    
    ## xconsumer&non-durables       NA         NA      NA       NA    
    ## xpublic&utilities       -2439.8    10778.7  -0.226    0.821    
    ## xmiscellaneous           1596.2     5861.2   0.272    0.785    
    ## xbasic&industries       26911.7    19551.1   1.376    0.169    
    ## xenergy                 -1016.6    10270.0  -0.099    0.921    
    ## xnum_industies               NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 50660 on 651 degrees of freedom
    ## Multiple R-squared:  0.01001,    Adjusted R-squared:  -0.006718 
    ## F-statistic: 0.5984 on 11 and 651 DF,  p-value: 0.8309

Also kind of interesting behavior that before his presidency the \# of favs and retweets were correlated to his mood score of tweets, after presidency favs and RTs are not statistically significantly correlated. \#Text summary

``` r
text_summary = function(text, n) {
  m = length(text)  # No of sentences in input
  jaccard = matrix(0,m,m)  #Store match index
  for (i in 1:m) {
    for (j in i:m) {
      a = text[i]; aa = unlist(strsplit(a," "))
      b = text[j]; bb = unlist(strsplit(b," "))
      jaccard[i,j] = length(intersect(aa,bb))/
                          length(union(aa,bb))
      jaccard[j,i] = jaccard[i,j]
    }
  }
  similarity_score = rowSums(jaccard)
  res = sort(similarity_score, index.return=TRUE,
          decreasing=TRUE)
  idx = res$ix[1:n]
  summary = text[idx]
}

presum1=text_summary(as.character(tweets$Tweet_Text[1:1000]),10)
presum2=text_summary(as.character(tweets$Tweet_Text[1001:2000]),10)
presum3=text_summary(as.character(tweets$Tweet_Text[2001:3000]),10)
presum4=text_summary(as.character(tweets$Tweet_Text[3001:4000]),10)
presum5=text_summary(as.character(tweets$Tweet_Text[4001:5000]),10)
presum6=text_summary(as.character(tweets$Tweet_Text[5001:6000]),10)
presum7=text_summary(as.character(tweets$Tweet_Text[6001:7000]),10)
presum8=text_summary(as.character(tweets$Tweet_Text[7001:7730]),10)
sumslist=list(presum1,presum2,presum3,presum4,presum5,presum6,presum7,presum8)
sumslist=unlist(sumslist)
presum=text_summary(as.character(sumslist),10)
presum
```

    ##  [1] "Thank you to all for the wonderful reviews of my foreign policy speech. I will soon be speaking in great detail on numerous other topics!"  
    ##  [2] "Such a great honor to be the Republican Nominee for President of the United States. I will work hard and never let you down! AMERICA FIRST!"
    ##  [3] "I want to thank all my friends in Macon for the special evening and great reception. What a crowd of incredible people!"                    
    ##  [4] "Thank you @SenJohnMcCain for your kind remarks on the important issue of PTSD and the dishonest media. Great to be in Arizona yesterday!"   
    ##  [5] "I will be making a big speech tomorrow to discuss the failed policies and bad judgment of Crooked Hillary Clinton."                         
    ##  [6] "Thank you to the @washingtonpost for the accurate and very discriptive  story on my speech in Alabama last night. It was a great evening!"  
    ##  [7] "Thank you to the LGBT community! I will fight for you while Hillary brings in more people that will threaten your freedoms and beliefs."    
    ##  [8] "Also, tune in to the @TodayShow at 7:00am. I will be on to discuss the campaign, my new ads and #CrippledAmerica."                          
    ##  [9] "I will be making the announcement of my Vice Presidential pick on Friday at 11am in Manhattan. Details to follow."                          
    ## [10] "This will prove to be a great time in the lives of ALL Americans. We will unite and we will win, win, win!"

``` r
postsum=text_summary(as.character(tweets$Tweet_Text[7731:8413]),10)
postsum
```

    ##  [1] "Today I will meet with Canadian PM Trudeau and a group of leading business women to discuss women in the workforce. https://t.co/bFAHPRXHdP"                                     
    ##  [2] "It was a great honor to have President Xi Jinping and Madame Peng Liyuan of China as our guests in the United States. Tremendous..."                                             
    ##  [3] "I have tremendous respect for women and the many roles they serve that are vital to the fabric of our society and our economy."                                                  
    ##  [4] "I explained to the President of China that a trade deal with the U.S. will be far better for them if they solve the North Korean problem!"                                       
    ##  [5] "What about all of the contact with the Clinton campaign and the Russians? Also, is it true that the DNC would not let the FBI in to look?"                                       
    ##  [6] "Despite what you hear in the press, healthcare is coming along great. We are talking to many groups and it will end in a beautiful picture!"                                     
    ##  [7] "Iran was on its last legs and ready to collapse until the U.S. came along and gave it a life-line in the form of the Iran Deal: $150 billion"                                    
    ##  [8] "Only by enlisting the full potential of women in our society will we be truly able to #MakeAmericaGreatAgain\360\237\207\272\360\237\207\270\342\200\246 https://t.co/cxhgYaxiek"
    ##  [9] "The Democrats, without a leader, have become the party of obstruction.They are only interested in themselves and not in what's best for U.S."                                    
    ## [10] "A fantastic day and evening in Washington D.C.Thank you to @FoxNews and so many other news outlets for the GREAT reviews of the speech!"
