

                                   ####VISITORS AND CONVERSION RATE####


visitors <- read.csv('visitors.csv')
quotes <- read.csv('quotes.csv')
head(visitors)
head(quotes)
colSums(unique(quotes$pro_id))


#Conversion into date format
visitors <- visitors %>% mutate(session_date = as.Date(session_date , format = '%m/%d/%y'))
head(visitors)


#Variation of visitors with time 
pl1 <- ggplot(visitors , aes(x = session_date)) + geom_bar(color = 'Black' )+ coord_flip() + geom_text(stat = 'count' , aes(label = ..count..), hjust =-0.1 )  +ggtitle('                        Variation of Visitor over time') + labs(y = 'Day of Visit' , x = 'No. of Visitors')   
print(pl1)



#No. of users sending the request
pl2 <- ggplot(visitors , aes(x = sent_request)) + geom_bar(color = 'Black' )  + geom_text(stat = 'count' , aes(label = ..count..)  , vjust = -0.1) + labs(x = 'Request Sent , Yes = 1 , No = 0' , y = 'No. of Requests')+ ggtitle('                             Users sending Request')
print(pl2)


###Category and device influencing this metrics
vis.mod.data <- visitors %>% select(session_date,category_name,device,request_id,sent_request)
head(vis.mod.data)


vis.mod.data$session_date.numeric = as.numeric(vis.mod.data$session_date)/(24*60*60)
write.csv(vis.mod.data,'C:\\Users\\Vanshaj\\Documents\\vis.mod.data' , row.names = FALSE)
#Through this above csv


library(caTools)
set.seed(101)
sample <- sample.split(vis.mod.data$session_date.numeric , SplitRatio = 0.7)
df.train <- subset(vis.mod.data , sample == TRUE)
df.test <- subset(vis.mod.data , sample == FALSE)

model1 <- lm(session_date.numeric ~ category_name + device , df.train)
summary(model1)
plot(model1)

model2 <- glm(sent_request ~ category_name + device, family = binomial , df.train)
summary(model2)


#str(df.train)




                                      #####QUOTES PER REQUEST######

#No. of Quotes per request


pl5 <- ggplot(quotes , aes(x = request_id )) + geom_histogram(binwidth = 0.1) + ggtitle('Variation of No. of Requests with Request Id') + labs(x = 'Request Id' , y = 'No. of Quotes ')
print(pl5)

pl6 <- ggplot(quotes , aes(x = request_id )) + geom_bar(binwidth = 0.1) + coord_flip() 
print(pl6)

pl18 <- ggplot(abc , aes(x = req_freq$Freq)) + geom_bar(color = 'Black') + geom_text(stat = 'Count' , aes(label = ..count..) , vjust = -0.1) + ggtitle('                         No. of Quotes per Request') + labs(x = 'No. of Quotes' , y = 'Count of No. Quotes')
print(pl18)
 


req_freq <- table(quotes$request_id)
req_freq <- as.data.frame(req_freq)
head(req_freq)
# vis.mod.data <- cbind(vis.mod.data , req_freq$Freq)


# Calculated this  Excel File by Applying Vlookups to find request IDs which have been quoted by the PROS 
#and removing those wwhich have been not quoted (Attached the Excel file for same in the mail)
abc <- read.csv('Quotes_pereq.csv')
abc <- cbind(abc , req_freq$Freq)
head(abc)
abc <- data.frame(abc)
# abc <- abc %>% rename(Count_of_Request = req_freq$Freq)


model43 <- lm(req_freq$Freq ~  device + category_name  , abc)
summary(model43)
str(abc)



  

                                      ######### JOB VALUE ########## 

#Made this excel file by applying Vlookups to find the Category name for the respective Request IDs and other simultaneous details
#Attached these 2 files in the mail Too

hos.cl <- read.csv('House_Cleaning_Filter.csv')
loc.mov <- read.csv('Local_Moving_Filter.csv')
hos.cl <- na.omit(hos.cl)
loc.mov <- na.omit(loc.mov)
head(hos.cl)
head(loc.mov)
colSums(is.na(hos.cl))



#By Category Distribution of Quote Prices (Do look bw the Histogram and Frequency Polygon as to which one to use)


#Local Moving Category 
pl6 <- ggplot(loc.mov , aes( x = quote_price , fill = factor(hired))) + geom_histogram(binwidth = 20 , color = 'Black') +scale_x_continuous(limits = c(0,1400)) +ggtitle('Distribution of Quote Prices') + labs(x = 'Quote Price' , y = 'No. of Quotes ata Respective price')
print(pl6)
pl6o <-  ggplot(loc.mov , aes( x = quote_price )) + geom_freqpoly(binwidth = 20 , color = 'Black') +scale_x_continuous(limits = c(0,1400)) +ggtitle('Distribution of Quote Prices') + labs(x = 'Quote Price' , y = 'No. of Quotes ata Respective price')
print(pl6o)




#House Cleaning Category 
pl7 <- ggplot(hos.cl , aes(x = quote_price)) +geom_freqpoly(binwidth = 15) +scale_x_continuous(limits = c(0,700)) + ggtitle('Distribution of Quote Prices') + labs(x = 'Quote Price' , y = 'No. of Quotes ata Respective price')
print(pl7)
pl7o <-  ggplot(hos.cl , aes(x = quote_price , fill = factor(hired))) +geom_histogram(binwidth = 15 , color = 'Black') +scale_x_continuous(limits = c(0,700)) + ggtitle('Distribution of Quote Prices') + labs(x = 'Quote Price' , y = 'No. of Quotes ata Respective price')
print(pl7o)



###Grouping the Price in House Cleaning Category####


#Filtering the Price Range Below 900 as Sales above this price are Minimal and dont jave any major effect on Revenue Generation of the Company

hos.cl <- hos.cl %>% arrange(quote_price) 
hos.cl <- hos.cl %>% filter(hos.cl$quote_price < 900)
hos.cl

#Function to group Prices according to the 
grp2 <- function(prc){
  if ( 0 <= prc & prc < 20 ){
    return('10')
  }else if (20 <= prc & prc < 40){
    return('30')
  }else if (40 <= prc & prc < 60){
    return('50')
  }else if (60 <= prc & prc < 80){
    return('70')
  }else if (80 <= prc & prc < 100){
    return('90')
  }else if (100 <= prc & prc < 120){
    return('110')
  }else if (120 <= prc & prc < 140){
    return('130')
  }else if (140 <= prc & prc < 160){
    return('150')
  }else if (160 <= prc & prc < 180){
    return('170')
  }else if (180 <= prc & prc < 200){
    return('190')
  }else if (200 <= prc & prc < 220){
    return('210')
  }else if (220 <= prc & prc < 240){
    return('230')
  }else if (240 <= prc & prc < 260){
    return('250')
  }else if (260 <= prc & prc < 280){
    return('270')
  }else if (280 <= prc & prc < 300){
    return('290')
  }else if (300 <= prc & prc < 320){
    return('310')
  }else if (320 <= prc & prc < 340){
    return('330')
  }else if (340 <= prc & prc < 360){
    return('350')
  }else if (360 <= prc & prc < 380){
    return('370')
  }else if (380 <= prc & prc < 400){
    return('390')
  }else if (400 <= prc & prc < 420){
    return('410')
  }else if (420 <= prc & prc < 440){
    return('430')
  }else if (440 <= prc & prc < 460){
    return('450')
  }else if (460 <= prc & prc < 480){
    return('470')
  }else if (480 <= prc & prc < 500){
    return('490')
  }else if (500 <= prc & prc < 520){
    return('510')
  }else if (520 <= prc & prc < 540){
    return('530')
  }else if (540 <= prc & prc < 560){
    return('550')
  }else if (560 <= prc & prc < 580){
    return('570')
  }else if (580 <= prc & prc < 600){
    return('590')
  }else if (600 <= prc & prc < 620){
    return('610')
  }else if (620 <= prc & prc < 640){
    return('630')
  }else if (640 <= prc & prc < 660){
    return('650')
  }else if (660 <= prc & prc < 680){
    return('670')
  }else if (680 <= prc & prc < 700){
    return('690')
  }else if (700 <= prc & prc < 720){
    return('710')
  }else if (720 <= prc & prc < 740){
    return('730')
  }else if (740 <= prc & prc < 760){
    return('750')
  }else if (760 <= prc & prc < 780){
    return('770')
  }else if (780 <= prc & prc < 800){
    return('790')
  }else if (800 <= prc & prc < 820){
    return('810')
  }else if (820 <= prc & prc < 840){
    return('830')
  }else if (840 <= prc & prc < 860){
    return('850')
  }else if (860 <= prc & prc < 880){
    return('870')
  }else if (880 <= prc & prc < 900){
    return('890')
  }
}  

#Obtaining the list of Gruped prices 
quote_price_grp <-   sapply(hos.cl$quote_price , grp2)
quote_price_grp <- as.numeric(quote_price_grp)

## Binding with the main dataframe
hos.cl2 <- cbind(hos.cl,quote_price_grp)
head(hos.cl2)
hos.cl3 <- hos.cl2


### Slecting 2 columns only which are required for analysis that is whether hired and grouped prices   

hos.req2<- hos.cl2 %>% select(hired , quote_price_grp)
head(hos.req2)


# pl9 <- ggplot(abc , aes(x = quote_price_grp) , fill = factor(hired))+geom_bar(color = 'black')
# print(pl9)


#Filtering the data for hired professionals

hos.2.hir <- hos.req2 %>% filter(hired == 1)  
head(hos.2.hir)

#Count of Pros Hired acc. to price
i <- 10
count_hired <- NULL
seq_price <- seq(from = 10 , to =890 , by = 20)
for (i in seq_price) {
  x <- (hos.2.hir %>% filter(quote_price_grp == i))
  count_hired[i] <- length(x$quote_price_grp)
}

# Combining the hiring Count and the grouped prices
count_hired <- as.data.frame(count_hired)
count_hired <- na.omit(count_hired)
head(count_hired)

w <- as.data.frame(seq_price)
count_hired <- cbind(count_hired,seq_price)
head(count_hired)


### Linear Regression Model for House Cleaning Category
model12 <- lm(count_hired ~ seq_price, data = count_hired)
summary(model12)


####Supply Side Equation in House Cleaning Category 

hos.cl3 <- hos.cl3 %>% arrange(quote_price_grp)
head(hos.cl3)
sply.tab1 <- table(hos.cl3$quote_price_grp)
sply.tab1 <- as.data.frame(sply.tab1)
head(sply.tab1)

sply.tab1$Var1 <- as.numeric(sply.tab1$Var1)

model33 <- lm(Freq ~ Var1 , sply.tab1)
summary(model33)



####     LOCAL MOVING CATEGORY    #####

# Filtering data with price range less than 1400 because sales above this price are minimal


loc.mov <- loc.mov %>% filter(quote_price <= 1400)

x <- NULL
tyu <- NULL
w <- seq(from = 0 , to = 1400 , by = 20)
grp3 <- function(abc){
  for (x in w){
    if (x <= abc & abc < x + 20){
      return (x+10)
    }
  }
} 

# Obtaining List of Grouped Prices 

quote_price_grp1 <- sapply(loc.mov$quote_price , grp3)
quote_price_grp1 <- unlist(quote_price_grp1)
quote_price_grp1 <- as.data.frame(quote_price_grp1)
head(quote_price_grp1)


## Binding the data with main dataframe

loc.mov1 <- cbind(loc.mov , quote_price_grp1)
loc.mov2 <- loc.mov1
head(loc.mov1)

loc.mov1 <- loc.mov1 %>% filter(hired == 1)  %>% select(hired , quote_price_grp1)
head(loc.mov1)

#### Count of Pros according to price ####

i1 <- 10
count_hired1 <- NULL
seq_price1 <- seq(from = 10 , to = 1390 , by = 20)
for (i1 in seq_price1) {
  x <- (loc.mov1 %>% filter(quote_price_grp1 == i1))
  count_hired1[i1] <- length(x$quote_price_grp1)
}

# Combining the hiring Count and the grouped prices 
count_hired1 <- as.data.frame(count_hired1)
count_hired1 <- na.omit(count_hired1)
head(count_hired1)
count_hired1 <- na.omit(count_hired1)
w <- as.data.frame(seq_price1)
count_hired1 <- cbind(count_hired1,seq_price1)
head(count_hired1)


### Linear Regression Model in Local Moving Category

model13 <- lm(count_hired1 ~ seq_price1, data = count_hired1)
summary(model13)

###Supply Equation in Local Moving Category

loc.mov2 <- loc.mov2 %>% arrange(quote_price_grp1)
head(loc.mov2)
sply.tab <- table(loc.mov2$quote_price_grp)
sply.tab <- as.data.frame(sply.tab)
head(sply.tab)

sply.tab$Var1 <- as.numeric(sply.tab$Var1)

model23 <- lm(Freq ~ Var1 , sply.tab)
summary(model23)










