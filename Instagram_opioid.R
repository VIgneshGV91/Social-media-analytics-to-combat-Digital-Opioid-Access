setwd('C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/Comments')

#installing required libraries
library(readxl)
library(tidyverse)
library(stringi)
library(tm)
library(tmap)
library(corpus)
library(SnowballC)
install.packages("sos")
library("sos")
findFn("laply")
install.packages("plyr")
require(plyr)
library(stringr)

#Aggregating all posts
Instagram_opioid_scraped_data <- read_excel("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/Instagram_opioid_scraped_data.xlsx")
New_opioids04_03 <- read_excel("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/New_opioids04_03.xlsx")
Updated_Data <- read_excel("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/Files/Updated_Data.xlsx")
insta_new_data_04_03 <- read_excel("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/insta_new_data_04_03.xlsx")

agg <- rbind(Instagram_opioid_scraped_data,New_opioids04_03,Updated_Data,insta_new_data_04_03)
agg$description <- as.character(agg$description)
table(duplicated(agg$description))
agg_unique <- agg[!duplicated(agg$description), ]
str(agg_unique)
write.csv(agg_unique, file = "all_posts.csv")

#Code to aggregate Comments in batches
Post_url_1_to_35000 <- read_excel("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/Post_url_1_to_35000.xlsx")


split(Post_url_1_to_35000, (seq(nrow(Post_url_1_to_35000))-1) %/% 500) 
newdata.split <- split(Post_url_1_to_35000, (as.numeric(rownames(Post_url_1_to_35000)) - 1) %/% 500)
str(newdata.split)
mydata <- as.data.frame(newdata.split)
write.csv(mydata, "post500urls.csv")

##Merge files
library(dplyr)
library(readr)
df <- list.files(path='C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/Comments', full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(df, "commentsData_first35000.csv")

##Merge files
library(dplyr)
library(readr)
df <- list.files(path='C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/comm', full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(df, "comments_35k_70k.csv")
comments <- df
comments$X9<-NULL


#Merging Comemnts and posts together
all_posts <- read.csv("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/Comments/all_posts.csv")
str(all_posts)

comments_1st_35k <- read.csv("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/Comments/comments_1st_35k.csv")
comments_2nd_35k <- read.csv("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/Comments/comments_2nd_35k.csv")
comments_3rd_35k <- read.csv("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/Comments/comments_3rd_35k.csv")

comments_1st_35k$X9<-NULL
comments_2nd_35k$X9<-NULL
comments_3rd_35k$X9<-NULL

Comm <- rbind(comments_1st_35k,comments_2nd_35k,comments_3rd_35k)
str(Comm)
write.csv(Comm,"All_Comments.csv")

All_Comments <- read.csv("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/Comments/All_Comments.csv")
all_posts_comments <- merge(x = all_posts, y = All_Comments, by = "postUrl", all = TRUE)
write.csv(all_posts_comments,"All_posts_Comments.csv")

table(is.na(all_posts_comments$comment))
backup_all_posts_comments <- all_posts_comments
apc <- all_posts_comments
str(apc)
#Removing rows which have NAs in Description Column
apc <- apc[!is.na(apc$description),]
apc <- apc[!is.na(apc$postUrl),]
str(apc)
apc_Text <- apc[,c("postUrl","description")]
table(duplicated(apc_Text$description))
apc_Text <- apc_Text[!duplicated(apc_Text$description),]
apc_Text['Type']='Post'
colnames(apc_Text)[which(names(apc_Text) == "description")] <- "Text"

#######################################

apc_Comment <- apc[,c("postUrl","comment")]
apc_Comment <- apc_Comment[!is.na(apc_Comment$comment),]
apc_Comment <- apc_Comment[!is.na(apc_Comment$postUrl),]
table(duplicated(apc_Comment$comment))
apc_Comment <- apc_Comment[!duplicated(apc_Comment$comment),]
apc_Comment['Type']='Comment'
colnames(apc_Comment)[which(names(apc_Comment) == "comment")] <- "Text"
apc_post_comm <- rbind(apc_Text, apc_Comment)
table(duplicated(apc_post_comm$Text))
apc_post_comm <- apc_post_comm[!duplicated(apc_post_comm$Text),]
str(apc_post_comm)
write.csv(apc_post_comm,"All_unique_posts_and_comments.csv")

#Data Cleaning & Preprocessing
sent <- apc_post_comm$Text
sent<-gsub("[<].*[>]"," ",sent)
sent<-gsub("\r?\n|\r"," ",sent)
sent<-gsub(","," ",sent)
sent<-gsub(":"," ",sent)
sent<-gsub("+","",sent)
sent<-gsub("(","",sent)
sent<-gsub(")","",sent)
sent<-gsub("-","",sent)
sent<-gsub('([.])|[[:punct:]]',"\\1",sent)
sent<-gsub('[[:cntrl:]]'," ",sent)
apc_post_comm$Text <- sent
df <- apc_post_comm
df$Text <- gsub("[^\x20-\x7E]", "", df$Text)
df$Text <- gsub("[.{2,}]", "", df$Text)

library(stringr)
df$Text <- str_replace(gsub("\\s+", " ", str_trim(df$Text )), "B", "b")
apc_post_comm <- df
write.csv(apc_post_comm,"apc_post_comm_cleaned.csv")
tostem <-apc_post_comm

#Stemming and Freq words
docs <- Corpus(VectorSource(tostem$Text))
docs <-tm_map(docs,content_transformer(tolower))
#remove punctuation
docs <- tm_map(docs, removePunctuation)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs,stemDocument, language = "english")
docs_df <- data.frame(text=sapply(docs, identity),stringsAsFactors=F)
afterstem <- cbind(tostem,docs_df)
colnames(afterstem)[which(names(afterstem) == "text")] <- "Stemmed_Text"
table(is.na(afterstem$text))
write.csv(afterstem,"Text_After_Stemming.csv")

#Data Filtering by matching bag of words and Scoring
word.match <- function(sentences,list.words){
  
  scores<-laply(sentences,function(sentence,list.words){

    word.list<-str_split(sentence,'\\s+')
    words<-unlist(word.list)
    pos.matches<-match(words,list.words)
    
    pos.matches<-!is.na(pos.matches)
    score<-sum(pos.matches)
    return(score)
  },list.words)
  scores.df<-data.frame(score=scores,text=sentences)
  return(scores.df)
}

BOW <- read.csv("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/Comments/BOW.csv")
#Remove all columns which are NAs
BOW <- BOW[,colSums(is.na(BOW))<nrow(BOW)]
table(is.na(BOW))

docs_Seller <- Corpus(VectorSource(BOW$BOW_Sellers))
docs_Consumer <- Corpus(VectorSource(BOW$BOW_Consumers))
docs_Edu_awar <- Corpus(VectorSource(BOW$BOW_Educ_Awar))

docs_Seller <-tm_map(docs_Seller,content_transformer(tolower))
docs_Consumer <-tm_map(docs_Consumer,content_transformer(tolower))
docs_Edu_awar <-tm_map(docs_Edu_awar,content_transformer(tolower))

docs_Seller <- tm_map(docs_Seller,stemDocument, language = "english")
docs_Consumer <- tm_map(docs_Consumer,stemDocument, language = "english")
docs_Edu_awar <- tm_map(docs_Edu_awar,stemDocument, language = "english")

docs_Seller_df <- data.frame(text=sapply(docs_Seller, identity),stringsAsFactors=F)
docs_Consumer_df <- data.frame(text=sapply(docs_Consumer, identity),stringsAsFactors=F)
docs_Edu_awar_df <- data.frame(text=sapply(docs_Edu_awar, identity),stringsAsFactors=F)
StemmedBOW <- NULL
StemmedBOW$Seller <- docs_Seller_df
StemmedBOW$Consumer <- docs_Consumer_df
StemmedBOW$Edu_awar <- docs_Edu_awar_df

StemmedBOW <- as.data.frame(StemmedBOW)

str(StemmedBOW)

colnames(StemmedBOW) <- c("Seller","Consumer","Edu_awar")

test.score.seller<- word.match(afterstem$Stemmed_Text,StemmedBOW$Seller)
colnames(test.score.seller) <- c("Seller_Score","Text")

test.score.consumer<- word.match(afterstem$Stemmed_Text,StemmedBOW$Consumer)
colnames(test.score.consumer) <- c("Consumer_Score","Text")

test.score.edu_awar<- word.match(afterstem$Stemmed_Text,StemmedBOW$Edu_awar)
colnames(test.score.edu_awar) <- c("Edu_awar_Score","Text")

textwithScores <- cbind(test.score.seller,test.score.consumer,test.score.edu_awar) #166401 observations

#Removing NA Observations
textwithScores <- textwithScores[!(textwithScores$Text==""),] #154815 observations

dupval <- textwithScores[duplicated(textwithScores$Text),]
#Dropping additional Text Columns
textwithScores <- textwithScores[,-c(4,6)] 
#Reordering Columns
textwithScores <- textwithScores[c(2,4,1,3)]
#converting variables
textwithScores$Text <- as.character(textwithScores$Text)
textwithScores$Edu_awar_Score <- as.numeric(textwithScores$Edu_awar_Score)
textwithScores$Seller_Score <- as.numeric(textwithScores$Seller_Score)
textwithScores$Consumer_Score <- as.numeric(textwithScores$Consumer_Score)

textwithScoresgrt0 <- textwithScores[textwithScores$Seller_Score >0 | textwithScores$Consumer_Score>0 | textwithScores$Edu_awar_Score>0,]
#47112 observations
#mapping stemmed text with original text
colnames(afterstem)[which(names(afterstem) == "Text")] <- "Original_Text"
colnames(afterstem)[which(names(afterstem) == "Stemmed_Text")] <- "Text"

relevant_original_posts <- merge(x = afterstem, y = textwithScoresgrt0, by = "Text", all.y =TRUE)

relevant_original_posts_unique <- relevant_original_posts[!duplicated(relevant_original_posts$Original_Text),]#46272

write.csv(relevant_original_posts_unique,"relevant_original_posts_unique.csv")

rel_cols <- c("Original_Text", "Type")
relevant_text_only <- relevant_original_posts_unique[rel_cols]

table(duplicated(relevant_text_only$Original_Text))

relevant_text_only_unique <- relevant_text_only[!duplicated(relevant_text_only$Original_Text),]

write.csv(relevant_text_only_unique,"relevant_text_only_unique.csv")

str(textwithScores)
str(textwithScoresgrt0)

textwithScores0 <- textwithScores[(textwithScores$Edu_awar_Score==0 & textwithScores$Seller_Score==0 &
                                   textwithScores$Consumer_Score==0),]
##107703 observations

textwithScores0$type <- "Others"

textwithScores0$type <- as.factor(textwithScores0$type)
str(textwithScores0)

write.csv(textwithScores,"textwithScores.csv")
write.csv(textwithScoresgrt0,"textwithScoresgrt0.csv")
write.csv(textwithScores0,"textwithScores0.csv")

textwithScoresgrt0$type <- NULL

only_scores <- textwithScoresgrt0[,-1]

only_scores$type <- colnames(only_scores)[max.col(only_scores,ties.method="first")]

textwithScoresgrt0$type <- only_scores$type

str(textwithScoresgrt0)

textwithScoresgrt0$type <- ifelse(textwithScoresgrt0$type == "Consumer_Score", "Consumer", ifelse(textwithScoresgrt0$type == "Seller_Score", "Seller","Edu_awar"))

textwithScoresgrt0$type <- as.factor(textwithScoresgrt0$type)

str(textwithScoresgrt0)

textwithclass <- rbind(textwithScores0,textwithScoresgrt0)#151288

write.csv(textwithScoresgrt0,"textwithScores_and_classification.csv") #47112

t1<-table(textwithclass$type)
t1
pts1<-prop.table(t1)
pts1<-pts1*100 # Convert to percentages 
barplot(pts1, main = "Bar Plot", xlab = "Tu=ype", ylab = "Proportion", col=c("orange", "steelblue"), ylim=c(0,70))
box()

#Removing Duplicates
textwithclass_unique <- textwithclass[!duplicated(textwithclass$Text),]#143476
table(textwithclass_unique$type)
write.csv(textwithclass_unique,"textwithclass_unique.csv")

All_Text_after_removing_others <- textwithclass_unique[textwithclass_unique$type!="Others",]#46088
write.csv(All_Text_after_removing_others,"All_Text_after_removing_others.csv")

sellertext <- textwithclass_unique[(textwithclass_unique$type=="Seller"),]
sellertext <- sellertext[order(-sellertext$Seller_Score),] 

Edu_awar_text <- textwithclass_unique[(textwithclass_unique$type=="Edu_awar"),]
Edu_awar_text <- Edu_awar_text[order(-Edu_awar_text$Edu_awar_Score),] 

Edu_awar_textonly <- Edu_awar_text$Text

write.csv(Edu_awar_textonly,"Edu_awar_textonly.csv")

Consumer_text <- textwithclass_unique[(textwithclass_unique$type=="Consumer"),]
Consumer_text <- Consumer_text[order(-Consumer_text$Consumer_Score),] 

Consumer_textonly <- Consumer_text$Text

write.csv(Consumer_textonly,"Consumer_textonly.csv")

#Reading the manually classified csv file 
ForGoogleAutoML <- read_excel("ForGoogleAutoML.xlsx")

forGml <- ForGoogleAutoML

forGml$Label <- tolower(forGml$Label)

forGml <- forGml[forGml$Label!="Others",]

forGml <- forGml[forGml$Label!="others",]

forGml <- forGml[forGml$Label!='NA',]

table(is.na(forGml$Label))

forGml <- forGml[!is.na(forGml$Label),]

str(forGml)
forGml$Label <- as.factor(forGml$Label)
str(forGml)

table(forGml$Label)
#converting labels to lower case
forGml$Label <- tolower(forGml$Label)

t1<-table(forGml$Label)
t1
barplot(t1, main = "Bar Plot", xlab = "Label", ylab = "Frequency")
pts1<-prop.table(t1)
pts1<-pts1*100 # Convert to percentages 
barplot(pts1, main = "Bar Plot", xlab = "Label", ylab = "Proportion", col=c("orange", "steelblue"), ylim=c(0,60))
box()

forGml$Label <- as.factor(forGml$Label)
str(forGml$Label)

#Clubbin all the labels to 5 major categories
levels(forGml$Label)[levels(forGml$Label)=="busted - book"] <- "busted"
levels(forGml$Label)[levels(forGml$Label)=="consumer - recovery experience"] <- "consumer recovery experience"
levels(forGml$Label)[levels(forGml$Label)=="consumer - rehab experience"] <- "consumer rehab experience"
levels(forGml$Label)[levels(forGml$Label)=="consumer"] <- "consumers"

levels(forGml$Label)[levels(forGml$Label)=="consumer recovery experience"] <- "experience sharing"
levels(forGml$Label)[levels(forGml$Label)=="consumer rehab experience"] <- "experience sharing"
levels(forGml$Label)[levels(forGml$Label)=="consumer experience"] <- "experience sharing"
levels(forGml$Label)[levels(forGml$Label)=="customer experience"] <- "experience sharing"
levels(forGml$Label)[levels(forGml$Label)=="experience"] <- "experience sharing"
levels(forGml$Label)[levels(forGml$Label)=="experience /consumer"] <- "experience sharing"

#Comblining awareness, news and busted together as news/awareness
levels(forGml$Label)[levels(forGml$Label)=="awareness"] <- "news/awareness"
levels(forGml$Label)[levels(forGml$Label)=="news"] <- "news/awareness"
levels(forGml$Label)[levels(forGml$Label)=="news/consumer"] <- "news/awareness"
levels(forGml$Label)[levels(forGml$Label)=="busted"] <- "news/awareness"


#combining all seller posts together
levels(forGml$Label)[levels(forGml$Label)=="seller"] <- "selling"
levels(forGml$Label)[levels(forGml$Label)=="cbd seller"] <- "selling"
levels(forGml$Label)[levels(forGml$Label)=="seller - tool"] <- "selling"
levels(forGml$Label)[levels(forGml$Label)=="weed/seller"] <- "selling"
levels(forGml$Label)[levels(forGml$Label)=="smoke/seller"] <- "selling"

#combining all consumer posts together
levels(forGml$Label)[levels(forGml$Label)=="smoke - consumers"] <- "consumers"

#combining all alternative/rehab, anti-opioids together as treatment options
levels(forGml$Label)[levels(forGml$Label)=="treatment"] <- "treatment options"
levels(forGml$Label)[levels(forGml$Label)=="treatment/rehab"] <- "treatment options"
levels(forGml$Label)[levels(forGml$Label)=="alternative"] <- "treatment options"
levels(forGml$Label)[levels(forGml$Label)=="anti-opioid"] <- "treatment options"

table(forGml$Label)

forGml$X__1 <- NULL

t1<-table(forGml$Label)
t1
barplot(t1, main = "Bar Plot", xlab = "Label", ylab = "Frequency")
pts1<-prop.table(t1)
pts1<-pts1*100 # Convert to percentages 
barplot(pts1, main = "Bar Plot", xlab = "Label", ylab = "Proportion", col=c("orange", "steelblue"), ylim=c(0,60))
box()

str(forGml)

write.csv(forGml,"ManuallyClassifiedTrainingData.csv")

library(readxl)
toTrain_GoogleAutoML_355Obs <- read_excel("toTrain_GoogleAutoML_355Obs.xlsx")

first_tbatch <- toTrain_GoogleAutoML_355Obs

first_tbatch$...1 <- NULL
first_tbatch$X__1 <- NULL
forGml$...1 <- NULL

str(first_tbatch)
str(forGml)
colnames(first_tbatch)[which(names(first_tbatch) == "X__2")] <- "Post_Link"
colnames(forGml)[which(names(forGml) == "...5")] <- "Post_Link"

second_batch <- rbind(first_tbatch,forGml)


table(duplicated(second_batch$Original_Text))

second_batch_unique <- second_batch[!duplicated(second_batch$Original_Text), ]

table(second_batch_unique$Label)

str(second_batch_unique)

second_batch_unique$Label <- as.factor(second_batch_unique$Label)

#merging Treatment options to news/awareness
levels(second_batch_unique$Label)[levels(second_batch_unique$Label)=="treatment options"] <- "news/awareness"

table(second_batch_unique$Label)

write.csv(second_batch_unique,"WholeTrainingData_forTraining_GoogleAutoML.csv")

#Sampling 150 consumer observations out of 252
second_batch_unique_consumers <- second_batch_unique[second_batch_unique$Label=="consumers",]

set.seed(123)
index <- sample(1:nrow(second_batch_unique_consumers), 150)
index

second_batch_unique150_consumers  <- second_batch_unique_consumers[index, ]

second_batch_unique_nonconsumers <- second_batch_unique[!(second_batch_unique$Label=="consumers"),]


final_secondbatch_AutoML_541 <- rbind(second_batch_unique_nonconsumers,second_batch_unique150_consumers)

table(final_secondbatch_AutoML_541$Label)

write.csv(final_secondbatch_AutoML_541,"FinalTrainingData_forTraining_GoogleAutoML.csv")

#Seller Analysis
library(readxl)
Seller_final_secondbatch_AutoML_541 <- read_excel("Seller_final_secondbatch_AutoML_541.xlsx", 
                                                   sheet = "Sellers_only")
Sellers_only <- Seller_final_secondbatch_AutoML_541

str(Sellers_only)
Sellers_only$...1 <- NULL

#Removing Misclassified Records
Sellers_only <- Sellers_only[Sellers_only$Sub_Category_1!="misclassification",]
Sellers_only <- Sellers_only[Sellers_only$Sub_Category_2!="misclassification",]
cols <- c("type", "Label", "Sub_Category_1", "Sub_Category_2")
Sellers_only[cols] <- lapply(Sellers_only[cols], factor)     
str(Sellers_only)

table(Sellers_only$Sub_Category_1)
table(Sellers_only$Sub_Category_2)

t1<-table(Sellers_only$Sub_Category_1)
t1
barplot(t1, main = "Bar Plot", xlab = "Sub_Category_1", ylab = "Frequency")
pts1<-prop.table(t1)
pts1<-pts1*100 # Convert to percentages 
barplot(pts1, main = "Bar Plot", xlab = "Sub_Category_1", ylab = "Proportion", col=c("orange", "steelblue"), ylim=c(0,95))
box()


t1<-table(Sellers_only$Sub_Category_2)
t1
barplot(t1, main = "Bar Plot", xlab = "Sub_Category_2", ylab = "Frequency")
pts1<-prop.table(t1)
pts1<-pts1*100 # Convert to percentages 
barplot(pts1, main = "Bar Plot", xlab = "Sub_Category_2", ylab = "Proportion", col=c("orange", "steelblue","blue","yellow","purple","green"), ylim=c(0,35))
box()

#Analysis of Marijuana Group
Sellers_marijuana_only <- Sellers_only[Sellers_only$Sub_Category_1=="marijuana",]
str(Sellers_marijuana_only)

Sellers_marijuana_only$Sub_Category_2 <- as.character(Sellers_marijuana_only$Sub_Category_2)
Sellers_marijuana_only$Sub_Category_2 <- as.factor(Sellers_marijuana_only$Sub_Category_2)

t1<-table(Sellers_marijuana_only$Sub_Category_2)
t1
barplot(t1, main = "Bar Plot", xlab = "Sub_Category_2", ylab = "Frequency")
pts1<-prop.table(t1)
pts1<-pts1*100 # Convert to percentages 
barplot(pts1, main = "Bar Plot", xlab = "Sub_Category_2", ylab = "Proportion", col=c("orange", "steelblue","blue","yellow","purple","green"), ylim=c(0,35))
box()


#Analysis of Opioid Group
Sellers_opioid_only <- Sellers_only[Sellers_only$Sub_Category_1=="opioid",]
str(Sellers_opioid_only)
Sellers_opioid_only$Sub_Category_2 <- as.character(Sellers_opioid_only$Sub_Category_2)
Sellers_opioid_only$Sub_Category_2 <- as.factor(Sellers_opioid_only$Sub_Category_2)

t1<-table(Sellers_opioid_only$Sub_Category_2)
t1
barplot(t1, main = "Bar Plot", xlab = "Sub_Category_2", ylab = "Frequency")
pts1<-prop.table(t1)
pts1<-pts1*100 # Convert to percentages 
barplot(pts1, main = "Bar Plot", xlab = "Sub_Category_2", ylab = "Proportion", col=c("orange","blue","yellow","purple","green"), ylim=c(0,95))
box()
str(apc)
apc_unique <- apc[!duplicated(apc$postUrl), ]

str(apc_unique)
str(Sellers_only)
class(apc_unique)
class(Sellers_only)
Sellers_only <- as.data.frame(Sellers_only)
apc_unique$postUrl <- as.character(apc_unique$postUrl)
write.csv(apc_unique,"apc_unique.csv")

seller_profiles <- merge(x = Sellers_only, y = apc_unique, by = "postUrl", all.x = TRUE)

#Merging Seller Profile Details with Seller Profile Post classification Details
Seller_final_secondbatch_AutoML_541 <- read_excel("Seller_final_secondbatch_AutoML_541.xlsx", sheet = "Sellers_only")

seller_post_classifications <- Seller_final_secondbatch_AutoML_541

Seller_profiles_API <- read_excel("Seller_profiles_after_extraction.xlsx")

seller_profile_all_details <- merge(x = seller_post_classifications, y = Seller_profiles_API, by = "profileUrl", all.x = TRUE)

#Opioid Seller Profiles
seller_profile_opioid <- seller_profile_all_details[seller_profile_all_details$Sub_Category_1=="opioid",]

write.csv(seller_profile_opioid,"seller_profile_opioid.csv")

t1<-table(seller_profile_opioid$Sub_Category_2)
t1
barplot(t1, main = "Bar Plot", xlab = "Sub_Category_2", ylab = "Frequency")
pts1<-prop.table(t1)
pts1<-pts1*100 # Convert to percentages 
barplot(pts1, main = "Bar Plot", xlab = "Sub_Category_2", ylab = "Proportion", col=c("orange","blue","yellow","purple","green"), ylim=c(0,95))
box()

#Filtering Unique Opioid Sellers
seller_profile_opioid_unique <- seller_profile_opioid[!duplicated(seller_profile_opioid$profileName), ]

write.csv(seller_profile_opioid_unique,"seller_profile_opioid_unique.csv")
#We add the profile creation date to the above csv file manually using the tool: https://123accs.com/instagram-account-age-checker/
#marijuana Seller Profiles
seller_profile_marijuana <- seller_profile_all_details[seller_profile_all_details$Sub_Category_1=="marijuana",]
write.csv(seller_profile_marijuana,"seller_profile_marijuana.csv")
str(seller_profile_marijuana)
seller_profile_marijuana
cols <- c("type", "Label", "Sub_Category_1", "Sub_Category_2")
Sellers_only[cols] <- lapply(Sellers_only[cols], factor) 

#Analysis of Experience Sharing Bucket
ExpSharing_final_secondbatch_AutoML_541 <- read_excel("C:/Vignesh/Studies/Spring 19/Healthcare Analytics/Project/Comments/Seller Profile Screenshots/ExpSharing_final_secondbatch_AutoML_541.xlsx", sheet = "Experience_Sharing")

exp_shar <-ExpSharing_final_secondbatch_AutoML_541
str(exp_shar)

exp_shar$...1 <- NULL
exp_shar$...7 <- NULL

cols <- c("type", "Label", "Subclassification")
exp_shar[cols] <- lapply(exp_shar[cols], factor)    
table(exp_shar$Subclassification)

levels(exp_shar$Subclassification)[levels(exp_shar$Subclassification)=="Drug Addiction"] <- "Opioid Addiction"
levels(exp_shar$Subclassification)[levels(exp_shar$Subclassification)=="Experience"] <- "Others"
levels(exp_shar$Subclassification)[levels(exp_shar$Subclassification)=="others"] <- "Others"
levels(exp_shar$Subclassification)[levels(exp_shar$Subclassification)=="Cannabis"] <- "Weed/Cannabis Experience"
levels(exp_shar$Subclassification)[levels(exp_shar$Subclassification)=="Treatment"] <- "Addiction Recovery"
levels(exp_shar$Subclassification)[levels(exp_shar$Subclassification)=="Injury/pain"] <- "Injury/Pain"
levels(exp_shar$Subclassification)[levels(exp_shar$Subclassification)=="Pain/Anxiety"] <- "Injury/Pain"

str(exp_shar$Subclassification)

write.csv(exp_shar,"experience_sharing_subcategories.csv")
