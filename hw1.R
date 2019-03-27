# 1. First we’ll use the data from the U.S. inaugural addresses available in quanteda.corpora.
#Let’s first look at the inaugural addresses given by Ronald Reagan in 1981 and 1985.
#(a) Calculate the TTR of each of these speeches and report your findings.
#(b) Create a document feature matrix of the two speeches, 
# with no pre-processing other than to remove the punctuation–be 
# sure to check the options on “dfm” in R as appropriate. 
# Calculate the cosine distance between the two documents with quanteda. Report your findings.

# setting up the environment
rm(list = ls())
setwd('/Users/xiaoguihua/R/text_as_data') #set work directory

#load up  libraries
library(quanteda)
library(quanteda.corpora)

#glance of the data set
summary(data_corpus_inaugural)
#head(docvars(data_corpus_inaugural), 10)

#select two speeches from the speech list
reagan = corpus_subset(data_corpus_inaugural, Year == 1981 | Year == 1985)
t_reagan = quanteda::tokens(reagan, remove_punct = TRUE)


#''' 1. a) Calculate TTR and report'''
TTR_reagan <- textstat_lexdiv(t_reagan,measure="TTR")
TTR_reagan

#1. b) document feature matrix, only remove the punctuation, check the options on “dfm”, calculate cosine dis'''
dfm_reagan <- dfm(reagan, tolower=FALSE)
reagan_simi <- textstat_simil(dfm_reagan, method='cosine')
as.matrix(reagan_simi)

#2. Consider different preprocessing choices you could make. 
#For each of the following parts of this question, you have three tasks: 
#(i), make a theoretical argument for how it should affect the TTR of each document 
#and the similarity of the two documents, and 
#(ii), re-do question (1a) with the preprocessing option indicated
#(iii) redo question(1b) with the preprocessing option indicated.

#(a) Stemming the words?
#(b) Removing stop words?
#(c) Converting all words to lowercase?
#(d) Does tf-idf weighting make sense here? Explain why or why not.

#(a) Stemming the words?
#a.(i) comment:
#a.(ii)
dfm_reagan_stem <- dfm(reagan, tolower=FALSE,stem = TRUE)
dfm_reagan_stem_TTR <- textstat_lexdiv(dfm_reagan_stem,measure="TTR")
dfm_reagan_stem_TTR
#a.(iii)
reagan_stem_simi <- textstat_simil(dfm_reagan_stem, method='cosine')
as.matrix(reagan_stem_simi)

#(b) Removing stop words?
#b.(i) comment
#b.(ii)
dfm_reagan_no_stopword <- dfm(reagan, tolower=FALSE,remove=stopwords("english"))
dfm_reagan_no_stopword_TTR <- textstat_lexdiv(dfm_reagan_no_stopword,measure="TTR")
dfm_reagan_no_stopword_TTR
#b.(iii)
dfm_reagan_no_stopword_simi <- textstat_simil(dfm_reagan_no_stopword, method='cosine')
as.matrix(dfm_reagan_no_stopword_simi)

#(c)Converting all words to lowercase?
#c.(i) comment
#c.(ii)
dfm_reagan_lower <- dfm(reagan, tolower=TRUE)
dfm_reagan_lower_TTR <- textstat_lexdiv(dfm_reagan_lower,measure="TTR")
dfm_reagan_lower_TTR
#c.(iii)
dfm_reagan_lower_simi <- textstat_simil(dfm_reagan_lower, method='cosine')
as.matrix(dfm_reagan_lower_simi)

#(d) Does tf-idf weighting make sense here? Explain why or why not.


# 3. Calculate the MTLD of each of the two speeches by RR, with the TTR limit set at .72. 
# Rather than covering the entire speech, you can find the Mean Lengths starting from 25 
# different places in each speech, as long as there is no overlap between the snippets.

# Hint: If you get stuck on this problem, examine the documentation for the library koRpus.

#install.packages('koRpus.lang.en')
library(koRpus.lang.en) # loading the package
#reagan[1] #you can exam the data here
token_reagan_1981 <- koRpus::tokenize(reagan[1],doc_id="sample",lang="en",format = "obj") #tokenize 1981 speech
token_reagan_1985 <- koRpus::tokenize(reagan[2],doc_id="sample",lang="en",format = "obj") #tokenize 1985 speech
MTLD(token_reagan_1981, factor.size = 0.72, min.tokens = 25) #calculate MTLD
MTLD(token_reagan_1985, factor.size = 0.72, min.tokens = 25) #calculate MTLD

#4. Take the following two headlines:
#  "Trump Says He’s ‘Not Happy’ With Border Deal, but Doesn’t Say if He Will Sign It."
#"Trump ‘not happy’ with border deal, weighing options for building wall."
# (a) Calculate the Euclidean distance between these sentences by hand—that is, you can use base R, 
# but you can’t use functions from quanteda or similar. 
# Use whatever pre- processing of the text you want, but justify your choice. Report your findings.

# (b) Calculate the Manhattan distance between these sentences by hand. Report your find- ings.
# (c) Calculate the cosine similarity between these sentences by hand. Report your findings.

sen_1 <- "Trump Says He’s ‘Not Happy’ With Border Deal, but Doesn’t Say if He Will Sign It."
sen_2 <- "Trump ‘not happy’ with border deal, weighing options for building wall."

dfm_sen_1 <- dfm(sen_1, tolower=FALSE)
dfm_sen_2 <- dfm(sen_2, tolower=FALSE)
#length(dfm_sen_2)
#length(dfm_sen_1)
zeros = c(0,0,0,0,0)
v_sen_1 = as.vector(dfm_sen_1)
v_sen_2 = append(as.vector(dfm_sen_2),zeros)
v_sen_2
norm_vec <- function(x) sqrt(sum(x^2))
calculate_dist <- function(vec1, vec2) { 
  euc_dist = sqrt(sum((vec1 - vec2)^2))
  man_dist = sum(abs(vec1 - vec2))
  nominator <- vec1%*% vec2  
  # %*% specifies dot product rather than entry by entry multiplication (we could also do: sum(x * y))
  norm_v1 = norm_vec(vec1)
  norm_v2 = norm_vec(vec2)
  denominator <- norm_v1 * norm_v2
  cos_dist = nominator/denominator
  return(list(Euclidean = euc_dist, Manhattan = man_dist,cosine=cos_dist))
}

calculate_dist(v_sen_1,v_sen_2)


# 5. You will be using Leslie Huang’s (a PhD student at NYU’s CDS) stylest package. 
#To get the texts for this exercise you will need the gutenbergr package.

#install.packages('stylest')
#install.packages('gutenbergr')
#load the packages
library(stylest)
library(gutenbergr)
# a.b) download novels and extract a short excerpt
# first four works of : Austen, Jane; Twain, Mark;Joyce, James;Dickens, Charles
#get all the book reference number in gutenbergr
all_book_ref = c()
for (i in c("Austen, Jane","Twain, Mark","Joyce, James","Dickens, Charles")){
   new = gutenberg_works(author == i)[1:4,1]
   all_book_ref = rbind(all_book_ref,new)
}

#remove the useless information: i.e.column name
book_ref = c()
for (i in all_book_ref){book_ref <- rbind(book_ref,i)}

#download the book, select 500 lines from each of them, combine all the books together
books = data.frame()
for (ref in book_ref){
  book <- gutenberg_download(ref,meta_fields = c('title','author'))
  lines <- book[10:510,]
  books <- rbind(books,lines)
}

# remove the special characters that is non-utf-8
df <- data.frame(apply(books, 2, function(x) {x <- gsub("\xa0", "", x)}))


# c) use the stylest_select_vocab function to select the terms you will include in your model.
# What percentile (of term frequency) has the best prediction rate? 
# Also report the rate of incorrectly predicted speakers of held-out texts.

# data pre-processing and using stylest_select_vocab
library("corpus")
filter <- corpus::text_filter(drop_punct = TRUE, drop_number = TRUE)
set.seed(2019L)
vocab_custom <- stylest_select_vocab(df$text, df$author,  # fits n-fold cross-validation
                                     filter = filter, smooth = 1, nfold = 10,cutoff_pcts = c(25, 50, 75, 99))

vocab_custom$cutoff_pct_best # percentile with best prediction rate
vocab_custom$miss_pct  # rate of incorrectly predicted speakers of held-out texts

# d)Use your optimal percentile from above to subset the terms to be included in your model 
# (this requires you use the stylest terms function). 
# Now go ahead and fit the model using stylest fit. 
# The output of this function includes information on the rate 
# at which each author uses each term (the value is labeled rate). 
# Report the top 5 terms most used by each author. Do these terms make sense?

# make the subset of the data using best cutoff percentile
vocab_subset <- stylest_terms(df$text, df$author, 
                              vocab_custom$cutoff_pct_best , filter = filter)
# fit the model
style_model <- stylest_fit(df$text, df$author, terms = vocab_subset, filter = filter)

#get the top 5 terms for each author
authors <- unique(df$author)
term_usage <- style_model$rate
term_usage
lapply(authors, function(x) head(term_usage[x,][order(-term_usage[x,][1:5])])) %>% setNames(authors)

# e) Choose any two authors, take the ratio of their frequency vectors (make sure dimensions are in the same order) 
# and arrange the resulting vector from largest to smallest values. 
# What are the top 5 terms according to this ratio? How would you interpret this ordering?
author_compare <- authors[1:2]


# f) Load the mystery excerpt provided. According to your fitted model, who is the most likely author?




# 6. For this question we will use the sophistication package discussed in the lab.
# The corpus for this exercise will be the U.S. inaugural addresses.
# (a) Using the aforementioned corpus make snippets between 150 to 350 characters 
# in length and clean the snippets (print the top 10).
# (b) Randomly sample 1000 snippets and use these to generate pairs for a minimum 
# spanning tree. From these generate 10 gold pairs (print these —only each pair of text— in your HW). 
# Without looking at the automated classification, 
# read each pair and select whichever you think is “easiest” to read. 
# Now compare your classification with those made by the package. 
# What proportion of the ten gold pairs were you in agreement with the automated classification? 
# Any reasons why you may have arrived at a different judgment?

#devtools::install_github("kbenoit/sophistication")
library("sophistication")

# a) make snippets and clean them, print the top10
snippetData <- snippets_make(data_corpus_inaugural, nsentence = 1, minchar = 150, maxchar = 350)
# clean up the snippets
snippetData <- snippets_clean(snippetData)
# print head 10
head(snippetData,10)

# b) 1000 random sample,generate pairs for a minimum spanning tree; make 10 gold pairs
set.seed(2019)
pairData <- snippetData[sample(1:nrow(snippetData), 1000), ]
snippetPairsMST <- pairs_regular_make(pairData)

#gold pairs
gold_pair <- pairs_gold_make(snippetPairsMST, n.pairs = 10)
#get the texts
gold_pair$text1
gold_pair$text2

#get the analysis result
gold_pair

# 7. use 4217 and 74 to make a graph to demonstrate Zipf's Law
#Include this graph and also discuss any pre-processing decisions you made.
JJ_4217 = gutenberg_download(4217)$text
TM_74 = gutenberg_download(74)$text

dfm_JJ_4217 <- dfm(JJ_4217, tolower=FALSE,remove=stopwords("english"))
plot(log10(1:100), log10(topfeatures(dfm_JJ_4217, 100)),
     xlab = "log10(rank)", ylab = "log10(frequency)", main = "4217-zipf's law")

#make the regression reference line
regression <- lm(log10(topfeatures(dfm_JJ_4217, 100)) ~ log10(1:100))
abline(regression, col = "red")
confint(regression)
#summary(regression)

# add the TM_74 data
dfm_TM_74 <- dfm(TM_74, tolower=FALSE,remove=stopwords("english"))
points(log10(1:100), log10(topfeatures(dfm_TM_74, 100)),
     xlab = "log10(rank)", ylab = "log10(frequency)", main = "74-zipf's law",col = 'blue')


# 8. Find the value of b that best fit the two novels from the previous question to Heap’s law, 
# fixing k = 44. Report the value of b as well as any pre-processing decisions you made.
k <- 44
M_JJ <- nfeat(dfm_JJ_4217) # number of features = number of types of JJ
M_TM <- nfeat(dfm_TM_74) # number of features = number of types of TM
Tee_JJ <- sum(lengths(TM_74)) #text size of JJ
Tee_TM <- sum(lengths(JJ_4217)) # text size of TM

# M = k*(Tee)^b, apply log to both sides of the equation
# logM = b*log(k*(Tee)) => b = logM / log(k*(Tee))
# calculate b in lines below
b_JJ <- log(M_JJ/k,Tee_JJ)
b_TM <- log(M_TM/k,Tee_TM)

#report the result
print (paste0("the value of b for 4217 is ",b_JJ))
print (paste0("the value of b for 74 is ",b_TM))

# 9.Both James Joyce’s “A Portrait of the Artist as a Young Man” and 
# Mark Twain’s “The Adventures of Tom Sawyer” broach the topic of religion, 
# but in very different ways. Choose a few Key Words in Context and discuss 
# the different context in which those words are used by each author. 
# Give a brief discussion of how the two novels treat this theme differently.

#help(kwic)
head(kwic(JJ_4217,pattern = "relig*",window=5,valuetype = "glob"))
head(kwic(TM_74,pattern = "relig*",window=5,valuetype = "glob"))


# 10. Consider the bootstrapping of the texts we used to calculate 
# the standard errors of the Flesch reading scores of Irish budget speeches in Recitation 4.

# a) Obtain the UK Conservative Party’s manifestos from quanteda.corpora. 
# Generate estimates of the FRE scores of these manifestos over time, 
# using sentence-level bootstraps instead of the speech-level bootstraps used in Recitation 4. 
# Include a graph of these estimates.

#library(quanteda.corpora)
#head(docvars(data_corpus_ukmanifestos), 10) # have a glance of data

#select conservative party ones
conserv = corpus_subset(data_corpus_ukmanifestos, Party == 'Con')
conserv_sentence = corpus_reshape(conserv, to = "sentences")
head(docvars(conserv), 10) # see the selected data

# convert corpus to df 
conserv_df <- conserv_sentence$documents %>% dplyr::select(texts,Party, Year)
#%>% mutate(Year = as.integer(Year))
# Let's filter out any NAs
conserv_df <- na.omit(conserv_df)

# FRE
flesch_conserv <- conserv_df$texts %>% textstat_readability(measure = "Flesch") %>% 
  dplyr::group_by(conserv_df$Year) %>% summary(mean_flesch = mean(Flesch)) %>% setNames("mean") 
flesch_conserv

# b) Report the means of the bootstrapped results and the means observed in the data. Discuss the contrast.
boot_flesch <- function(ydata){
  N <- nrow(ydata)
  bootstrap_sample <- sample_n(ydata, N, replace = TRUE)
  readability_results <- textstat_readability(bootstrap_sample$texts, measure = "Flesch")
  return(mean(readability_results$Flesch))
}


# apply function to each year
library(pbapply)
iters <- 10
Years = unique(conserv_df$Year)

boot_flesch_by_year <- pbapply(Years, function(x){
  sub_data <- conserv_df[conserv_df$Year == x,]
  output_flesch <- lapply(1:iters, function(i) boot_flesch(sub_data))
  return(unlist(output_flesch))
  #return(sub_data)
})
names(boot_flesch_by_year) <- Years



# compute mean and std.errors
party_means <- lapply(boot_flesch_by_year, mean) %>% unname() %>% unlist()
party_ses <- lapply(boot_flesch_by_year, sd) %>% unname() %>% unlist() # bootstrap standard error = sample standard deviation bootstrap distribution


# c) For the empirical values of each text, calculate the FRE score and the Dale-Chall score. 
# Report the FRE and Dale-Chall scores and the correlation between them.

all_readability_measures <- textstat_readability(conserv, c("Flesch", "Dale.Chall"))

readability_matrix <- cbind(all_readability_measures$Flesch, all_readability_measures$Dale.Chall)

readability_cor <- cor(readability_matrix)
rownames(readability_cor) <- c("Flesch", "Dale-Chall")
colnames(readability_cor) <- c("Flesch", "Dale-Chall")
readability_cor




