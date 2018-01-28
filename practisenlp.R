## natural langaugae processing

### loading data

datasets = read.delim("Restaurant_Reviews.tsv",quote = '',stringsAsFactors = FALSE)
View(datasets)


library(tm)
library(SnowballC)

corpus = VCorpus(VectorSource(datasets$Review))
corpus = tm_map(corpus, content_transformer(tolower))

# to chgeck the above function we will check it by seeing first row to
## do that we need to use beloiw functon
# as.character(corpus[[1]])

## below function remove numbers numbers in each review for example check 841 row which contains number 40 in it is removed

corpus = tm_map(corpus, removeNumbers)

corpus = tm_map(corpus, removePunctuation)
corpus
corpus = tm_map(corpus, removeWords,stopwords())

## remove words will remove words where as stopwords is a PARAMETER which will 

## remove all articles and prepositions like this that etc... stopwords is a prt of snowballc pacakage


corpus = tm_map(corpus, stemDocument)

## stemdocumnet parametere helps you to find out root word of a particular word

## for example loved is the word in the first row it has changed to love
## beacuse root word for loved is love

## stipwhite space parameter  will remove white spaces in each and every row

## for eaxmple we have removed all articles after removing it that place is

## filled with a space that will be an extra space between words so now by using this parameter

## we can remove that extra space

corpus = tm_map(corpus,stripWhitespace)


### now the task is to create a bag of words 

dtm = DocumentTermMatrix(corpus)
View(dtm)


dtm = removeSparseTerms(dtm,0.999)
View(dtm)

## creating a data frame for the above sparse matrix

dataset = as.data.frame(as.matrix(dtm))
View(dataset)

## now adding dependent variable(liked) which is there is original datset
## so that machine learning algirthm can understand independent and dependent variabe
dataset$Liked = datasets$Liked
dataset
str(dataset)

## Now work on machine learning model  randomforest

## first encode the dependent variabe as a factor

dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

## now splitting the datset into training and testing

library(caTools)

split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)
cm
prop.table(cm)

