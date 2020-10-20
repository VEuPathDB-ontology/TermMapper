# Author: Jie Zheng
# Date: Sept 20th, 2020
# ----------------------------------------

####
# set memory size
# ----------------------------------------

options(java.parameters = "-Xmx4G")

library(rdflib)
library(igraph)
library(reshape2)
library(solrium)
library(dplyr)
library(stringdist)
library(e1071)
library(caret)

####
# document path
# ----------------------------------------

path.example <- "./src/examples/"

####
# test mapping file, all clinEpi terms
# ----------------------------------------

file.map <- "mapping/clinEpi_noClassification.csv"

mapping.res <- read.csv(file = paste0(path.example, file.map))

mapping.res <- cbind.data.frame(mapping.res$entity, mapping.res$label, mapping.res$variable, mapping.res$codebook_description, mapping.res$codebook_values)

names(mapping.res) <- c("IRI","label","variable","codebookDescription","codebookValues")

print(nrow(mapping.res)) # 6019 rows
print(length(mapping.res$variable[nchar(mapping.res$variable)>0])) # 5453 rows with not blank variable
print(length(mapping.res$codebookDescription[nchar(mapping.res$codebookDescription)>0])) # 4886 rows with not blank codebook description
print(length(mapping.res$codebookValues[nchar(mapping.res$codebookValues)>0])) # 3777 rows with not blank codebook values
print(length(mapping.res$codebookValues[nchar(mapping.res$codebookValues)>1])) # 3668 rows with codebook values is longer than 1 character

# clean punctuation
mapping.res$variable_clean <- gsub(pattern = '[[:punct:]]',
                                 replacement = ' ',
                                 x = mapping.res$variable) 


mapping.res$codebookDescription_clean <- gsub(pattern = '[[:punct:]]',
                                 replacement = ' ',
                                 x = mapping.res$codebookDescription) 

mapping.res$codebookValues_clean <- gsub(pattern = '[[:punct:]]',
                                 replacement = ' ',
                                 x = mapping.res$codebookValues) 


#========================================================================================================
# SubClasses relations in the ontology
#    Retrieve all superclasses for all classes in the clinEpi terminologies, exclude GEMS1a
# ----------------------------------------

file.superclasses <- "subClasses/clinEpi_noClassification_subClasses.csv"

superclasses.res <- read.csv(file = paste0(path.example, file.superclasses))
# subClasses axioms [subClass ($sub), superClass ($super)], 24318 rows

superclasses.table <- table(superclasses.res$super)

superclasses.table <- cbind.data.frame(names(superclasses.table), as.numeric(superclasses.table))

names(superclasses.table) <- c("superclass", "subclass.count")

superclasses.table <- superclasses.table[superclasses.table$subclass.count > 1 , ]

real.supers <- superclasses.table$superclass

real.supers <- superclasses.res[superclasses.res$sub %in% real.supers | superclasses.res$super %in% real.supers ,]

real.supers <- real.supers[, c(2, 1)]


####
# use igraph to find the number of subClassOf hops
#   between each entity (shortest path between two classes)
# ----------------------------------------

real.supers.graph <- graph_from_data_frame(real.supers, directed = FALSE)

real.supers.dist.tab <- distances(real.supers.graph)
real.supers.dist.tab <- as.data.frame(real.supers.dist.tab)

colnames(real.supers.dist.tab) <- V(real.supers.graph)$name
real.supers.dist.tab$from <- V(real.supers.graph)$name

real.supers.dist.tab <- melt(data = real.supers.dist.tab, id.vars = "from")

real.supers.dist.tab$variable <- as.character(real.supers.dist.tab$variable)


#========================================================================================================
# map all clinEpi variables to clinEpi terminologies
# 	using Solr (start as cloud mode)
# ----------------------------------------

###
# read csv file and add the collection which contains all clinEpi terms to solr server
# ----------------------------------------
# 

# read collection csv file
solr.collection.res <- read.csv(file=paste0(path.example, "solr_collection/solr_collection_clinEpi_noClassification.csv"), head=TRUE, sep=",")

print(names(solr.collection.res))

# clean punctuation
solr.collection.res$variable_clean <- gsub(pattern = '[[:punct:]]',
                                 replacement = ' ',
                                 x = solr.collection.res$variable) 

solr.collection.res$codebook_description_clean <- gsub(pattern = '[[:punct:]]',
                                 replacement = ' ',
                                 x = solr.collection.res$codebook_description) 

solr.collection.res$definition_clean <- gsub(pattern = '[[:punct:]]',
                                 replacement = ' ',
                                 x = solr.collection.res$definition) 

solr.collection.res$codebook_description_clean <- gsub(pattern = '[[:punct:]]',
                                 replacement = ' ',
                                 x = solr.collection.res$codebook_description) 

solr.collection.res$codebook_values_clean <- gsub(pattern = '[[:punct:]]',
                                 replacement = ' ',
                                 x = solr.collection.res$codebook_values) 


###
# load collection to solr
# ----------------------------------------

selected.core <- "clinEpi"

cli <- SolrClient$new()

if (!cli$collection_exists(selected.core)) {
	cli$collection_create(name = selected.core)
}
add(solr.collection.res, cli, selected.core)


###
# Solr mapping
# ----------------------------------------

# Go through 'variable' or 'codebook description', which is searchable column, in each row, and search 'variable' or 'codebook description' 
# in Solr term label, return id and mapped searchable value, also copy over value for search, as query.label and query.iri


# ----------------------------------------
# map based on 'variable' to label
# ----------------------------------------

multi.search.var <-
  apply(
    X = mapping.res,
    MARGIN = 1,
    FUN = function(current.search) {
      search.field <- current.search[['variable_clean']]

      if (nchar(search.field) > 0) {
        search.query <- paste0("label:(", search.field, ")")
        # print (search.query)

        solr.res <-
          solrium::solr_search(
            conn = cli,
            name = selected.core,
            params = list(q = search.query,
                        fl = 'entity,label,score')
         )

        if (nrow(solr.res) > 0) {
          solr.res$rank <- 1:nrow(solr.res)
          solr.res$query.iri <- current.search[['IRI']]
          solr.res$query.label <- current.search[['label']]
          solr.res$query.variable <- current.search[['variable']]
          solr.res$searchable <- search.field
          return(solr.res)
        }
      }
      # print(solr.res)
    }
  )

multi.search.var <- do.call(rbind.data.frame, multi.search.var)



# ----------------------------------------
# map based on 'codebook description' to label
# ----------------------------------------

multi.search.des <-
  apply(
    X = mapping.res,
    MARGIN = 1,
    FUN = function(current.search) {
      search.field <- current.search[['codebookDescription_clean']]

      if (nchar(search.field) > 0) {
        search.query <- paste0("label:(", search.field, ")")
        #print (search.query)

        solr.res <-
          solrium::solr_search(
            conn = cli,
            name = selected.core,
            params = list(q = search.query,
                        fl = 'entity,label,score')
          )

        if (nrow(solr.res) > 0) {
          solr.res$rank <- 1:nrow(solr.res)
          solr.res$query.iri <- current.search[['IRI']]
          solr.res$query.label <- current.search[['label']]
          solr.res$query.variable <- current.search[['variable']]
          solr.res$searchable <- search.field
          return(solr.res)
        }
        # print(solr.res)
      }
    }
  )

multi.search.des <- do.call(rbind.data.frame, multi.search.des)


# Notes: there is limitation of search string, cannot run when combine variable + codeDescription

###
# Machine learning
# ----------------------------------------

# ----------------------------------------
# ML on mapping based on 'variable' to label
# ----------------------------------------

# add the distances between nodes back in, use this distance as the ML target

multi.search.var <-
  left_join(
    x = multi.search.var,
    y = real.supers.dist.tab,
    by = c("entity" = "from", "query.iri" = "variable")
  )


# add more features for machine learning
# one example: calculate string distance between each Solr query and match

cosine.dist <-
  apply(
    X = multi.search.var,
    MARGIN = 1,
    FUN = function(current.row) {
      temp <-
        stringdist(a = current.row[['searchable']], b = current.row[['query.label']], method = 'cosine')
      return(temp)
    }
  )

multi.search.var$cosine.dist <- cosine.dist

# write the mapping results with class node and cosine distances
write.csv(multi.search.var, paste0(path.example, "outputs/multi.search.variable2label.csv"), row.names=FALSE)

# extract just the numeric predictors and target
#  some ML algorithms can learn from categorical features, too
# solr matching score, rank, cosine distance and class distance 
trainable <- multi.search.var[, c("score", "rank", "cosine.dist", "value")]

# do this as a classification
trainable$value <- factor(trainable$value, ordered = TRUE)

set.seed(42)

# split training and testing data
train.frac <- 0.70

sample <-
  sample.int(
    n = nrow(trainable),
    size = floor(train.frac * nrow(trainable)),
    replace = F
  )
train <- trainable[sample,]
test  <- trainable[-sample,]

# svm classification

svm.model <- svm(value ~ ., data = train)

pred <- predict(svm.model, test)

# Summary information of classification including sensitivity, specificity, etc.
summary.multi.search.var <- caret::confusionMatrix(pred, test$value)
print(summary.multi.search.var)

# ----------------------------------------
# ML on mapping based on 'codebook description' to label
# ----------------------------------------

# add the distances between nodes back in, use this distance as the ML target

multi.search.des <-
  left_join(
    x = multi.search.des,
    y = real.supers.dist.tab,
    by = c("entity" = "from", "query.iri" = "variable")
  )


# add more features for machine learning
# one example: calculate string distance between each Solr query and match

cosine.dist <-
  apply(
    X = multi.search.des,
    MARGIN = 1,
    FUN = function(current.row) {
      temp <-
        stringdist(a = current.row[['searchable']], b = current.row[['query.label']], method = 'cosine')
      return(temp)
    }
  )

multi.search.des$cosine.dist <- cosine.dist

# write the mapping results with class node and cosine distances
write.csv(multi.search.des, paste0(path.example, "outputs/multi.search.des2label.csv"), row.names=FALSE)

# extract just the numeric predictors and target
#  some ML algorithms can learn from categorical features, too
# solr matching score, rank, cosine distance and class distance 
trainable <- multi.search.des[, c("score", "rank", "cosine.dist", "value")]

# do this as a classification
trainable$value <- factor(trainable$value, ordered = TRUE)

set.seed(42)

# split training and testing data
train.frac <- 0.70

sample <-
  sample.int(
    n = nrow(trainable),
    size = floor(train.frac * nrow(trainable)),
    replace = F
  )
train <- trainable[sample,]
test  <- trainable[-sample,]

# svm classification

svm.model <- svm(value ~ ., data = train)

pred <- predict(svm.model, test)

# Summary information of classification including sensitivity, specificity, etc.
summary.multi.search.des <- caret::confusionMatrix(pred, test$value)
print(summary.multi.search.des)

