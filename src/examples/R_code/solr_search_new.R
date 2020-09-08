# Author: Jie Zheng
# Date: Aug 6th, 2020
# ----------------------------------------

####
# set memory size
# ----------------------------------------

options(java.parameters = "-Xmx4G")


####
# document path
# ----------------------------------------

path.example <- "/Users/jiezheng/Documents/VEuPathDB-git/TermMapper_local/examples/"
path.dataDictionary <- "/Users/jiezheng/Documents/VEuPathDB-git/TermMapper_local/examples/dataDictionary/"
path.ontology <- "/Users/jiezheng/Documents/VEuPathDB-git/TermMapper_local/examples/ontology/"
path.predicts <- "/Users/jiezheng/Documents/VEuPathDB-git/TermMapper_local/examples/predicts/"
path.solr_collection <- "/Users/jiezheng/Documents/VEuPathDB-git/TermMapper_local/examples/solr_collection/"
path.subClasses <- "/Users/jiezheng/Documents/VEuPathDB-git/TermMapper_local/examples/subClasses/"


####
# document filename for testing, first use case gems1a
# ----------------------------------------

test.var <- "gems"

#========================================================================================================
# SubClasses relations in the ontology
#    Retrieve all superclasses for all classes in the ontology
# ----------------------------------------

superclasses.filename <- paste0("clinEpi_", test.var, "_subClasses.csv")

####
# usage of annotation properties in the ontology
#    Count of annotation property usages in the ontology
# 	Using ROBOT tool rather than library(rdflib) 
# ----------------------------------------

predict.filename <- paste0("clinEpi_", test.var, "_predict_literal.csv")

####
# subClasses calculation
# ----------------------------------------

# subClasses axioms [subClass ($sub), superClass ($super)]
superclasses.res <- read.csv(file = paste0(path.subClasses, superclasses.filename))

# get contingency table using $super as factor, [group terms based on $supper, so list number of subClasses for a superclass]
superclasses.table <- table(superclasses.res$super)

# change array to data.frame, the first column is table names and the second column is array of the table
superclasses.table <- cbind.data.frame(names(superclasses.table), as.numeric(superclasses.table))

# set column names of data.frame
names(superclasses.table) <- c("superclass", "subclass.count")

# exclude the superclass which only have one child
superclasses.table <- superclasses.table[superclasses.table$subclass.count > 1 , ]

# keep the classes that have more than one child, and call it as real superclasses 
real.supers <- superclasses.table$superclass

# remove the axioms which are a superclass has only one child, such as EUPATH_0011800
real.supers <- superclasses.res[superclasses.res$sub %in% real.supers | superclasses.res$super %in% real.supers ,]

# move the last column to first -> swab column 2 with 1, now first column is super, second is sub.
real.supers <- real.supers[, c(2, 1)]


####
# use igraph to find the number of subClassOf hops
#   between each entity (shortest path between two classes)
# ----------------------------------------

library(igraph)

# load data into graph, all edges, no vertices information
real.supers.graph <- graph_from_data_frame(real.supers, directed = FALSE)

# calculate the shortest path
real.supers.dist.tab <- distances(real.supers.graph)
real.supers.dist.tab <- as.data.frame(real.supers.dist.tab)

colnames(real.supers.dist.tab) <- V(real.supers.graph)$name
real.supers.dist.tab$from <- V(real.supers.graph)$name

# print(real.supers.dist.tab[1:3, 1:3])

####
# use reshape2 melt function 
# 	convert the format in class A, class B, and their distances
# ----------------------------------------

library(reshape2)

# wide to long conversion, stack the data as variable and value with id.vars not stacked
real.supers.dist.tab <- melt(data = real.supers.dist.tab, id.vars = "from")

# change to text type (as.character)
real.supers.dist.tab$variable <- as.character(real.supers.dist.tab$variable)


#========================================================================================================
# load ontology
# ----------------------------------------

library(rdflib)

# set ontology filename and format
target.ontology.file <- paste0(path.ontology, "clinEpi_", test.var, "_merged.owl")
target.ontology.format <- "rdfxml"

# load ontology file
target.onto <- rdf_parse(target.ontology.file, format = target.ontology.format)


#========================================================================================================
# annotation properties, including IRI, label, values for each ontology term, and frequency based on both ontology term or annotation value
# ----------------------------------------

##########################
# function, get summary information of a given predicate
##########################
summarize.pred <- function(pred) {
  print(pred)

  # -----------------
  # retrieve label of entity, pred
  # -----------------
  label.q <- paste0('
  select distinct ?s (lcase(str(?l)) as ?label)
  where {
  values ?s { <', pred , '>}
  ?s <http://www.w3.org/2000/01/rdf-schema#label> ?l }'
  )

  label.res <- rdf_query(target.onto, label.q)
  label.res <- as.data.frame(label.res)
  label.res <- label.res$label
  
  # -----------------
  # get distinct values of entity, pred
  # -----------------
  main.q <- paste0('
  select distinct *
  where {
    ?s <', pred , '> ?o . }'
  )
  
  main.res <- rdf_query(target.onto, main.q)
  main.res <- as.data.frame(main.res)
  print(head(main.res))
  
  # ontology term entity and the frequency of a value for a given property associated with it (all are 1)
  values.per.subj <- table(main.res$s)
  values.per.subj <- cbind.data.frame(names(values.per.subj), as.numeric(values.per.subj))
  names(values.per.subj) <- c("subject", "value.count")  
  print(head(values.per.subj)) 
  # print(values.per.subj[values.per.subj$value.count>1,])    #### most are 1

  # value of a given predict and the frequency of subject entity associate with the value 
  subjs.per.value <- table(main.res$o)
  subjs.per.value <- cbind.data.frame(names(subjs.per.value), as.numeric(subjs.per.value))
  names(subjs.per.value) <- c("value", "subject.count") 
  print(head(subjs.per.value))
  # print(subjs.per.value[subjs.per.value$subject.count>1,])  #### most are 1
  
  return(
    list(
      pred.label = label.res,
      pred.frame = main.res,
      values.per.subj = values.per.subj,
      subjs.per.value = subjs.per.value
    )
  )
}

####
# literal predict (annotation property) usage
# ----------------------------------------

predicts.filename <- paste0("clinEpi_", test.var, "_predict_literal.csv")
find.literal.preds.res <- read.csv(file = paste0(path.predicts, predicts.filename))
find.literal.preds.res <- as.data.frame(find.literal.preds.res)
find.literal.preds.res$count <- as.numeric(find.literal.preds.res$count)

unique.preds <- sort(unique(find.literal.preds.res$p))

literal.pred.breakdowns <-
  lapply(
    X = unique.preds,
    FUN = function(current.pred) {
      print(current.pred)
      temp <-
        summarize.pred(current.pred)
      return(temp)
    }
  )

names(literal.pred.breakdowns) <- unique.preds

#========================================================================================================
# Only consider Machine learning to one annotation property
##### what strings should we load into Solr?
# only taking one for now, EUPATH_0001003 'codebook description'
# ----------------------------------------

solr.pred <- "http://purl.obolibrary.org/obo/EUPATH_0001003"

# Based on names, retrieve different element from a list ($pred.label, $pred.frame, $values.per.subj, $subjs.per.value)
# Get the subject and object with given predict (codebook description, term with its codebook description values)
frame.to.post <- literal.pred.breakdowns[[solr.pred]]$pred.frame

# gsub(search_term, replacement_term, string_searched, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
# remove the punctuation in 'codebook description' and saved in a new column,$searchable
frame.to.post$searchable <- gsub(pattern = '[[:punct:]]',
                                 replacement = '',
                                 x = frame.to.post$o)

frame.to.post$searchable <- tolower(frame.to.post$searchable)

frame.to.post <- frame.to.post[order(frame.to.post$searchable), ]

frame.to.post <- frame.to.post[nchar(frame.to.post$searchable) > 0 ,]

print(nrow(frame.to.post))


#========================================================================================================
# Solr need to start as cloud mode
# 	bin/solr start -e cloud
# 	http://localhost:8983/solr/

# Some useful commands after connection
# delete collection
# 	collection_delete(cli, name = "clinEpiOntology")
#
# list available collections
# 	collection_list(cli)

library(solrium)

##################################################
# Solr client connection and collection generation
##################################################

####
# connect: by default we connect to localhost, port 8983
# ----------------------------------------

cli <- SolrClient$new()

########################################################################################
# add "gems" collection, all gems without gems1a
# ----------------------------------------
# read collection csv file
x <- read.csv(file=paste0(path.solr_collection, "solr_collection_gems.csv"), head=TRUE, sep=",")

# create collection "gems"
if (!cli$collection_exists("gems")) {
	cli$collection_create(name = "gems")
}
#   cli$collection_create(name = "clinEpiOntology", numShards = 2, router.field = "entity")
#	cli$collection_reload(name = "clinEpiOntology")

# add document from file to solr collection
add(x, cli, "gems")

####
# add "noGems1a" collection, all clinEpi without gems1a
# ----------------------------------------
# read collection csv file
x <- read.csv(file=paste0(path.solr_collection, "solr_collection_noGems1a.csv"), head=TRUE, sep=",")

if (!cli$collection_exists("noGems1a")) {
	cli$collection_create(name = "noGems1a")
}
add(x, cli, "noGems1a")

# This part load the ontology including all annotation available in the ontology
########################################################################################


# ----------------------------------------
# how large should each Solr post be?
first.row <- 1
chunk.size <- 100 #change from 1000 to 100

# what strings should we load into Solr?
# only taking one for now 'codebook description'
clear.solr <- TRUE
selected.core <- "clinEpi" 

# what fraction of the Solr results should be used to train 
train.frac <- 0.70

#---- This part does not work yet
if(clear.solr) {
  solrium::delete_by_query(conn = cli, query = "*:*", name = selected.core)
}

if(clear.solr) {
  cli$delete_by_query(query = "*:*", selected.core)
}
#---- 

#----
# load searchable information in the Solr, only contain IRI and codebook description
row.num.chunks <- 1:nrow(frame.to.post)
chunk.count <- ceiling(nrow(frame.to.post) / chunk.size)

selected.core <- "gems"

for (current.rownum in first.row:chunk.count)
{
  # print(current.rownum)
  inner.first <-  ((current.rownum - 1) * chunk.size) + 1
  inner.last <- (current.rownum * chunk.size)
  # print(inner.first)
  print(paste(inner.first, inner.last))
  temp <- as.data.frame(frame.to.post[inner.first:inner.last,])
  temp <- temp[, c("s", "searchable")]
  colnames(temp) <- c("id", "searchable")
  rownames(temp) <- NULL
  print(head(temp))
  add(temp, cli, selected.core)
}

# Go through 'codebook description', which is searchable column, in each row, and search 'codebook description' what in Solr
# return id and mapped searchable value, also copy over value for search, as query.label and query.iri
multi.search <-
  apply(
    X = frame.to.post,
    MARGIN = 1,
    FUN = function(current.search) {
      # print(current.search)
      cs.s <- current.search[['s']]
      cs.o <- current.search[['searchable']]
      print(cs.o)
      solr.res <-
        solrium::solr_search(
          conn = cli,
          name = selected.core,
          params = list(q = paste0('searchable:(', cs.o, ')'),
                        fl = 'id,searchable,score')
        )
      if (nrow(solr.res) > 0) {
        solr.res$rank <- 1:nrow(solr.res)
        solr.res$query.iri <- cs.s
        solr.res$query.label <- cs.o
        return(solr.res)
      }
      # print(solr.res)
    }
  )

# list to data.frame
multi.search <- do.call(rbind.data.frame, multi.search)

library(dplyr)

# add the distances between nodes back in
# we'll use this distance as the ML target
multi.search <-
  left_join(
    x = multi.search,
    y = real.supers.dist.tab,
    by = c("id" = "from", "query.iri" = "variable")
  )


library(stringdist)

# add more features for machine learning
# one example: qgram string distance between each Solr query and match, using cosine distance
# can easily be made "more parallel"
cosine.dist <-
  apply(
    X = multi.search,
    MARGIN = 1,
    FUN = function(current.row) {
      temp <-
        stringdist(a = current.row[['searchable']], b = current.row[['query.label']], method = 'cosine')
      return(temp)
    }
  )

multi.search$cosine.dist <- cosine.dist

# extract just the numeric predictors and target
#  some ML algorithms can learn from categorical features, too
# solr matching score, rank, cosine distance and class distance 
trainable <-
  multi.search[, c("score", "rank", "cosine.dist", "value")]

# do this as a classification, not a regression?
trainable$value <- factor(trainable$value, ordered = TRUE)

set.seed(42)

# split training and testing data
sample <-
  sample.int(
    n = nrow(trainable),
    size = floor(train.frac * nrow(trainable)),
    replace = F
  )
train <- trainable[sample,]
test  <- trainable[-sample,]


# will work on it later, since we can add more feature in
# # It's silly to use a random forest when there's only three training features!
# library(randomForest)
# rf.model <- randomForest(value ~ .,
#                          data = train)

# svm classification
library(e1071)

svm.model <- svm(value ~ ., data = train)

pred <- predict(svm.model, test)



# we should determine sensitivity, specificity and area under the ROC curve for a classification
# different quality measures would be used for a regression... r-squared?
library(caret)
print(caret::confusionMatrix(pred, test$value))

