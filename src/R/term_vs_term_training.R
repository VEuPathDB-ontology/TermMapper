options(java.parameters = "-Xmx8G")

# run this in an directory that also contains the target.ontology.file

# library(tidyverse)
library(dplyr)
library(rrdf)
# I also use rdflib and SPARQL in some cases

library(igraph)
library(solrium)
library(stringdist)
library(reshape2)
# library(randomForest)
library(e1071)
library(caret)

target.ontology.file <- "clinEpi_merged.owl"
target.ontology.format <- "RDF/XML"

# how large should each Solr post be?
first.row <- 1
chunk.size <- 1000

# make sure this core or collection already exists
# this script does not try to create it
selected.core <- "clinEpi"
# what strings should we load into Solr?
# only taking one for now
solr.pred <- "http://purl.obolibrary.org/obo/EUPATH_0001003"
clear.solr <- TRUE

# what fraction of the Solr results should be used to train 
train.frac <- 0.70

target.onto <-
  load.rdf(filename = target.ontology.file, format = target.ontology.format)

# don't worry if you see this warning when an rrdf function is first used
#  although it might be good to set up logging?
#  I don't know how to do that

# log4j:WARN No appenders could be found for logger (org.apache.jena.riot.RDFLanguages).
# log4j:WARN Please initialize the log4j system properly.
# log4j:WARN See http://logging.apache.org/log4j/1.2/faq.html#noconfig for more info.

#### I familiarize myself with the new Ontology, starting with predicates

find.literal.preds.q <- '
select
?p ?pl (count(?s) as ?count)
where {
	?s ?p ?lit .
    filter(isiri(?s))
    filter(isliteral(?lit))
    optional{
        ?p <http://www.w3.org/2000/01/rdf-schema#label> ?pl
    }
}
group by ?p ?pl
'

find.literal.preds.res <-
  sparql.rdf(model = target.onto, sparql = find.literal.preds.q)

find.literal.preds.res <- as.data.frame(find.literal.preds.res)
find.literal.preds.res$count <-
  as.numeric(find.literal.preds.res$count)

####

find.iri.preds.q <- '
select
?p ?pl (count(?s) as ?count)
where {
	?s ?p ?o .
    filter(isiri(?s))
    filter(isiri(?o))
    optional{
        ?p <http://www.w3.org/2000/01/rdf-schema#label> ?pl
    }
}
group by ?p ?pl
'

find.iri.preds.res <-
  sparql.rdf(model = target.onto, sparql = find.iri.preds.q)

find.iri.preds.res <- as.data.frame(find.iri.preds.res)
find.iri.preds.res$count <-
  as.numeric(find.iri.preds.res$count)

####

# what kinds of values does each predicate take?
# this creates a list of lists of dataframes

summarize.pred <- function(pred) {
  print(pred)
  label.q <- paste0(
    '
  select
  distinct
  ?s (lcase(str(?l)) as ?label)
  where {
  values ?s { <',
    pred ,
    '>}
  ?s <http://www.w3.org/2000/01/rdf-schema#label> ?l }
'
  )
  label.res <- sparql.rdf(model = target.onto, sparql = label.q)
  label.res <- as.data.frame(label.res)
  label.res <- label.res$label
  
  main.q <- paste0('
select
distinct
*
where {
  ?s <', pred , '> ?o .
}')
  
  main.res <-
    sparql.rdf(model = target.onto, sparql = main.q)
  
  main.res <- as.data.frame(main.res)
  
  print(head(main.res))
  
  values.per.subj <- table(main.res$s)
  values.per.subj <-
    cbind.data.frame(names(values.per.subj),
                     as.numeric(values.per.subj))
  names(values.per.subj) <-
    c("subject", "value.count")
  
  print(head(values.per.subj))
  
  subjs.per.value <- table(main.res$o)
  subjs.per.value <-
    cbind.data.frame(names(subjs.per.value),
                     as.numeric(subjs.per.value))
  names(subjs.per.value) <- c("value", "subject.count")
  
  print(head(subjs.per.value))
  
  return(
    list(
      pred.label = label.res,
      pred.frame = main.res,
      values.per.subj = values.per.subj,
      subjs.per.value = subjs.per.value
    )
  )
}

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


# ####
#
# replacements predicate...
# # IRIs as strings?
# # CURIes as strings!?
#
####

# how long are the definitions?

def.chars.q <- '
select * where {
  ?s <http://purl.obolibrary.org/obo/IAO_0000115> ?o .
  bind(strlen(?o) as ?defchars)
}'

def.chars.res <-
  sparql.rdf(model = target.onto, sparql = def.chars.q)

def.chars.res <- as.data.frame(def.chars.res)
def.chars.res$defchars <- as.numeric(def.chars.res$defchars)

####

# http://purl.obolibrary.org/obo/EUPATH_0000274 has two rdfs:labels:
# order and displayOrder

# are the objects of these triples numeric?

order.q <- '
select * where {
  ?s <http://purl.obolibrary.org/obo/EUPATH_0000274> ?o .
}'

order.res <-
  sparql.rdf(model = target.onto, sparql = order.q)

order.res <- as.data.frame(order.res)
order.res$o <- as.numeric(order.res$o)

# were any of the order values non-numeric before conversion?
table(order.res$o < 1)
table(is.na(order.res$o))

# ####

# the "things with orders" look interesting
# what type are they?

types.with.codes.q <- '
select
distinct
?s ?t
where {
  ?s a ?t ;
      <http://purl.obolibrary.org/obo/EUPATH_0001004> ?o .
}'

types.with.codes.res <-
  sparql.rdf(model = target.onto, sparql = types.with.codes.q)

types.with.codes.res <- as.data.frame(types.with.codes.res)

term.type.table <- table(types.with.codes.res$s)
term.type.table <-
  cbind.data.frame(names(term.type.table),
                   as.numeric(term.type.table))
names(term.type.table) <-
  c("term", "type.count")


type.term.table <- table(types.with.codes.res$t)
type.term.table <-
  cbind.data.frame(names(type.term.table),
                   as.numeric(type.term.table))
names(type.term.table) <-
  c("type", "term.count")

# in this environment, each term appears as all three:
# http://www.w3.org/2000/01/rdf-schema#Class
# http://www.w3.org/2000/01/rdf-schema#Resource
# http://www.w3.org/2002/07/owl#Class

# but I believe they are only asserted as owl:Classes in the ontology

# ####

blank.subjects.q <- '
select * where {
  ?s ?p ?o .
  filter(isblank(?o))
}'

blank.subjects.res <-
  sparql.rdf(model = target.onto, sparql = blank.subjects.q)

blank.subjects.res <- as.data.frame(blank.subjects.res)

# ####

blank.objects.q <- '
select * where {
  ?s ?p ?o .
  filter(isblank(?o))
}'

blank.objects.res <-
  sparql.rdf(model = target.onto, sparql = blank.objects.q)

blank.objects.res <- as.data.frame(blank.objects.res)

# there are no blank subjects or objects, so no OWL Axioms, Restrictions, etc.
# why not just call this an RDF file?

# ####

superclasses.q <- '
select
distinct
?sub ?super
where {
  ?sub <http://www.w3.org/2000/01/rdf-schema#subClassOf>+ ?super .
}'

superclasses.res <-
  sparql.rdf(model = target.onto, sparql = superclasses.q)

superclasses.res <- as.data.frame(superclasses.res)

superclasses.table <- table(superclasses.res$super)
superclasses.table <-
  cbind.data.frame(names(superclasses.table),
                   as.numeric(superclasses.table))
names(superclasses.table) <- c("superclass", "subclass.count")

superclasses.table <-
  superclasses.table[superclasses.table$subclass.count > 1 , ]

real.supers <- superclasses.table$superclass
real.supers <-
  superclasses.res[superclasses.res$sub %in% real.supers |
                     superclasses.res$super %in% real.supers ,]
real.supers <- real.supers[, c(2, 1)]

# use igraph to find the number of subClassOf hops
#   between each entity

real.supers.graph <-
  graph_from_data_frame(real.supers, directed = FALSE)

real.supers.dist.tab <- distances(real.supers.graph)
real.supers.dist.tab <- as.data.frame(real.supers.dist.tab)

colnames(real.supers.dist.tab) <- V(real.supers.graph)$name
real.supers.dist.tab$from <- V(real.supers.graph)$name

# print(real.supers.dist.tab[1:3, 1:3])

real.supers.dist.tab <-
  melt(data = real.supers.dist.tab, id.vars = "from")
real.supers.dist.tab$variable <-
  as.character(real.supers.dist.tab$variable)

# ####

# connect: by default we connect to localhost, port 8983
cli <- SolrClient$new()

# NOT using SolrCloud (I'm to too lazy to set it up?)
# can check if core exists, but can't create from here?
print(cores(cli))

if(clear.solr) {
  solrium::delete_by_query(conn = cli, query = "*:*", name = selected.core)
}

####

frame.to.post <-
  literal.pred.breakdowns[[solr.pred]]$pred.frame

frame.to.post$searchable <- gsub(pattern = '[[:punct:]]',
                                 replacement = '',
                                 x = frame.to.post$o)


frame.to.post$searchable <- tolower(frame.to.post$searchable)


frame.to.post <- frame.to.post[order(frame.to.post$searchable), ]

frame.to.post <-
  frame.to.post[nchar(frame.to.post$searchable) > 0 ,]

print(nrow(frame.to.post))

row.num.chunks <- 1:nrow(frame.to.post)

chunk.count <- ceiling(nrow(frame.to.post) / chunk.size)

#####

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

multi.search <- do.call(rbind.data.frame, multi.search)

# add the distances between nodes back in
# we'll use this distance as the ML target
multi.search <-
  left_join(
    x = multi.search,
    y = real.supers.dist.tab,
    by = c("id" = "from", "query.iri" = "variable")
  )

# add more features for machine learning
# one example: qgram string distance between
#   each Solr query and match
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

# # It's silly to use a random forest when there's only three training features!
# rf.model <- randomForest(value ~ .,
#                          data = train)

svm.model <- svm(value ~ ., data = train)

pred <- predict(svm.model, test)

# we should determine sensitivity, specificity and area under the ROC curve for a classification
# different quality measures would be used for a regression... r-squared?
print(caret::confusionMatrix(pred, test$value))
