options(java.parameters = "-Xmx4G")

library(tidytext)
library(dplyr)

path.example <- "/Users/jiezheng/Documents/VEuPathDB-git/TermMapper/src/examples/"

# get all terms and associated annotations used in clinEpi projects
clinEpi.corpus <- read.csv(file = paste0(path.example, "solr_collection/solr_collection_clinEpi_noClassification.csv"))

####
# count number of not blank cells for each field
# ----------------------------------------
corpus.info.notNull <- apply(clinEpi.corpus, 2, function(current.column) {
	length(current.column[nchar(current.column)>0])
	}
 )

corpus.info.names <- names(corpus.info.notNull)

corpus.info.notNull <- as.data.frame(rbind(names(corpus.info.notNull), corpus.info.notNull))

corpus.info.notNull <- as.data.frame(t(corpus.info.notNull))

names(corpus.info.notNull) <- c("Field", "Count")

write.csv(corpus.info.notNull, paste0(path.example, "outputs/clinEpi.corpus.notNull.count.csv"), row.names=FALSE)


####
# combine label, definition, variable, codebook_description, and codebook_values in a new column 'text'
# ----------------------------------------

clinEpi.corpus$allText <- paste(clinEpi.corpus$label, clinEpi.corpus$definition, clinEpi.corpus$variable, clinEpi.corpus$codebook_description, clinEpi.corpus$codebook_values)
 
####
# count the words used in different fields using predefined 'word' token, and saved in a csv file, it won't keep '-'
# ----------------------------------------

# label
clinEpi.corpus %>% unnest_tokens(word, label) %>% count(word, sort = TRUE) %>% write.csv(paste0(path.example, "outputs/clinEpi.words.label.word.csv"), row.names=FALSE) 

# definition
clinEpi.corpus %>% unnest_tokens(word, definition) %>% count(word, sort = TRUE) %>% write.csv(paste0(path.example, "outputs/clinEpi.words.definition.word.csv"), row.names=FALSE) 

# variable
clinEpi.corpus %>% unnest_tokens(word, variable) %>% count(word, sort = TRUE) %>% write.csv(paste0(path.example, "outputs/clinEpi.words.variable.word.csv"), row.names=FALSE) 

# codebook_description
clinEpi.corpus %>% unnest_tokens(word, codebook_description) %>% count(word, sort = TRUE) %>% write.csv(paste0(path.example, "outputs/clinEpi.words.codebook_description.word.csv"), row.names=FALSE) 

# codebook_values
clinEpi.corpus %>% unnest_tokens(word, codebook_values) %>% count(word, sort = TRUE) %>% write.csv(paste0(path.example, "outputs/clinEpi.words.codebook_values.word.csv"), row.names=FALSE) 


# all related text
clinEpi.words <- clinEpi.corpus %>% unnest_tokens(word, allText) %>% count(word, sort = TRUE)
write.csv(clinEpi.words, paste0(path.example, "outputs/clinEpi.words.allText.word.csv"), row.names=FALSE) 

clinEpi.words <- clinEpi.words[nchar(clinEpi.words$word)>1,]
write.csv(clinEpi.words, paste0(path.example, "outputs/clinEpi.words.allText.word_cleaned.csv"), row.names=FALSE) 


####
# count the words used in 'label' field, keep '-', so, we have words, like z-score
# ----------------------------------------

# get subset of clinEpi.corpus, only keep those we are interested in
clinEpi.text <- data.frame(clinEpi.corpus$label, clinEpi.corpus$definition, clinEpi.corpus$variable, clinEpi.corpus$codebook_description, clinEpi.corpus$codebook_values)

names(clinEpi.text) <- c("label", "definition", "variable", "codebook_description", "codebook_values")

# remove '(', ')'
clinEpi.text <- apply(clinEpi.text, 2, function(current.column) {
	current.column <- gsub(pattern = '[\\(\\)]', replacement = '', x = current.column)
	}
 )
clinEpi.text <- as.data.frame(clinEpi.text)
names(clinEpi.text) <- c("label", "definition", "variable", "codebook_description", "codebook_values")

# count the words used in the label using custom specified token, and saved in a csv file, keep '-'
clinEpi.text %>% unnest_tokens(word, label, token = "regex", pattern = "[[\\s\\;\\,]]") %>% count(word, sort = TRUE) %>% write.csv(paste0(path.example, "outputs/clinEpi.words.label.pattern.csv"), row.names=FALSE) 


####
# calculation of tf-idf, treat each ontology term as a document
# ----------------------------------------

# -----
# label
# -----
clinEpi.corpus.label <- data.frame(clinEpi.corpus$entity, clinEpi.corpus$label)

# exclude rows containing blank cell
clinEpi.corpus.label <- clinEpi.corpus.label[nchar(clinEpi.corpus.label[,2]) > 0,]
names(clinEpi.corpus.label) <- c("entity", "text")

label.tf_idf <- clinEpi.corpus.label %>% unnest_tokens(word, text) %>% count(entity, word, sort = TRUE) %>% bind_tf_idf(word, entity, n) %>% arrange(desc(tf_idf))

write.csv(label.tf_idf, paste0(path.example, "outputs/clinEpi.tf_idf.label.csv"), row.names=FALSE) 


# -----
# definition
# -----
clinEpi.corpus.definition <- data.frame(clinEpi.corpus$entity, clinEpi.corpus$definition)

# exclude rows containing blank cell
clinEpi.corpus.definition <- clinEpi.corpus.label[nchar(clinEpi.corpus.definition[,2]) > 0,]
names(clinEpi.corpus.definition) <- c("entity", "text")

definition.tf_idf <- clinEpi.corpus.definition %>% unnest_tokens(word, text) %>% count(entity, word, sort = TRUE) %>% bind_tf_idf(word, entity, n) %>% arrange(desc(tf_idf))

write.csv(definition.tf_idf, paste0(path.example, "outputs/clinEpi.tf_idf.definition.csv"), row.names=FALSE) 


# -----
# variable
# -----
clinEpi.corpus.variable <- data.frame(clinEpi.corpus$entity, clinEpi.corpus$variable)

# exclude rows containing blank cell
clinEpi.corpus.variable <- clinEpi.corpus.variable[nchar(clinEpi.corpus.variable[,2]) > 0,]
names(clinEpi.corpus.variable) <- c("entity", "text")

variable.tf_idf <- clinEpi.corpus.variable %>% unnest_tokens(word, text) %>% count(entity, word, sort = TRUE) %>% bind_tf_idf(word, entity, n) %>% arrange(desc(tf_idf))

write.csv(variable.tf_idf, paste0(path.example, "outputs/clinEpi.tf_idf.variable.csv"), row.names=FALSE) 


# -----
# codebook_description
# -----
clinEpi.corpus.codebook_description <- data.frame(clinEpi.corpus$entity, clinEpi.corpus$codebook_description)

# exclude rows containing blank cell
clinEpi.corpus.codebook_description <- clinEpi.corpus.codebook_description[nchar(clinEpi.corpus.codebook_description[,2]) > 0,]
names(clinEpi.corpus.codebook_description) <- c("entity", "text")

codebook_description.tf_idf <- clinEpi.corpus.codebook_description %>% unnest_tokens(word, text) %>% count(entity, word, sort = TRUE) %>% bind_tf_idf(word, entity, n) %>% arrange(desc(tf_idf))

write.csv(codebook_description.tf_idf, paste0(path.example, "outputs/clinEpi.tf_idf.codebook_description.csv"), row.names=FALSE) 


# -----
# all text associated with ontology term
# -----

allText.tf_idf <- clinEpi.corpus %>% unnest_tokens(word, allText) %>% count(entity, word, sort = TRUE) %>% bind_tf_idf(word, entity, n) %>% arrange(desc(tf_idf))

write.csv(allText.tf_idf, paste0(path.example, "outputs/clinEpi.tf_idf.allText.csv"), row.names=FALSE) 


# -------------------------------------------------------------------------------------------------------------------------------------------------------

####
# subClasses calculation, all clinEpi projects
# ----------------------------------------

path.subClasses <- "/Users/jiezheng/Documents/VEuPathDB-git/TermMapper/src/examples/subClasses/"

# subClasses axioms
superclasses.res <- read.csv(file = paste0(path.subClasses, "clinEpi_noClassification_subClasses.csv"))

superclasses.table <- table(superclasses.res$super)
superclasses.table <- cbind.data.frame(names(superclasses.table), as.numeric(superclasses.table))

names(superclasses.table) <- c("superclass", "subclass.count")
superclasses.table <- superclasses.table[superclasses.table$subclass.count > 1 , ]
write.csv(superclasses.table, paste0(path.example, "outputs/clinEpi.superclasses.table.csv"), row.names=FALSE) 

real.supers <- superclasses.table$superclass
real.supers <- superclasses.res[superclasses.res$sub %in% real.supers | superclasses.res$super %in% real.supers ,]
real.supers <- real.supers[, c(2, 1)]

####
# use igraph to find the number of subClassOf hops
#   between each entity (shortest path between two classes)
# ----------------------------------------

library(igraph)

real.supers.graph <- graph_from_data_frame(real.supers, directed = FALSE)

# calculate the shortest path
real.supers.dist.tab <- distances(real.supers.graph)
real.supers.dist.tab <- as.data.frame(real.supers.dist.tab)

colnames(real.supers.dist.tab) <- V(real.supers.graph)$name
real.supers.dist.tab$from <- V(real.supers.graph)$name

write.csv(real.supers.dist.tab, paste0(path.example, "outputs/clinEpi.real.supers.dist.tab.csv")) 

####
# use reshape2 melt function 
# 	convert the format in class A, class B, and their distances
# ----------------------------------------

library(reshape2)

# wide to long conversion, stack the data as variable and value with id.vars not stacked
real.supers.dist.tab <- melt(data = real.supers.dist.tab, id.vars = "from")

# change to text type (as.character)
real.supers.dist.tab$variable <- as.character(real.supers.dist.tab$variable)

write.csv(real.supers.dist.tab, paste0(path.example, "outputs/clinEpi.classes.long.distance.csv")) 





















# removed row without codebook_description
doc.codebook_desc <- doc.codebook_desc[nchar(doc.codebook_desc$codebook_description) > 0,]

temp <- unnest_tokens(doc.codebook_desc, word, codebook_description)
temp <- count(temp, entity, word, sort = TRUE)
temp2 <- unnest_tokens(doc.codebook_desc, word, codebook_description)
temp2 <- count(temp2, word, sort = TRUE)

doc.codebook_desc.tf_idf <- bind_tf_idf(temp, word, entity, n)

doc.codebook_desc.tf_idf.desc <- arrange(doc.codebook_desc.tf_idf, desc(tf_idf))


# write the tidytext stop word
write.csv(stop_words, paste0(path.example, "stop_words.csv"), row.names=FALSE)

# write the word count in codebook_description
write.csv(temp2, paste0(path.example, "codebook_description_words.csv"), row.names=FALSE)



# not used

clinEpi.corpus.clean <- apply(clinEpi.corpus, 2, function(current.column) {
		gsub(pattern = '[[:punct:]]',
                                 replacement = '',
                                 x = current.column)
	}
 )



