-----------------------------------------------------------
File organization
-----------------------------------------------------------


ontology
- OWL file used to prepare the input files

mapping
- example mapping file used for mapping/training/validation test

outputs
- R analysis results

R_code
- R script used for text mining, mapping and analysis

solr_collection
- files used to create solr collection

SPARQL
- query may be used for retrieving useful information from OWL file for analysis

subClasses
- subClasses.rq query results


-----------------------------------------------------------
R scripts
-----------------------------------------------------------
text_mining.R
	Understanding the terms used in ClinEpi terminologies 

1. Check any useful fields for searching that are blank, results saved in outputs/clinEpi.corpus.notNull.count.csv

The fields in new dataset data dictionary can be used for mapping are variable, codebook_description and codebook_values that provided by data providers. We can test to map them to label, definition, codebook_description, and codebook_values in the clinEpi terminologies. 

Notes, not all variables have codebook_description and codebook_values. 

2. Check the words used in different fields in ClinEpi terminologies, results stored in outputs directory, the file begin with "clinEpi.words."

Using predefined word tokens in tidytext package, there are some number words might not be useful in mapping. May consider to remove in the future. And it will remove the '-'. So, the word 'z-score' will become to z and score. It may consider to use custom defined tokens to extract words in sentences. The test results stored in clinEpi.words.label.pattern.csv.

clinEpi.words.allText.word_cleaned.csv listed all words used in the clinEpi terminologies excluded one-letter words.

Consider all text associated with one ontology term as a document and calculated the td_idf. Some words may consider as stop words.


-----------------------------------------------------------
solr_search_modified.R
	Solr mapping and mapping analysis

Using GEMS1a as example (GEMS1a_mapping.csv):
- Total 587 variables need to map
- 544 can find the mapped terms in the clinEpi terminologies
- 43 variables do not have mapped terms in the clinEpi terminologies, replaced by the new terms parent in the example file

Fields used to map, 'variable', 'codebookDescription', and 'codebookValues'
- 582 variables have 'codebookDescription'
- 507 variables have 'codebookValues' and only 112 variables have more than 1 character codebooksvalues

3 kinds of search used
- variable to label in Solr collection
- variable and codebook_description to label in Solr collection, or variable to variable in Solr collection
- variable, codebook_description, and codebook_values to label in Solr collection, or variable to variable in Solr collection

Results of Solr search score, rank, class and cosine distances stored under 'examples/outputs' directory, filename start with 'multi.search'

SVM model is applied to ML 
- Summary information of SVN model training stored under 'examples/outputs' directory, filename start with 'SVM.summary'

