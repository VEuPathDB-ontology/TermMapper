# TermMapper

Files related to map variables in data dictionary to ontology terms.

src/R directory
- solr_search.R : map variables in data dictionary to ontology terms based on variable label and definition to ontology term label using solr, and list ontology terms with top 10 Solr scores.
- term_vs_term_training.R: use a SVM to predict the distance based on Solr scores and string distances that calculated the subClassOf distance between all entities. 

src/Input_Files
- input files used for mapping R scripts
- searchResults.csv: output file of solr_search.R generated in 2018
