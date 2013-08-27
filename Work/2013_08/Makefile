BASE_DIR = ../..

splits: $(BASE_DIR)/Data/fvc.5.visits.csv
	Rscript -e 'source("data.R"); write_splits("$<", "train.csv", "test.csv")'
