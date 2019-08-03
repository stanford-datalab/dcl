# For explanation of how to use, see:
#   https://github.com/dcl-docs/project-example
# For example Makefile, see:
#   https://github.com/dcl-docs/project-example/blob/master/Makefile

# Search path
VPATH = data data-raw eda reports scripts

# Processed data files
data =

# EDA studies
eda =

# Reports
reports =

# All targets
all : $(data) $(eda) $(reports)

# Data dependencies


# EDA study and report dependencies


# Pattern rules
%.rds : %.R
	Rscript $<
%.md : %.Rmd
	Rscript -e 'rmarkdown::render(input = "$<", output_options = list(html_preview = FALSE))'
