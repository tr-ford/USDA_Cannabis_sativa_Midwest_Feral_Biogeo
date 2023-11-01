## File_Tree.R

## Objective: Print out project directory tree

library(fs)

sink(file = "file_tree.txt") ## Open sink console, which captures outputs from console
dir_tree(".") ## may have to .. up from other folder
sink()## Close sink


