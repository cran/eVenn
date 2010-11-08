man <-
function()
{
  write("#####################", file="")
  write("# eVenn Quick Start #", file="")
  write("#####################", file="")
  write("- Prototype:\n\tevenn(annot=TRUE, ud=TRUE, noms=c(\"A\", \"B\", \"C\", \"D\"))", file="")
  write("\n- Venn diagram with up/down details and annotations:\n\tevenn(annot=TRUE, ud=TRUE)", file="")
  write("\n- Simple Venn diagram:\n\tevenn()\n", file="")
  write("- Venn diagram with custom short names of lists:\n\tevenn(noms=c(\"A\", \"B\", \"C\", \"D\"))\n", file="")
  write("####################", file="")
  write("# eVenn Quick Help #", file="")
  write("####################", file="")
  write("- annot [TRUE/FALSE]:\tIf the lists to be compared are files with identifiers (used for comparisons) and data (annotations or whatever).", file="")
  write("\t\t\tThe identifiers data (Gene name and extended name) must be the 2nd and 3rd columns.", file="")
  write("- path_res [path]:\tPath used to save the results. The working directory is used by default [getwd()].", file="")
  write("- path_lists [path]:\tPath used to find the lists to be compared. There is no default.", file="")
  write("\t\t\tThe files must be text tabulated.", file="")
  write("- res [matrix]:\t\tBinary matrix showing which identifier belong to which lists.", file="")
  write("\t\t\tColumns are compared lists, rows are identifiers.", file="")
  write("- ud [TRUE/FALSE]:\tIf the details of up and down regulated genes should be shown.", file="")
  write("\t\t\tThen the source files must comport a \"ratios\" column.", file="")
}

