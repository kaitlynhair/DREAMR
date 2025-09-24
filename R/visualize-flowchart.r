# Function to generate flowchart (3-steps) --------------------------------

visualize_flow_3 <- function(labels){
  
  label_one <- labels[[1]]
  label_two <- labels[[2]]
  label_three <- labels[[3]]
  
  DiagrammeR::grViz("digraph trials {

# GRAPH
graph [layout = dot, rankdir = TB, splines = false]
node [shape = rectangle, width = 3, height = 1, fixedsize = true, penwidth = 1, fontname = Arial, fontsize = 12]
edge [penwidth = 1]

# NODES INCLUSION
one [label = '@@1']
two [label = '@@2']
three [label = '@@3'] [color = black, fillcolor = lightgray, fontcolor = black, shape = rounded, style='filled,dotted']

# rank = same

# EDGES
edge [minlen = 1]
one -> two -> three

}

# LABELS
[1]: label_one
[2]: label_two
[3]: label_three
")
}


# Prepare flowchart for unit: references ----------------------------------

label_entries <- "N entries detected"
label_references <- "N references (DOIs and/or PMIDs)\n detected"
label_openalex <- "N Open Alex records retrieved"

labels_records <- c(label_entries, label_references, label_openalex)

visualize_flow_3(labels_records)

# Prepare flowchart for unit: authors -------------------------------------

label_authors <- "N unique authors in Open Alex"
label_orcid_ids <- "N authors in Open Alex with ORCID"
label_orcid_records <- "N ORCID records retrieved"

labels_authors <- c(label_authors, label_orcid_ids, label_orcid_records)

visualize_flow_3(labels_authors)
