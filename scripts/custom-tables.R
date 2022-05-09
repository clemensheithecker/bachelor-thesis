regression_table_thesis <- function(..., file, footnote = NA) {
  # Todo: Optional argument for longtable
  require(devtools)
  require(stargazer)
  # install_github("markwestcott34/stargazer-booktabs")
  
  table <- capture.output(
    stargazer(
      # Output as LaTeX file
      type = "latex",
      # Omit the dependent variable caption
      dep.var.caption = "",
      # omit dependent variable labels
      dep.var.labels.include = FALSE,
      # Font size used in the table
      font.size = "small",
      # Omit stargazer header at the top of the LaTeX file
      header = FALSE,
      # Move intercept/constant coefficient to the top of the table
      intercept.bottom = FALSE,
      # Prevent numbering of models
      model.numbers = FALSE,
      # Remove all empty lines from the table
      no.space = TRUE,
      # Define which parts of the table to include: single horizontal line,
      # column labels, mandatory single horizontal line, coefficient table,
      # single horizontal line, model statistics
      table.layout = "-c-!t-s",
      ...
    )
  )
  
  caption <- table[which(startsWith(table, "  \\caption{"))][1]
  
  label <- table[which(startsWith(table, "  \\label{"))][1]
  
  font_sizes <- c(
    "tiny",
    "scriptsize",
    "footnotesize",
    "small",
    "normalsize",
    "large",
    "Large",
    "LARGE",
    "huge",
    "Huge"
  )

  font_size <- grep(
    paste(font_sizes, collapse = "|"),
    table,
    value = TRUE
  )[1]

  begin_tabular <- which(startsWith(table, "\\begin{tabular}"))[1]
  end_tabular <- which(startsWith(table, "\\end{tabular}"))[1]

  table <- table[begin_tabular:end_tabular]

  # Replace tabular with tabularx environment
  
  table[1] <- gsub(
    ".*}c",
    "\\\\begin{tabularx}{\\\\textwidth}{Xc",
    table[1]
  )
  
  table[length(table)] <- gsub(
    "tabular",
    "tabularx",
    table[length(table)]
  )
  
  # Fix booktabes table lines
  
  first_midrule <- which(startsWith(table, "\\midrule "))[1]
  second_midrule <- which(startsWith(table, "\\midrule "))[2]
  
  table[first_midrule] <- "\\toprule "
  

  table[second_midrule] <- gsub(
    "\\midrule \\\\[-2.1ex]",
    " ",
    table[second_midrule],
    fixed = TRUE
  )
  
  table <- c(
    # Add beginning of ThreePartTable
    "\\begin{ThreePartTable} ",
    "\\centering ",
    # Add font size
    font_size,
    table[1],
    # Add caption
    caption,
    # Add label
    paste0(label, "\\\\ "),
    # Add header (first page)
    table[2:3],
    "\\midrule \\\\[-2.1ex] ",
    "\\endfirsthead ",
    # Add header (second and following pages)
    paste0(
      gsub("} ", " (Continued)} ", caption, fixed = TRUE),
      "\\\\ "
    ),
    table[2:3],
    "\\midrule \\\\[-2.1ex] ",
    "\\endhead ",
    # Add footer (first page)
    "\\midrule[\\heavyrulewidth] ",
    "\\multicolumn{4}{r}{\\textit{Continued on next page}} ",
    "\\endfoot ",
    "\\bottomrule \\\\[-4.1ex] ",
    "\\endlastfoot ",
    # Insert table (and remove \bottomrule at the end of the table)
    table[6:length(table) - 2],
    table[length(table)],
    # Add end of ThreePartTable
    "\\end{ThreePartTable} "
  )
  
  # Add footnote if provided
  if (!is.na(footnote)) {
    table <- c(
      table[1:length(table) - 1],
      footnote,
      "\\bigskip ",
      table[length(table)]
    )
  }
  
  # Print table
  cat(table, sep = "\n")

  # Export table
  cat(table, file = file, sep = "\n")
}


summary_stats_to_latex <- function(df, file, title, label, footnote = NA) {
  # Capture output from xtable
  output <- capture.output(print(
    xtable(
      df,
      type = "latex",
      caption = title,
      # Set the alignment of the columns
      align = c("l", "X", "r", "r", "r", "r"),
      # Set the number of digits
      digits = c(0, 0, 3, 3, 3, 3),
      # Set the format of the columns
      display = c("s", "s", "f", "f", "f", "f"),
      label = label
    ),
    type = "latex",
    include.rownames = FALSE,
    caption.placement = "top",
    booktabs = TRUE,
    tabular.environment = "tabularx",
    width = "\\textwidth",
    comment = FALSE
  ))
  
  caption_position <- which(startsWith(output, "\\caption{"))
  label_position <- which(startsWith(output, "\\label{"))
  
  caption <- output[caption_position]
  label <- output[label_position]
  
  tabularx_position <- which(startsWith(output, "\\begin{tabularx}"))
  
  output <- c(
    output[1:caption_position - 1],
    output[tabularx_position],
    caption,
    paste0(label, " \\\\"),
    output[tabularx_position + 1:length(output)]
  )
  
  # Remove null values
  output <- output[!is.na(output)]
  
  # Remove unnecessary decimal points
  output <- gsub(".000", "", output)
  
  if (!is.na(footnote)) {
    # Add footnote
    output[length(output) + 1] <- output[length(output)]
    output[length(output) - 1] <- paste0("\\floatfoot*{", footnote, "}")
  }
  
  # Print table
  cat(output, sep = "\n")
  
  # Write to LaTeX file
  write(output, file = file)
}
