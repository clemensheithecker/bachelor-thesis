# Custom texreg function --------------------------------------------------

texreg_custom <- function(..., path) {
  require(texreg)
  
  table <- texreg(...)
  
  # Replace longtable with tabularx
  table <- sub(
    pattern = "\\begin{longtable}{l",
    replacement = "\\begin{tabularx}{\\textwidth}{X",
    x = table,
    fixed = TRUE
  )
  table <- sub(
    pattern = "\\end{longtable}",
    replacement = "\\end{tabularx}",
    x = table,
    fixed = TRUE
  )
  
  # Fix table notes
  table <- sub(
    pattern = "\\begin{TableNotes}[flushleft]",
    replacement = "\\begin{TableNotes}[para]",
    x = table,
    fixed = TRUE
  )
  table <- sub(
    pattern = "\\begin{TableNotes}[para]\n\\scriptsize",
    replacement = "\\begin{TableNotes}[para]\n\\footnotesize",
    x = table,
    fixed = TRUE
  )
  
  # Print table
  # cat(table, sep = "\n")
  
  # Export table
  cat(table, file = path, sep = "\n")
}


# Summary statistics table ------------------------------------------------

summary_stats_to_latex <- function(df, file, title, label, footnote) {
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
  
  output_new <- c(
    output[1:caption_position - 1],
    "\\begin{ThreePartTable}",
    "\\begin{TableNotes}[para]",
    paste0("\\footnotesize{", footnote, "}"),
    "\\end{TableNotes}",
    output[tabularx_position],
    caption,
    paste0(label, " \\\\"),
    output[tabularx_position + 1:length(output)]
  )


  # Remove null values
  output_new <- output_new[!is.na(output_new)]
  
  output_new[length(output_new) + 2] <- output_new[length(output_new)]
  output_new[length(output_new) - 2] <- output_new[length(output_new) - 3]
  output_new[length(output_new) - 1] <- "\\end{ThreePartTable}"
  output_new[length(output_new) - 3] <- "   \\insertTableNotes"
  
  # Remove unnecessary decimal points
  output_new <- gsub(".000", "", output_new)
  
  # Print table
  cat(output_new, sep = "\n")
  
  # Write to LaTeX file
  write(output_new, file = file)
}
