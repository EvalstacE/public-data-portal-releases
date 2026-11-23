# shared/R/schema_utils.R
# --- Reusable release-pipeline guards (base R only — zero extra dependencies)
# --- call it right before bind_rows() in any update script

assert_same_schema <- function(old_df,
                               new_df,
                               label = "dataset",
                               check_types = TRUE,
                               strict_numeric = FALSE) {
  if (!is.data.frame(old_df)) {
    stop(sprintf("`old_df` for %s must be a data.frame.", label), call. = FALSE)
  }
  if (!is.data.frame(new_df)) {
    stop(sprintf("`new_df` for %s must be a data.frame.", label), call. = FALSE)
  }
  
  old_cols <- names(old_df)
  new_cols <- names(new_df)
  
# Verify column names match (order-independent)
  if (!setequal(old_cols, new_cols)) {
    only_old <- setdiff(old_cols, new_cols)
    only_new <- setdiff(new_cols, old_cols)
    
    stop(sprintf(
      paste0(
        "Schema mismatch in %s — migrate the archived .rds before publishing.\n",
        "  Only in archive: %s\n",
        "  Only in new:     %s"
      ),
      label,
      if (length(only_old)) paste(only_old, collapse = ", ") else "(none)",
      if (length(only_new)) paste(only_new, collapse = ", ") else "(none)"
    ), call. = FALSE)
  }
  
# Verify column types match on aligned columns
  if (isTRUE(check_types)) {
    get_type <- function(x) {
      if (is.logical(x) && all(is.na(x))) return("any_na")
      cls <- class(x)[1]
      if (!strict_numeric && cls %in% c("integer", "numeric")) "numeric" else cls
    }
    
    old_types <- vapply(old_df[old_cols], get_type, character(1))
    new_types <- vapply(new_df[old_cols], get_type, character(1))
    
    mismatch <- (old_types != new_types) &
      old_types != "any_na" &
      new_types != "any_na"
    
    bad <- old_cols[mismatch]
    if (length(bad) > 0L) {
      diffs <- sprintf(
        "  %s: archive=<%s>, new=<%s>",
        bad, old_types[bad], new_types[bad]
      )
      stop(sprintf(
        "Column type mismatch in %s — migrate or cast before publishing.\n%s",
        label,
        paste(diffs, collapse = "\n")
      ), call. = FALSE)
    }
  }
  
  invisible(TRUE)
}