#' Calculate means using group_by type
#'
#' @export

calc_means <- function(df, x_col, type_col, id_col) {
  df$x <- df[[x_col]]
  df$batch <- df[[type_col]]
  df$id <- df[[id_col]]
  new.df = df %>%
    group_by(batch) %>%
    summarise(
      id = id[1],
      n = n(),
      mean = mean(x),
      `mean + sigma` = mean + sd(x),
      `mean - sigma` = mean - sd(x),
      `mean + 2 sigma` = mean + 2 * sd(x),
      `mean - 2 sigma` = mean - 2 * sd(x)
    ) %>%
    gather(linetype, yintercept, -batch, -n, -id) #%>%
  #filter(!is.na(yintercept))
  return(new.df)
}