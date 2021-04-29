if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, cli, janitor, glue, progress)

# Show a single reference
render_ref <- function(ref) {
  journal_string <- glue::glue("{ref$journal}", "{ref$volume}", "{ref$issue}",
                               "{ref$pages}", .sep=", ")

  cli_div(theme = list(span.emph = list(color = "grey")))
    cli_par()
      cli_text("{ref$author_s}")
    cli_end()

    cli_par()
      col_silver(cli_text("{.emph {journal_string}.}"))
    cli_end()

    cli_par()
      cli_text("{.strong {str_to_title(ref$title)}}")
    cli_end()
  cli_end()

  cli_div()
    cli_par()
      cli_text("{ref$abstract}")
    cli_end()
  cli_end()
}


# Tag a single reference
tag_ref <- function(ref, .tags) {

  render_ref(ref)

  cli_h2("Tags")
  cli_div()

  cli_par()

  for (i in seq_along(.tags)) {
    tag_name <- tags[i]
    tag_col <- names(.tags)[i]
    current <- ref[tag_col]

    if (is.na(current)) {
      cat_bullet(col_grey(tag_name), bullet_col = "grey")
    } else if (current == TRUE) {
      cat_bullet(col_green(tag_name), bullet = "tick", bullet_col = "green")
    } else if (current == FALSE) {
      cat_bullet(col_red(tag_name), bullet = "cross", bullet_col = "red")
    }
  }

  cli_end()
  cli_end()

  cat("\n")

  cli_div()
  cli_par()

  tryCatch({
    for (i in seq_along(.tags)) {
        tag_name <- .tags[[i]]
        tag_col <- names(.tags)[[i]]

        resp_ok <- FALSE
        while (!resp_ok) {
          tryCatch({
            resp <- askYesNo(glue("{tag_name}?"),
                             prompts = c("yes", "no", "SKIP"),
                             default = NA)
            resp_ok <- TRUE
          },
          error = function(e) {
            cli_alert_warning(
              col_yellow("Response not recognized. ",
                       "Type [y]es, [n]o, or leave empty...")
              )
          })
        }

        res <- if(is.na(resp)) ref[tag_col] else resp
        ref[tag_col] <- resp

        if (is.na(res)) {
          cat_bullet(col_grey("skipped"), bullet_col = "grey")

        } else if (res == TRUE) {
          cat_bullet(col_green("yes"), bullet = "tick", bullet_col = "green")

        } else if (res == FALSE) {
          cat_bullet(col_red("no"), bullet = "cross", bullet_col = "red")
        }
    }
  },
  interrupt = function(e) {
    cli_alert_danger("Interrupt!")
    return(ref)
  })

  return(ref)
}

# Screen all references
screen_refs <- function(df, tags, ...) {
  # add columns for tags if they don't already exist
  tmp <- tags
  tmp[] <- NA
  df <- add_column(df, !!!tmp[setdiff(names(tmp), names(df))])

  options(cli.width = 50)
  pb <- progress_bar$new(
    format = "[:bar] Item :current/:total (:percent), eta: :eta",
    total = nrow(df), clear = FALSE, width = 50, show_after = 0)

  # get responses
  res <- pmap_dfr(df, function(...) {
    cat("\014")  # clear screen
    cli_par()
      cli_h1("Tagging")
      pb$tick()
      cat("\n")
    cli_end()

    ref <- tibble(...)
    tag_ref(ref, .tags = tags)
  })

  # convert responses to decision
  # res <- mutate(res,
  #   include = if_all(!!names(tags), ~ .x == TRUE),
  #   exclude = if_any(!!names(tags), ~ .x == FALSE),
  #   maybe   = if_any(!!names(tags), ~ is.na(.x)) & !exclude,
  # )

  return(res)
}


sample_n(lit, 4) %>%
  screen_refs(tags)















# read in the file

lit <- readxl::read_xlsx(path = 'RCTsLangCommRevised.xlsx', trim_ws = TRUE, skip = 1) %>%
  janitor::clean_names() %>%
  janitor::remove_empty('rows') %>%
  select(author_s, year, journal, title, abstract, volume, issue, pages)



tags = c(
  lang = "lang/comm",
  rct = "RCT",
  lt6 = "<6 years",
  english = "English"
)


lit10 <- lit[1:10, ] %>%
  screen_refs(tags)

lit20 <- lit[11:20, ] %>%
  screen_refs(tags)

lit30 <- lit[21:30, ] %>%
  screen_refs(tags)

lit40 <- lit[31:40, ] %>%
  screen_refs(tags)

lit50 <- lit[41:50, ] %>%
  screen_refs(tags)


lit60 <- lit[51:60, ] %>%
  screen_refs(tags)


lit70 <- lit[61:70, ] %>%
  screen_refs(tags)


lit80 <- lit[71:80, ] %>%
  screen_refs(tags)


lit90 <- lit[81:90, ] %>%
  screen_refs(tags)


lit100 <- lit[91:100, ] %>%
  screen_refs(tags)

# ...

lit125 <- lit[101:125, ] %>%
  screen_refs(tags)

lit150 <- lit[126:150, ] %>%
  screen_refs(tags)



result <- bind_rows(
  lit10, lit20, lit30, lit40, lit50, lit60, lit70, lit80, lit90, lit100,
  lit125, lit150
)

# bug fix in post :)
result <- result %>%
  mutate(
    include = if_all(!!names(tags), ~ .x == TRUE),
    exclude = if_any(!!names(tags), ~ .x == FALSE),
    maybe   = if_any(!!names(tags), ~ is.na(.x)) & !exclude,
  )

result %>%
  write_csv("output.csv")

result %>%
  write_rds("result.rds")

result %>%
  mutate(across(c(!!names(tags)),
                ~ ifelse(.x, 'y', 'n')),
         across(c(include, exclude, maybe),
                ~ ifelse(.x == TRUE, 'y', NA))) %>%
  relocate(
    include, .after = last_col()
  ) %>%
  rename(
    no = exclude,
    yes = include
  ) %>%
  write_csv("output_tb.csv", na = "")




