find_workbench_repos <- function(out = stdout()) {
  # Ideally, we would do search for
  # path:config.yaml carpentry:
  # since sandpaper will fail if this field is unspecified. I.e., it is a
  # surefire way to catch all (valid=) workbench repos.
  # But GitHub API for search is extremely limited so we have to resort to
  # other means.

  # It isn't really clear why a single search doesn't allow to identify all
  # repos.
  # Manually checks confirm that both files are present everywhere...
  repos_workbench_search1 <- gh::gh(
    "/search/code",
    q = "filename:sandpaper-main.yaml",
    accept = "application/vnd.github.v3+json",
    per_page = 100,
    .limit = Inf
  ) |>
    purrr::pluck("items") |>
    purrr::map(function(element) {
      c(
        owner = purrr::pluck(element, "repository", "owner", "login"),
        repo = purrr::pluck(element, "repository", "name"),
        owner_type = tolower(purrr::pluck(
          element,
          "repository",
          "owner",
          "type"
        ))
      )
    }) |>
    dplyr::bind_rows()

  repos_workbench_search2 <- gh::gh(
    "/search/code",
    q = "filename:sandpaper-version.txt",
    accept = "application/vnd.github.v3+json",
    per_page = 100,
    .limit = Inf
  ) |>
    purrr::pluck("items") |>
    purrr::map(function(element) {
      c(
        owner = purrr::pluck(element, "repository", "owner", "login"),
        repo = purrr::pluck(element, "repository", "name"),
        owner_type = tolower(purrr::pluck(
          element,
          "repository",
          "owner",
          "type"
        ))
      )
    }) |>
    dplyr::bind_rows()

  repos_workbench <- rbind(repos_workbench_search1, repos_workbench_search2) |>
    dplyr::distinct() |>
    dplyr::arrange(owner, repo) |>
    dplyr::filter(repo != "sandpaper" | owner != "carpentries")

  repos_workbench |>
    jsonlite::write_json(out, pretty = TRUE)
}
