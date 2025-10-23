library(usethis)

#START YOUR GIT
usethis::use_git_ignore("data/*")
usethis::use_git()
create_github_token()

#CONNECT GIT TO GITHUB
library(gitcreds)
gitcreds_set()

#COMMITING TO GIT
print(3+1)

#PUSH TO GITHUB
use_github()

#SEARCH FOR BIG SIZE FILES
fs::dir_info(recurse = TRUE) |> dplyr::filter(size > fs::fs_bytes("50MB")) |> dplyr::arrange(dplyr::desc(size)) |> dplyr::select(path, size)