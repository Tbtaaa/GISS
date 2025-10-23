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










library(usethis)
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)  # elimina origin
usethis::use_github(private = TRUE)  
2

library(usethis)
usethis::use_git_remote("origin", "https://github.com/Tbtaaa/GISS.git", overwrite = TRUE)
usethis::git_push(set_upstream = TRUE)

# ver a dónde apunta ahora
system("git remote -v")

# quitar el remoto 'origin'
system("git remote remove origin")

# (opcional) quitar la referencia de upstream si quedó guardada
system("git branch --unset-upstream")

# comprobar que ya no hay remoto
system("git remote -v")


system("git remote -v")                     # debería NO mostrar nada
system("git config --get-regexp '^remote\\.'")   # debería NO mostrar nada
