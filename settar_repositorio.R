# Pacotes ----

library(usethis)

library(gert)

# Iniciar git ----

usethis::use_git()

# Primeiro commit ----

gert::git_add(files = ".gitignore")

gert::git_commit(message = "gitignore")

# Criar repositório ----

usethis::use_github()
