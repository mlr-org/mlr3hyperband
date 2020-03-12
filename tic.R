do_package_checks(error_on = "error")

get_stage("install") %>%
  add_step(step_install_cran("bibtex"))

if (ci_on_ghactions()) {
  get_stage("install") %>%
    add_step(step_install_github("mlr-org/mlr3pkgdowntemplate"))
  do_pkgdown()
}

get_stage("after_success") %>%
  add_code_step(system("curl -s https://raw.githubusercontent.com/mlr-org/mlr3orga/master/trigger-mlr3book.sh | bash"))
