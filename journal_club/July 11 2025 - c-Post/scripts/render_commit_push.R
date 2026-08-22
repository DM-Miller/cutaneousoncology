render_commit_push <- function() {
  # Find all index.qmd files in the project
  index_files <- fs::dir_ls(path = ".", recurse = TRUE, regexp = "index\\.qmd$")
  # Render each index.qmd file
  for (file in index_files) {
    quarto::quarto_render(input = file)
  }
  
  # Initialize the repository
  repo <- git2r::repository(".")
  
  # Add all changes to staging
  git2r::add(repo, path = ".")
  
  # Commit the changes
  git2r::commit(repo, message = "Render all index.qmd files and update HTML output")
  
  # Push the changes to the remote repository
  system("git push origin HEAD:refs/heads/main")
}