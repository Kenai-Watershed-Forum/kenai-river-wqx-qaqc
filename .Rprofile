# Ensure interactive mode and Git repository detection
if (interactive() && file.exists(".git")) {
  options(ask_git_pull = TRUE)
}

# Function to prompt for Git pull when opening a project
git_pull_prompt <- function() {
  if (getOption("ask_git_pull", FALSE)) {
    message("Checking for remote changes...")
    system("git fetch")
    local_status <- system("git status -uno", intern = TRUE)
    
    if (any(grepl("Your branch is behind", local_status))) {
      response <- readline("⚠️ CAUTION: Your local branch is behind GitHub. Do you want to pull the latest changes? (y/n): ")
      if (tolower(response) == "y") {
        system("git pull")
      } else {
        message("Skipped git pull.")
      }
    } else {
      message("✅ Your branch is up to date.")
    }
  }
}

# Function to check Git status and prompt before closing
git_push_reminder <- function() {
  if (!interactive() || !requireNamespace("rstudioapi", quietly = TRUE)) {
    return()
  }
  
  status <- system("git status --porcelain", intern = TRUE)
  
  if (length(status) > 0) {
    response <- tryCatch(
      rstudioapi::showQuestion(
        title = "Reminder: Push to GitHub",
        message = "You have uncommitted or unpushed changes. Would you like to commit and push now?",
        ok = "Yes, Commit & Push",
        cancel = "No, Exit Anyway"
      ),
      error = function(e) {
        message("Error displaying push reminder:", e$message)
        return(FALSE)
      }
    )
    
    if (isTRUE(response)) {
      commit_message <- tryCatch(
        rstudioapi::showPrompt(
          title = "Commit Message",
          message = "Enter a commit message:",
          default = "Auto-commit before closing"
        ),
        error = function(e) {
          message("Error displaying commit prompt:", e$message)
          return(NULL)
        }
      )
      
      if (!is.null(commit_message) && commit_message != "") {
        system("git add .", intern = TRUE)
        system(paste("git commit -m", shQuote(commit_message)), intern = TRUE)
        
        # Push changes and capture output
        push_output <- system("git push 2>&1", intern = TRUE)
        
        if (any(grepl("fatal|error", push_output, ignore.case = TRUE))) {
          # If push fails, show error message
          rstudioapi::showDialog(
            title = "⚠️ Git Push Failed",
            message = paste("Git push failed. Check authentication or network settings.\n\n",
                            paste(push_output, collapse = "\n"))
          )
        } else {
          # If push is successful, show confirmation
          rstudioapi::showDialog(
            title = "✅ Push Successful",
            message = "Changes committed and pushed to GitHub!"
          )
        }
      }
    }
  }
}

# Ensure both hooks work correctly
setHook("rstudio.sessionInit", function(...) {
  git_pull_prompt()
}, action = "append")

.Last <- function() {
  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
    git_push_reminder()
  }
}

# Confirm that .Rprofile has loaded
print("✅ .Rprofile is running correctly")
