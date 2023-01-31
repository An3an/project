# Connect existing rstudio cloud environment to github using R


# Add authentication information: Use your GitHub user name and associated email address.
usethis::use_git_config(user.name = "An3an",
                        user.email = "andrean337@gmail.com")

# Initiate git:
usethis::use_git()

# Now use:
gitcreds::gitcreds_set()
# And enter the created GH-Token in the R console when prompted

# Create GitHub repo using:
usethis::use_github()


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
