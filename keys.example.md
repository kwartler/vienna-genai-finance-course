# Step 1 - Install the usethis package
## Within the R Console
`install.packages('usethis')`

# Step 2 - Load the package
Run this code within the R Console
`library(usethis)`

# Step 3 - Open the local/secret environment file
Run this code within the R Console
`usethis::edit_r_environ()`

# Step 4 - add your API keys with the *exact* name here
Within the R "scripting" area in the opened up file, paste your keys.  Replace the text between the ' and ' from below.

`OPENROUTER_API_KEY='sk-or-v1-...a5'`

`NEWSORG_API_KEY='b6...fb'`

`FMP_API_KEY='yv...pf'`

`GEMINI_API='AQ...3w'`

# Step 5 - Close the environment file & restart R

# Step 6 - Check to make sure it works.
Run this code within the R Console and it should print your keys.

`Sys.getenv('OPENROUTER_API_KEY')`

`Sys.getenv('NEWSORG_API_KEY')`

`Sys.getenv('FMP_API_KEY')`

`Sys.getenv('GEMINI_API')`

