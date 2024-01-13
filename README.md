# Semantic Scholar Paper Data Visualizer
Extract and process academic paper data (authors, citation counts, influential citation counts, references )from any search query, cluster the papers based on their abstracts, and analyze their features using R and RShiny

## 1. Run code in `raw_data_creation.ipynb` to get raw data and cluster
- Set up the environment first. Look at first cell for relevant code.
- Set a semantic scholar search query of your choice - I used "large language models".
- The embedding creation for the clustering algorithm can take some time. There is a cell in the notebook that creates the initial embeddings. The following cell uses an existing embeddings file and appends to it in case you have additional data points to add.

## 2. Run `RData_creation.R` to create the `.RData` file used for RShiny dashboard

## 3. Run `app.R` to display the RShiny dashboard

## Deploy the dashboard to the cloud:
- Create a shinyapps account here: [shinyapps.io](https://www.shinyapps.io).
- Follow the instructions for deployment here: [Deploying to shinyapps.io](https://shiny.posit.co/r/articles/share/shinyapps/).
- When deploying, set the directory to the app as the folder that contains both the `app.R` file and the `papers.RData` file. I named this file `LLM-papers-2023-dashboard`, but you can rename it to anything else. Note that this folder name will appear in the URL of the RShiny web app. For example, mine is [https://davydsadovskyy.shinyapps.io/llm-papers-2023-dashboard/](https://davydsadovskyy.shinyapps.io/llm-papers-2023-dashboard/).