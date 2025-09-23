## Background
Meta-research rarely considers equity, diversity, and inclusion (EDI), meaning interventions designed to improve science could unintentionally disadvantage certain groups. Our project aims to address this by developing an automated workflow to evaluate the EDI characteristics of studies included in meta-research, similar to a 'Table 1' in clinical trials

## Aim
Develop a workflow (and shiny app) to analyse the sample characteristics of articles included in meta-research. 

# Preprint 
[COMING SOON] 

# ES Hackathon 2024 Project :rocket:
Shared doc: https://docs.google.com/document/d/1Ipl56W98NeFqhApFt6XT-oOFqGZsDJnOlNDGYQtbG90/edit?usp=sharing

# RRIA Unconference 2025 Project 🌟
Shared doc: 

## Run Locally with Docker

- Build and start the Shiny container:

```powershell
cd c:\Users\chris\source\repos\DREAMR
docker compose up --build
```

- Open the app at: http://localhost:3838/dreamr

- Live reload: edits to `shiny_app/` are mounted into the container and picked up automatically (refresh the browser).

- Stop the container:

```powershell
docker compose down
```
