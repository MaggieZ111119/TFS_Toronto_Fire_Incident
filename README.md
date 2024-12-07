# Starter folder

## Overview

This repository contains the Toronto Fire Services (TFS) Incidents Analysis project. The project investigates fire severity levels in Toronto to improve resource allocation for Toronto Fire Services (TFS). The analysis identifies that higher-severity incidents are correlated with incidents originating from mechanical, HVAC,
and electrical areas, while cooking and lighting areas tend to lead to lower severity
fires. The study also finds that fire incidents involving certain ignition sources,
such as electrical units or lightning equipment, are more likely to be severe. Additionally, severe fires are associated with greater financial losses and higher casualty
rates, affecting both civilians and firefighters. These insights can guide TFS to
enhance response times, prioritize resources, and optimize fire response operations,
ultimately improving community safety in Toronto.

## File Structure

The repo is structured as:

-   `data/01-raw_data` contains the raw data as obtained from Open Data Toronto: Fire Incidents: https://open.toronto.ca/dataset/fire-incidents/
-   `data/02-analysis_data` contains the cleaned dataset that was constructed,and also a dataset created for analysis in EDA Process.
-   `model` contains fitted models. 
-   `other` contains details about LLM chat interactions, and sketches of the data planning.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download and clean data. Also included scripts used for model puddling, result stud.


## Statement on LLM usage

Aspects of the paper are written with the help of ChatGPT 4.0 and the full chat history is available in `other/llms/usage.txt`.



