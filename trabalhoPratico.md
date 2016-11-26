---
title: "Trabalho Prático DM1"
author: "Luis Torgo"
date: "20/11/2016"
output: html_document
---



## The Problem and the Data

The provided spreadsheet contains information on crime events on Houston, Texas, USA. Each row contains several information on the event.

The general problem you need to solve consists in trying to obtain a model that helps the police to distribute its resources through the different areas of the city (policy beats). We will assume that the goal of the policy department is to allocate the resources based on the number of estimated events (of any type) for a certain area, i.e. your goal is to accurately estimate the number of events/offenses that are going to occur on a certain area. The police makes personel allocation decisions 3 times per day: (i) one decision for the morning period (8:00-12:00); (ii) one for the afternoon (12:00-19:00); and the last decision for the night period (19:00-8:00). These decisions are made for each week day and for each police beat. So, the general question for which the police wants an answer is: "What will be the number of eventes/offenses in police beat X for the period of the day Y?"

## Tasks

Your work should have 3 main goals:

- Data importation and clean-up
  - In this part of your work you should focus on importing the provided data files into an appropriate R format so that your posterior analysis is made simpler. Your should also check if it is necessary to carry out any data clean-up and/or pre-rpocessing steps.

- Data exploratory analysis
  - This part involves summarizing and visualizing the data in forms that you think are useful for the police.

- Predictive modeling
  - You should define a predictive task that can help the police to answer their operational goal. After defining the task you should use your available data to select and obtain a good model for this task. 

