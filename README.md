## Epimath COVID-19 Modeling Dashboard
Development version by [Stephanie Choi](https://github.com/choisteph)

[A reworking of this dashboard.](https://epimath.github.io/covid-19-modeling/
)

*Deployment uses Phil Batey's [Flexdashboard Example](https://github.com/pbatey/flexdashboard-example) as reference for structuring `_site.yml`*

### Files and what they do
- **_site.yml**
    - This file "turns" RMarkdown files into HTML.
        - The most important line is `output_dir` because it sets which directory the HTML/CSS/JS files are created in. Here, it's the `doc` directory.
- favicon.png
    - A favicon for the web browser to load.
- **index.Rmd**
    - This file contains all of the RMarkdown that is used to generate the HTML/CSS/JS.
        - All pages are created in this one .Rmd document.
        - There are JS blocks in the code that use JQuery
        - CSS is external (available in `styles.css`) whenever possibles
- logo.png
    - A logo used in the navbar
- model_diagram_3_29.jpeg
    - This is an image of the model diagram that is used in the About page.
- styles.css
    - This file contains custom, external css.
- Workspace2020-04-25-1587832621.RData
    - All of the data that is used to create models, as well as other useful functions and data are stored here.
        - In the original repository, each time new data was pulled, a new version of the .RData file was created. Older versions of the workspace were then stored in a directory called `archive`.
    - Noted to be an inefficient way to call the necessary data.
- Various `.pdf` files
    - Thesea are the reports used in the Modeling Reports section.


### You need the following to run
#### RStudio and R
- RStudio - I'm using the latest version (1.3.1093)
- R - I'm using the latest version (4.0.2)

#### R Libraries
- Check `packrat.lock` in the `packrat` directory for the full list and their versions. The versions for each library will most likely be the latest versions.

### How to generate updated web files (HTML/CSS/JS)
*disclaimer: this might not be the most efficient way to do this*
1. Open `index.Rmd` in RStudio and make the changes you'd like to make
2. Save `index.Rmd`
3. In the RStudio Console (lower lefthand corner of the window), run `rmarkdown::render_site()` 
    - You can also "Knit" the document by pressing CTRL + SHIFT + K on Windows or SHIFT + COMMAND + K on Mac
