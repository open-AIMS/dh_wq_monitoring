This [Shiny](https://shiny.posit.co/) application is designed to
ingest very specifically structured excel spreadsheets containing
Darwin Harbour Water Quality monitoring data and produce various analyses
and visualisations. The application is served from a
[docker](https://www.docker.com/) container to the localhost and the
default web browser.

Docker containers can be thought of a computers running within other
computers. More specifically, a container runs an instance of image
built using a series of specific instructions that govern the entire
software environment. As a result, containers run from the same image
will operate (virtually) identically regardless of the host
environment. Furthermore, since the build instructions can specify
exact versions of all software components, containers provide a way of
maximising the chances that an application will continue to run as
designed into the future despite changes to operating environments and
dependencies.

This shiny application comprises five pages (each accessable via the
sidebar menu on the left side of the screen):

1. a **Landing** page (this page) providing access to the settings and
   overall initial instructions
2. a **Dashboard** providing information about the progression of
   tasks in the analysis pipeline
3. a **Data** page providing overviews of data in various stages
4. a **QAQC** page providing graphical QAQC outputs
5. a **Summaries** page providing summaries of the bootstrap
   aggregation of indices
6. a **Manual** page that displays the online manual for the
   application

Each page will also contain instructions to help guide you through
using or interpreting the information. In some cases, this will take
the from of an info box (such as the current box). In other cases, it
will take the form of little <span class="fas fa-circle-info"></span>
symbols whose content is revealed with a mouse hover.

There are numerous stages throughout the analysis pipeline that may
require user review (for example examining any data validation issues
as well as the QAQC figures to confirm that the data are as expected).
Consequently, it is advisable for the user to manually trigger each
successive stage of the pipeline. The stages are:

- Stage 1 - Prepare environment 
  <details><summary>More info</summary>
  <p class = "details-info">
  This stage is run automatically on startup and essentially sets up the operating environment.
  
  - load any R package dependencies
  - get runtime settings from `../params/config.ini`.
    These include:
    - `focal_year`: usually the final year of sampling, all artifacts
       (data/graphics) will be stored in a folder reflecting this year
    - `method`: the index method to apply when calculating indices
    - `foldcap`: the folding cap to apply when calculating indices
    - `tuning`: the tuning to apply when calculating indices
    - `size`: the number of bootstrapp samples
    - `seed`: the random seed to apply to bootstrapping
  </p> </details>
- Stage 2 - Obtain data
  <details><summary>More info</summary>
  <p class = "details-info">
  This stage comprises of the following steps:
  
  - read in the water quality guidelines from
    `../parameters/water_quality_guidelines.csv`.
  - read in each of the water quality data files from `../input/`.
    These files are in the format of `<number>_wq.csv`, where
    `<number>` is a two digit number representation of the sampling
    year.
  - read in each of the overwrites file from
    `../input/overwrites.csv`.
  - read in each of the measures weights file from
    `../input/weights_m.csv`.
  - read in each of the spatial weights file from
    `../input/weights_s.csv`.
  - read in the aggregation hierarchy file from
    `../input/hierarchy.csv`.
  - read in the spatial settings file from
    `../parameters/spatial.csv`.
  - validating each of the sources of input data according to a set of
    validation rules
  
  The tables within the **Raw data** tab of the **Data** page will
  also be populated (but wont be available for review until after the
  data have been processed in Stage 3). 
  </p> </details>
- Stage 3 - Prepare spatial data
  <details><summary>More info</summary>
  <p class = "details-info">
  This stage comprises of the following steps:
 
  - read in individual shapefiles from `../parameters/GIS`.  The files are:
    - `RCZ_rev24.shp`
    - `SBZone_upper.shp`
    - `Middle_Harbour_Upper.shp`
  - combine all shapefiles into a single shapefile
  
  The tables within the **Processed data** tab of the **Data** page will also be populated.
  </p></details>
- Stage 4 - Process data
  <details><summary>More info</summary>
  <p class = "details-info">
  This stage comprises of the following steps:
 
  - combine all the water quality data into a single data set
  - process the dates from strings into genuine date objects
  - filter data to the bounds either defined in
    `../parameters/config.ini` or the data
  - select only measures for which there are guideline values
  - if the `focal_year` is undefined, define it based on the maximum date
  - pivot the data into a longer format that is more suitable for
    analysis and graphing
  - join in the guidelines information
  - use the spatial information in the shapefiles to assign spatial
    domains such as Regions and Zones.
  - apply any unit conversions to the values
  - apply limit of detection rules (to Dissolved Oxygen)
  - join in the aggregation hierarchy
  
  The tables within the **Processed data** tab of the **Data** page will also be populated and the `Data` page will be available for review.
  </p>
  </details>
- Stage 5 - Calculate indices
  <details><summary>More info</summary>
  <p class = "details-info">
  This stage comprises of the following steps:
  
  - retrieve the processed data.
  - calculate the indices
  - prepare for bootstrapping
  
  </p>
  </details>
- Stage 6 - QAQC
  <details><summary>More info</summary>
  <p class = "details-info">
  This stage comprises of the following steps:
  
  - retrieve the processed data.
  - construct outlier plots
  - contruct an LOR table
  - contruct boxplots for each Measure for the Focal Year for each
    Zone
  - construct timeseries boxplots for each Measure/Zone
  - construct boxplots for each Measure for the Focal Year conditional
    on Zone
  
  The QAQC figures of the **QAQC** page will also be populated.
  </p>
  </details>
- Stage 7 - Bootstrapping
  <details><summary>More info</summary>
  <p class = "details-info">
  This stage comprises of the following steps:
  
  - generate bootsrapping schematic diagram
  - retrieve the processed data
  - retrieve the indices
  - process the overwrites
  - process the weights
  - aggregate to Zone/Measure/Source level
  - aggregate to Zone/Measure level
  - aggregate to Zone/Subindicator level
  - aggregate to Zone/Indicator level
  - aggregate to Region/Measure level
  - aggregate to Region/Subindicator level
  - aggregate to Region/Indicator level
  - aggregate to WH/Measure level
  - aggregate to WH/Subindicator level
  - aggregate to WH/Indicator level

  </p> </details>
- Stage 8 - Summaries
  <details><summary>More info</summary>
  <p class = "details-info">
  This stage comprises of the following steps:
  
  - retrieve the processed data
  - compile all the indice scores
  - generate Zone/Measure/Source level
  - collate Zone/Measure level scores
  - collate Zone/Subindicator level scores
  - collate Zone/Indicator level scores
  - collate Region/Measure level scores
  - collate Region/Subindicator level scores
  - collate Region/Indicator level scores
  - collate WH/Measure level scores
  - collate WH/Subindicator level scores
  - collate WH/Indicator level scores
  - generate trend plots
  - calculate effects (between years)
  - generate effects plots

  The trend and effects figures of the **Summaries** page will also be populated.
  </p> </details>

Underneath the sidebar menu there are a series of buttons that control
progression through the analysis pipeline stages. When a button is
blue (and has a play icon), it indicates that the Stage is the next
Stage to be run in the pipeline. Once a stage has run, the button will
turn green. Grey buttons are disabled.

Clicking on button will run that stage (or stages in some cases).
While the stage is in progress, a popup will be displayed over the
buttons. This popup serves two purposes. Firstly, some tasks within
some stages are computationally intense and thus take some time to
perform. For such tasks, a progress bar will be displayed in the popup
to inform you of the progress through this task. Secondly, as some
stages/tasks are slow, it provides visual feedback about when a stage
has truly started and completed and prevents the temptation to
repeatedly click on a button when nothing appears to be happening.

Once a stage is complete, the button will change to either green
(success), yellow (orange) or red (failures) indicating whether
errors/warnings were encountered or not. If the stage was completed
successfully, the button corresponding to the next available stage
will be activated.

Sidebar menu items that are in orange font are active and clicking on
an active menu item will reveal an associated page. Inactive menu
items are in grey font. Menu items will only become active once the
appropriate run stage has been met. The following table lists the
events that activate a menu item.

<div class="table-minimal">

| Menu Item | Trigger Event |
|-----------|---------------|
| Landing   | Always active |
| Dashboard | Always active |
| Data      | After Stage 4 |
| QAQC      | After Stage 6 |
| Summaries | After Stage 8 |
| Manual    | Always active |

Note, it is also possible to make all menu and buttons active using
the **Run in sequence** toggle, however this should only be used if
the full sequence of stages has already been run and you are returning
to the analyses in a later session 
</div>

