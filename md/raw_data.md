To be valid, input data must be flat text files (*.csv) comprising:

  - `../input/*_wq.csv`

    <div class="table-minimal">

    | Field                 | Description                             | Validation conditions                                   |
    |-----------------------|-----------------------------------------|---------------------------------------------------------|
    | Zone                  | Spatial zone                            | must be numeric or factor                               |
    | Region                | Spatial region                          | must be numeric or factor                               |
    | Source                | Source of samples (CFM or Discrete)     | must be character or factor of either (CFM or Discrete) |
    | Date                  | Sample data                             | must be a valid string                                  |
    | Latitude              | Latitude of sample                      | must be a numeric of format -d.d                        |
    | Longitude             | Longitude of sample                     | must be a numeric of format d.d                        |
    | Chla_mug_PER_L        | Chlorophyll-a concentration             | must contain only numbers or start with a '<' symbol    |
    | Turbidity_NTU         | Turbidity concentration                 | must contain only numbers or start with a '<' symbol    |
    | Turbidity_NTU         |                                         | should not exist for Discrete source                    |
    | DO_PERCENT_saturation | Percentage dissolved oxygen             | must contain only numbers or start with a '<' symbol    |
    | NH3_mug_PER_L         | Ammonium concentration                  | must contain only numbers or start with a '<' symbol    |
    | NH3_mug_PER_L         |                                         | should not exist for CFM source                         |
    | PO4_mug_PER_L         | Phosphate concentration                 | must contain only numbers or start with a '<' symbol    |
    | PO4_mug_PER_L         |                                         | should not exist for CFM source                         |
    | Nox_mug_PER_L         | Nox (nitrate and nitrite) concentration | must contain only numbers or start with a '<' symbol    |
    | Nox_mug_PER_L         |                                         | should not exist for CFM source                         |
    
    </div>

  - `../input/hierarchy.csv`

    <div class="table-minimal">

    | Field          | Description                                               | Validation conditions   |
    |----------------|-----------------------------------------------------------|-------------------------|
    | Component      | Highest level of measure hierarchy (always Environmental) | must contain characters |
    | IndicatorGroup | Next measure level (always Water Quality)                 | must contain characters |
    | Indicator      | Next measure level (always Water Quality)                 | must contain characters |
    | Subindicator   | Either Nutrients or Physico-chem                          | must contain characters |
    | Measure        | Name of the Measure                                       | must contain characters |
   
    : {tbl-colwidths="[10,50,40]"} 
   
    </div>

  - `../input/weights_*.csv`

    <div class="table-minimal">

    | Field          | Description                                               | Validation conditions     |
    |----------------|-----------------------------------------------------------|---------------------------|
    | Component      | Highest level of measure hierarchy (always Environmental) | must contain characters   |
    | IndicatorGroup | Next measure level (always Water Quality)                 | must contain characters   |
    | Indicator      | Next measure level (always Water Quality)                 | must contain characters   |
    | Subindicator   | Either Nutrients or Physico-chem                          | must contain characters   |
    | Measure        | Name of the Measure                                       | must contain characters   |
    | Zone           | Spatial zone                                              | must be numeric or factor |
    | Region         | Spatial region                                            | must be numeric or factor |
    | Site           | Spatial site                                              | must be numeric or factor |
    | Weight         | Spatial site                                              | must be numeric           |
  
    Could be empty
    : {tbl-colwidths="[25,35,40]"} 
  
    </div>
  
  - `../input/overwrites.csv`

    <div class="table-minimal">

    | Field            | Description                                               | Validation conditions                                   |
    |------------------|-----------------------------------------------------------|---------------------------------------------------------|
    | Component        | Highest level of measure hierarchy (always Environmental) | must contain characters                                 |
    | IndicatorGroup   | Next measure level (always Water Quality)                 | must contain characters                                 |
    | Indicator        | Next measure level (always Water Quality)                 | must contain characters                                 |
    | Subindicator     | Either Nutrients or Physico-chem                          | must contain characters                                 |
    | Measure          | Name of the Measure                                       | must contain characters                                 |
    | Source           | Source of samples (CFM or Discrete)                       | must be character or factor of either (CFM or Discrete) |
    | Region           | Spatial region                                            | must be numeric or factor                               |
    | Zone             | Spatial zone                                              | must be numeric or factor                               |
    | Site             | Spatial site                                              | must be numeric or factor                               |
    | overwrittenGrade | Grade to apply (overwrite observations)                   | must contain characters (A, B, C, D or E)               |
  
    Could be empty
    : {tbl-colwidths="[25,35,40]"} 
  
    </div>
  

  - `../parameters/water_quality_guidelines.csv`

    <div class="table-minimal">

    | Field              | Description                                                 | Validation conditions                                |
    |--------------------|-------------------------------------------------------------|------------------------------------------------------|
    | ZoneName           | Zone name                                                   | must contain characters                              |
    | HydstraName        | Name in hydstra                                             | must contain characters                              |
    | Conversion         | Unit conversion factor                                      | must be numeric                                      |
    | Measure            | Name of the Measure                                         | must contain characters                              |
    | UnitsLabel         | Name of the Measure including units                         | must contain characters                              |
    | Label              | Name of the Measure including units (formatted for LaTeX)   | must contain characters                              |
    | DirectionOfFailure | Direction of failure relative to guideline value            | must be a single character (B or H)                  |
    | GL                 | Water quality guideline value                               | must contain only numbers or start with a '<' symbol |
    | RangeFrom          | Water quality guideline lower limit of range (for DO)       | must contain only numbers or start with a '<' symbol |
    | RangeTo            | Water quality guideline upper limit of range range (for DO) | must contain only numbers or start with a '<' symbol |
    | DetectionLimit     | Limit of detection value                                    | must contain only numbers or start with a '<' symbol |

    : {tbl-colwidths="[25,35,40]"}

    </div>

  - `../parameters/spatial.csv`

    <div class="table-minimal">

    | Field      | Description                                     | Validation conditions                           |
    |------------|-------------------------------------------------|-------------------------------------------------|
    | Region     | Spatial region                                  | must be numeric or factor                       |
    | RegionName | Region name                                     | must contain characters                         |
    | Zone       | Spatial zone                                    | must be numeric or factor                       |
    | ZoneName   | Zone name                                       | must contain characters                         |
    | Lab_lat    | Latitude of zone label on maps                  | must be a numeric of format -d.d                |
    | Lab_long   | Longitude of zone label on maps                 | must be a numeric of format d.d                 |
    | HexColor   | Hexidecimal colour code for zone labels on maps | must be a six digit hex code proceeded by a '#' |

    : {tbl-colwidths="[25,35,40]"}

    </div>



