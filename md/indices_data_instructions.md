The **Indices data** panel displays a summary of indices. The reason
it is summarised is that bootstrapping adds 10000 values per
combination - this is too large to load into a table. The following
new fields will be created:


<div class="table-minimal">

| Field          | Description                                               |
|----------------|-----------------------------------------------------------|
| Region         | Darwin Harbour Region number                              |
| Zone           | Darwin Harbour Zone number                                |
| Component      | Highest level of measure hierarchy (always Environmental) |
| IndicatorGroup | Next measure level (always Water Quality)                 |
| Indicator      | Next measure level (always Water Quality)                 |
| Subindicator   | Either Nutrients or Physico-chem                          |
| Measure        | Name of the Measure                                       |
| Source         | Source of samples (CFM or Discrete)                       |
| Index          | Summary of indices                                        |

: {tbl-colwidths="[20,80]"} 
   
</div>
