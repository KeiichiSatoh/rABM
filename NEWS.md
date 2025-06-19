# rABM (development version)

# rABM 0.1.1
2025-06-19
Middle update
* [revised] runABM: now acccepts the method to change the parameter specified in 'plan' argument.
* [revised] runABM: argument name changed from "schedule" to "plan".

# rABM 0.1.0
2025-06-04
Middle update
* [revised] class_ABM_G: now acccepts 'summary_FUN' and 'plot_FUN'.
* [revised] class_ABM_Agent: new method "replace_field', "rename_field' added. 
* [revised] setABM: now acccepts 'summary_FUN' and 'plot_FUN'.
* [revised] setABM_helper: each ".shape_***_FUN" now governed solely by '.shape_G_FUN'
* [revised] modify_G: now acccepts 'summary_FUN' and 'plot_FUN'.
* [revised] modify_agents: change the internal process of 'rename', 'replace', and 'copy'. 
* [revised] modify_agents: change the method name from 'delete' to 'delete_field' to distinguish it from 'delete_agent'.
* [deleted] class_ABM_G: "partial update_FUN" category. 
* [deleted] setABM:  "partial update_FUN" category.
* [deleted] runABM: argument 'remove_field', 'rename_field', 'keep_field' 
as these functions are now covered by 'modify_G'.

# rABM 0.0.5
2025-06-02
* [new] modify_agents: A new function added.
* [revised] 'init_agent' renamed to 'init_agents'
* [revised] modify_G: argument 'field' renamed to 'new_obj'

# rABM 0.0.4
2025-05-31
* [revised] modify_G: added a method 'add_agents'.

# rABM 0.0.3
2025-05-31
* [revised] modify_G: put G and E to the formals when adding global_FUN, select_FUN, stop_FUN, or update_FUN.

# rABM 0.0.2
2025-05-26
* [add] modify_G
* [bug fix] class_ABM_G
* [bug fix] class_ABM_Agent

# rABM 0.0.1
2025-05-23
* Initial release to github.
* Basic rABM framework supported.
