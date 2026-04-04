# rABM (development version)

# rABM 1.1.9
2026-04-03
* [fixed] `run_ABM`: fixed the malfunctioned `fields_to_save`argument. Improved the behavior, when `verbose` is `FALSE`


# rABM 1.1.8
2026-04-02
* [improved] Logic of `Series`, `Chunk` and `run_Series`

# rABM 1.1.7
2026-04-02
* [improved] `run_Game`: added beep function after running.
* [deleted] `E = E` argument formatting and the related functions are now removed.
* [fixed] mal-functioned `value_of`.
* [new] Added a tutorial page (JP): act_FUN, run_Game, report_FUN

# rABM 1.1.6
2026-03-30
* [removed] deleted mal-functioning `ask_gpt`. 

# rABM 1.1.5
2026-03-20
* [fixed] changed `summary_FUN` and `Summary` to `report_FUN` and `Report` due to 
the potential conflicting names.


# rABM 1.1.4
2026-03-19
* [new] Added a tutorial page (JP): plot_FUN

# rABM 1.1.3
2026-03-18
* [new] Added a tutorial page (JP): state and active state

# rABM 1.1.2
2026-03-18
* [new] Added a tutorial page (JP): rABM classes


# rABM 1.1.1
2026-03-17
* [new] Added a tutorial page (JP): using model library
* [new] prob_score: various basic transformation rules from score to probability

# rABM 1.1.0
2026-03-04
* [new] Added a tutorial page (JP): quick start guide 

# rABM 1.0.0
2026-03-04
* Completely overhauled the structure of major classes. 

# rABM 0.1.9
2025-11-03
* [revised] test_FUN: amended some bugs.

# rABM 0.1.8
2025-08-14
* [new] test_FUN: an interactive test function of a new function for the ABM. 
* [revised] compare_G: the output now shows G1 and G2 as column name.

# rABM 0.1.7
2025-08-07
* [new] runABM_repl: an interactive version of runABM.

# rABM 0.1.6
2025-08-05
* [new] ask_gpt: a function to ask questions about rABM to GPT utilizing RAG.
* [new] log_to_G: a function to reconstruct G object from the log.
* [new] compare_G: a function that lists the different field values between G1 and G2.
* [revised] ABM_Agent: revised .add_active_binding method so that it surely delete the previous bindings.


# rABM 0.1.5
2025-07-30
* [revised] setABM: Changed the argument from 'active_binding_field' to 'active_binding'
* [new] log_to_G: a function to reconstruct G object from log.


# rABM 0.1.4
2025-07-29
* [revised] init_agents: 
    - now accepts custom_ID
    - agents can have have different active binding functions
    - parameter name changed from "active_binding_field" to "active_binding"
    - agents does not have ID as their list name. They have only IDs in their field.
* - revised test files in accordance to this overall changes.
    

# rABM 0.1.3
2025-06-23
* [revised] modify_agents: has a new method "replace_agent"
* [new] get_agents_by_idx: is now added.
* [revised] zzz.R: added a .DollarNames control so that the internal methods are
not shown in the suggestion in Rstudio and "time", "notes" and "log" are shown always at the end.

# rABM 0.1.2
2025-06-20
* [new] %aa% and %ai% operators are now added.
* [new] sample2 is now added.

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
