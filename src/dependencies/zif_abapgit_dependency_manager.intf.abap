"! Dependency manager
INTERFACE zif_abapgit_dependency_manager PUBLIC.
  TYPES:
    gty_decision TYPE c LENGTH 1,
    gty_state    TYPE c LENGTH 20,
    BEGIN OF gty_strategy,
      dependency TYPE zif_abapgit_dependency=>gty_unique_name,
      state      TYPE gty_state,
      decision   TYPE gty_decision,
    END OF gty_strategy,
    gty_strategy_tab TYPE STANDARD TABLE OF gty_strategy WITH DEFAULT KEY.
*    gty_graph_tab TYPE STANDARD TABLE OF gty_graph WITH DEFAULT KEY.
  CONSTANTS:
    BEGIN OF gc_decisions,
      ignore  TYPE gty_decision VALUE 'I',
      install TYPE gty_decision VALUE 'L',
    END OF gc_decisions,
    BEGIN OF gc_states,
      already_installed     TYPE gty_state VALUE 'INSTALLED',
      not_installed         TYPE gty_state VALUE 'NOT_INSTALLED',
      installed_old_version TYPE gty_state VALUE 'INSTALLED_OLD',
    END OF gc_states.
  METHODS:
    build_graph IMPORTING io_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit,
*      RETURNING VALUE(rt_graph) TYPE zif_abapgit_dependency~,
    resolve_dependencies IMPORTING it_strategy TYPE gty_strategy_tab.
ENDINTERFACE.
