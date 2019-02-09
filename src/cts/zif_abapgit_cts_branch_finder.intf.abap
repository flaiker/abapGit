"! Branch finder
INTERFACE zif_abapgit_cts_branch_finder PUBLIC.
  TYPES:
    gty_branching_strategy TYPE c LENGTH 1.
  CONSTANTS:
    BEGIN OF gc_strategies,
      transport TYPE gty_branching_strategy VALUE 'T',
    END OF gc_strategies.
  METHODS:
    get_branch_for_transport IMPORTING iv_transport     TYPE trkorr
                                       iv_strategy      TYPE gty_branching_strategy
                             RETURNING VALUE(rv_branch) TYPE string
                             RAISING   zcx_abapgit_exception.
ENDINTERFACE.
