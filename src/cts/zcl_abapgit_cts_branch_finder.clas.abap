"! Branch finder
CLASS zcl_abapgit_cts_branch_finder DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_cts_branch_finder.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_cts_branch_finder IMPLEMENTATION.
  METHOD zif_abapgit_cts_branch_finder~get_branch_for_transport.
    IF iv_strategy NA zif_abapgit_cts_branch_finder=>gc_strategies OR iv_transport IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDIF.

    CASE iv_strategy.
      WHEN zif_abapgit_cts_branch_finder=>gc_strategies-transport.
        rv_branch = to_upper( iv_transport ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
