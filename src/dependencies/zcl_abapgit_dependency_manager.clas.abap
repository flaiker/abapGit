"! Dependency manager
CLASS zcl_abapgit_dependency_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_dependency_manager.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      get_requirements IMPORTING io_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit.
*        RETURNING VALUE(rt_graph) TYPE zif_abapgit_dependency_manager~.
ENDCLASS.



CLASS zcl_abapgit_dependency_manager IMPLEMENTATION.
  METHOD zif_abapgit_dependency_manager~build_graph.
*    APPEND LINES OF get_requirements( io_dot_abapgit ) TO rt_graph.
  ENDMETHOD.

  METHOD zif_abapgit_dependency_manager~resolve_dependencies.

  ENDMETHOD.

  METHOD get_requirements.
    DATA: lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt.
    FIELD-SYMBOLS: <ls_requirement> LIKE LINE OF lt_requirements.

    lt_requirements = io_dot_abapgit->get_data( )-requirements.

    LOOP AT lt_requirements ASSIGNING <ls_requirement>.
*      INSERT INITIAL LINE INTO rt
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
