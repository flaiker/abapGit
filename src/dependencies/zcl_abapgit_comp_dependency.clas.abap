"! Dependency to software component
CLASS zcl_abapgit_comp_dependency DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_dependency.
    CONSTANTS:
      gc_dependency_type TYPE zif_abapgit_dependency=>gty_dependency_type VALUE 'COMP'.
    DATA:
      mv_component   TYPE zif_abapgit_dot_abapgit=>ty_requirement-component,
      mv_min_release TYPE zif_abapgit_dot_abapgit=>ty_requirement-min_release,
      mv_min_patch   TYPE zif_abapgit_dot_abapgit=>ty_requirement-min_patch.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_comp_dependency IMPLEMENTATION.
  METHOD zif_abapgit_dependency~get_dependency_type.
    rv_type = gc_dependency_type.
  ENDMETHOD.

  METHOD zif_abapgit_dependency~get_relative_name.
    rv_name = mv_component.
    IF mv_min_release IS NOT INITIAL.
      rv_name = |{ rv_name }@{ mv_min_release }|.
    ENDIF.
    IF mv_min_patch IS NOT INITIAL.
      rv_name = |{ rv_name }-{ mv_min_patch }|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_dependency~get_unique_name.
    rv_name = |{ gc_dependency_type }={ zif_abapgit_dependency~get_relative_name( ) }|.
  ENDMETHOD.
ENDCLASS.
