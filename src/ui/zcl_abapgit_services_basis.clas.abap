CLASS zcl_abapgit_services_basis DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS create_package
      IMPORTING
        iv_prefill_package TYPE devclass OPTIONAL
      RETURNING
        VALUE(rv_package)  TYPE devclass
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS test_changed_by
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS run_performance_test
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_services_basis IMPLEMENTATION.


  METHOD create_package.

    DATA ls_package_data TYPE scompkdtln.
    DATA lv_create       TYPE abap_bool.
    DATA li_popup        TYPE REF TO zif_abapgit_popups.

    ls_package_data-devclass = to_upper( iv_prefill_package ).

    li_popup = zcl_abapgit_ui_factory=>get_popups( ).

    li_popup->popup_to_create_package(
      IMPORTING
        es_package_data = ls_package_data
        ev_create       = lv_create ).

    IF lv_create = abap_true.
      zcl_abapgit_factory=>get_sap_package( ls_package_data-devclass )->create( ls_package_data ).
      rv_package = ls_package_data-devclass.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD test_changed_by.

    DATA ls_tadir TYPE zif_abapgit_definitions=>ty_tadir.
    DATA ls_item  TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_user  TYPE xubname.

    ls_tadir = zcl_abapgit_ui_factory=>get_popups( )->popup_object( ).
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    ls_item-obj_type = ls_tadir-object.
    ls_item-obj_name = ls_tadir-obj_name.

    lv_user = zcl_abapgit_objects=>changed_by( ls_item ).

    MESSAGE lv_user TYPE 'S'.

  ENDMETHOD.

  METHOD run_performance_test.
    DATA: lo_performance                TYPE REF TO zcl_abapgit_performance_test,
          lv_package                    TYPE devclass,
          lv_include_sub_packages       TYPE abap_bool VALUE abap_true,
          lv_serialize_master_lang_only TYPE abap_bool VALUE abap_true,
          lt_object_type_filter         TYPE zcl_abapgit_performance_test=>gty_object_type_range,
          lt_object_name_filter         TYPE zcl_abapgit_performance_test=>gty_object_name_range,
          lt_result                     TYPE zcl_abapgit_performance_test=>gty_result_tab,
          lo_alv                        TYPE REF TO cl_salv_table,
          lx_salv_error                 TYPE REF TO cx_salv_error,
          lv_current_repo               TYPE zif_abapgit_persistence=>ty_value.

    TRY.
        lv_current_repo = zcl_abapgit_persistence_user=>get_instance( )->get_repo_show( ).
        IF lv_current_repo IS NOT INITIAL.
          lv_package = zcl_abapgit_repo_srv=>get_instance( )->get( lv_current_repo )->get_package( ).
        ENDIF.
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

    zcl_abapgit_ui_factory=>get_popups( )->popup_perf_test_parameters(
      IMPORTING
        et_object_type_filter         = lt_object_type_filter
        et_object_name_filter         = lt_object_name_filter
      CHANGING
        cv_package                    = lv_package
        cv_include_sub_packages       = lv_include_sub_packages
        cv_serialize_master_lang_only = lv_serialize_master_lang_only ).

    CREATE OBJECT lo_performance
      EXPORTING
        iv_package                    = lv_package
        iv_include_sub_packages       = lv_include_sub_packages
        iv_serialize_master_lang_only = lv_serialize_master_lang_only.


    lo_performance->set_object_type_filter( lt_object_type_filter ).
    lo_performance->set_object_name_filter( lt_object_name_filter ).

    lo_performance->run_measurement( ).

    lt_result = lo_performance->get_result( ).

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = lt_result ).
        lo_alv->get_functions( )->set_all( ).
        lo_alv->get_display_settings( )->set_list_header( 'abapGit - Performance Test' ).
        lo_alv->get_columns( )->get_column( 'RUNTIME' )->set_medium_text( 'Runtime' ).
        lo_alv->get_columns( )->get_column( 'SECONDS' )->set_medium_text( 'Seconds' ).
        lo_alv->get_columns( )->set_count_column( 'COUNTER' ).
        lo_alv->get_aggregations( )->add_aggregation( 'RUNTIME' ).
        lo_alv->get_aggregations( )->add_aggregation( 'SECONDS' ).
        lo_alv->set_screen_popup(
          start_column = 1
          end_column   = 180
          start_line   = 1
          end_line     = 25 ).
        lo_alv->display( ).
      CATCH cx_salv_error INTO lx_salv_error.
        zcx_abapgit_exception=>raise(
          iv_text     = lx_salv_error->get_text( )
          ix_previous = lx_salv_error ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
