*&---------------------------------------------------------------------*
*& Include zabapgit_callbacks
*&---------------------------------------------------------------------*

INTERFACE lif_callback_listener.
  METHODS:
    on_after_install IMPORTING iv_package     TYPE devclass
                               iv_old_version TYPE string
                               iv_new_version TYPE string.
ENDINTERFACE.

CLASS lcl_dummy_callback_listener DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      lif_callback_listener.
    ALIASES:
      on_after_install FOR lif_callback_listener~on_after_install.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_dummy_callback_listener IMPLEMENTATION.
  METHOD lif_callback_listener~on_after_install.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_callback_adapter DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES:
      lif_callback_listener.
    ALIASES:
      on_after_install FOR lif_callback_listener~on_after_install.
    CLASS-METHODS:
      get_instance IMPORTING io_repo            TYPE REF TO lcl_repo
                             iv_force_new       TYPE abap_bool DEFAULT abap_false
                   RETURNING VALUE(ro_instance) TYPE REF TO lcl_callback_adapter
                   RAISING   zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      dyn_call_method IMPORTING io_object     TYPE REF TO object
                                iv_methname   TYPE abap_methname
                                it_parameters TYPE abap_parmbind_tab.
    METHODS:
      constructor IMPORTING io_repo TYPE REF TO lcl_repo
                  RAISING   zcx_abapgit_exception,
      init_listener RAISING cx_sy_create_object_error.
    DATA:
      mo_repository TYPE REF TO lcl_repo,
      mo_listener   TYPE REF TO object.
ENDCLASS.

CLASS lcl_callback_adapter IMPLEMENTATION.
  METHOD get_instance.
    TYPES: BEGIN OF lty_cache,
             key      TYPE lcl_persistence_db=>ty_value,
             instance TYPE REF TO lcl_callback_adapter,
           END OF lty_cache.
    STATICS: st_cache TYPE SORTED TABLE OF lty_cache WITH UNIQUE KEY key.
    DATA: lr_cache TYPE REF TO lty_cache.

    ASSERT io_repo IS BOUND AND io_repo->get_key( ) IS NOT INITIAL.

    IF iv_force_new = abap_false.
      READ TABLE st_cache WITH TABLE KEY key = io_repo->get_key( ) REFERENCE INTO lr_cache.
      IF sy-subrc = 0 AND lr_cache IS BOUND.
        ro_instance = lr_cache->instance.
        ASSERT ro_instance IS BOUND.
        FREE lr_cache.
      ENDIF.
    ENDIF.

    IF ro_instance IS NOT BOUND.
      CREATE OBJECT ro_instance
        EXPORTING
          io_repo = io_repo.

      CREATE DATA lr_cache.
      lr_cache->instance = ro_instance.
      lr_cache->key = io_repo->get_key( ).

      TRY.
          INSERT lr_cache->* INTO TABLE st_cache.
        CATCH cx_sy_itab_duplicate_key ##NO_HANDLER.
          " Can occur if iv_force_new was set to true
      ENDTRY.
      FREE lr_cache.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    DATA: lx_ex TYPE REF TO cx_sy_create_object_error.

    ASSERT: io_repo IS BOUND.
    mo_repository = io_repo.

    TRY.
        init_listener( ).
      CATCH cx_sy_create_object_error INTO lx_ex.
        RAISE EXCEPTION TYPE zcx_abapgit_exception
          EXPORTING
            previous = lx_ex
            text     = lx_ex->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD init_listener.
    DATA: lv_classname TYPE abap_classname.

    ##TODO.
*    lv_classname = mo_repository->get_dot_abapgit( )->get_callback_classname( ).

    IF lv_classname IS NOT INITIAL.
      CREATE OBJECT mo_listener TYPE (lv_classname).
    ELSE.
      CREATE OBJECT mo_listener TYPE lcl_dummy_callback_listener.
    ENDIF.
  ENDMETHOD.

  METHOD lif_callback_listener~on_after_install.
    CONSTANTS: lc_methname             TYPE abap_methname VALUE 'ON_AFTER_INSTALL',
               lc_parmname_package     TYPE abap_parmname VALUE 'IV_PACKAGE',
               lc_parmname_old_version TYPE abap_parmname VALUE 'IV_OLD_VERSION',
               lc_parmname_new_version TYPE abap_parmname VALUE 'IV_NEW_VERSION'.
    DATA: lt_parameters  TYPE abap_parmbind_tab,
          ls_parameter   TYPE abap_parmbind,
          lr_package     TYPE REF TO devclass,
          lr_old_version TYPE REF TO string,
          lr_new_version TYPE REF TO string.

    CREATE DATA: lr_package, lr_old_version, lr_new_version.

    ls_parameter-name = lc_parmname_package.
    ls_parameter-kind = cl_abap_objectdescr=>exporting.
    lr_package->* = iv_package.
    ls_parameter-value = lr_package.
    INSERT ls_parameter INTO TABLE lt_parameters.

    ls_parameter-name = lc_parmname_old_version.
    ls_parameter-kind = cl_abap_objectdescr=>exporting.
    lr_old_version->* = iv_old_version.
    ls_parameter-value = lr_old_version.
    INSERT ls_parameter INTO TABLE lt_parameters.

    ls_parameter-name = lc_parmname_new_version.
    ls_parameter-kind = cl_abap_objectdescr=>exporting.
    lr_new_version->* = iv_new_version.
    ls_parameter-value = lr_old_version.
    INSERT ls_parameter INTO TABLE lt_parameters.

    dyn_call_method( io_object     = mo_listener
                     iv_methname   = lc_methname
                     it_parameters = lt_parameters ).
  ENDMETHOD.

  METHOD dyn_call_method.
    DATA: lx_ex TYPE REF TO cx_sy_dyn_call_error.

    ASSERT: io_object IS BOUND,
            iv_methname IS NOT INITIAL.

    TRY.
        CALL METHOD io_object->(iv_methname) PARAMETER-TABLE it_parameters.
      CATCH cx_sy_dyn_call_error INTO lx_ex.
        " If a short dump occurs here the listener object does not implement the callback methods
        " correctly, see LIF_CALLBACK_LISTENER for the method signatures.
        RAISE EXCEPTION lx_ex.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
