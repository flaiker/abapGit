*&---------------------------------------------------------------------*
*& Include zabapgit_callbacks
*&---------------------------------------------------------------------*

"! Callback interface for abapGit repository events
"! <p>
"! To subscribe to abapGit repository events create a preferably global class in your repository
"! and <em>copy</em> the method signatures of this interface. You only need to copy the ones you
"! want to subscribe to. Specify the name of the class (full name for local classes) in the
"! .abapgit.xml attribute <em>CALLBACK_CLASSNAME</em>.
"! </p>
"! <p>
"! This interface is intentionally defined locally. If it were global users might be inclined to
"! instead of copying the interface's methods to just implement it in their callback class. Since
"! abapGit most of the time only exists in the development system this would cause a compiler error
"! on import in QA / production systems.
"! </p>
INTERFACE lif_callback_listener.
  METHODS:
    "! Deserialize event listener
    "! <p>
    "! Will be called after deserialization and activation of repository objects. Activation of all
    "! objects in the repository is not guaranteed.
    "! </p>
    "! @parameter iv_package | Installation package name
    on_after_deserialize IMPORTING iv_package TYPE devclass,
    "! Uninstall event listener
    "! <p>
    "! This method is called just before a repository gets uninstalled.
    "! </p>
    "! @parameter iv_package | Installation package name
    on_before_uninstall IMPORTING iv_package TYPE devclass.
ENDINTERFACE.

"! Dummy callback listener
CLASS lcl_dummy_callback_listener DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      lif_callback_listener.
    ALIASES:
      on_after_deserialize FOR lif_callback_listener~on_after_deserialize,
      on_before_uninstall FOR lif_callback_listener~on_before_uninstall.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_dummy_callback_listener IMPLEMENTATION.
  METHOD lif_callback_listener~on_after_deserialize.
  ENDMETHOD.

  METHOD lif_callback_listener~on_before_uninstall.
  ENDMETHOD.
ENDCLASS.

"! Callback adapter
"! <p>
"! Adapter class to encapsulate the triggering of callback events. Either redirects a callback
"! event to an empty dummy implementation of <em>LIF_CALLBACK_LISTENER</em> or to a concrete class
"! in the repository.
"! </p>
"! <p>
"! Method calls are <em>upward compatible</em>, meaning that parameters that are added to the
"! callback interface method definition at a later point and were not copied to the callback class
"! implementation will not be supplied / used at all in the callback method call.
"! </p>
CLASS lcl_callback_adapter DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES:
      lif_callback_listener.
    ALIASES:
      on_after_deserialize FOR lif_callback_listener~on_after_deserialize,
      on_before_uninstall FOR lif_callback_listener~on_before_uninstall.
    CONSTANTS:
      BEGIN OF gc_methnames,
        on_after_deserialize TYPE abap_methname VALUE 'ON_AFTER_DESERIALIZE',
        on_before_uninstall  TYPE abap_methname VALUE 'ON_BEFORE_UNINSTALL',
      END OF gc_methnames.
    CLASS-METHODS:
      "! Gets an adapter instance (used for callback class instance reuse / caching)
      "! @parameter io_repo | Repository
      "! @parameter iv_force_new | Force new instance
      "! @parameter ro_instance | Instance
      "! @raising zcx_abapgit_exception | Callback class initialization error
      get_instance IMPORTING io_repo            TYPE REF TO lcl_repo
                             iv_force_new       TYPE abap_bool DEFAULT abap_false
                   RETURNING VALUE(ro_instance) TYPE REF TO lcl_callback_adapter
                   RAISING   zcx_abapgit_exception.
    METHODS:
      "! Check if a callback is allowed to be executed
      "! <p>
      "! See <em>GC_METHNAMES</em> for method name constants.<br/>
      "! <strong>Contains a SAP GUI dialog!</strong>
      "! </p>
      "! @parameter iv_methname | Callback method name
      "! @parameter rv_allowed | Execution is allowed
      "! @raising zcx_abapgit_exception | Dialog error
      check_execution_allowed IMPORTING iv_methname       TYPE abap_methname
                              RETURNING VALUE(rv_allowed) TYPE abap_bool
                              RAISING   zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      dyn_call_method IMPORTING io_object     TYPE REF TO object
                                iv_methname   TYPE abap_methname
                                it_parameters TYPE abap_parmbind_tab,
      get_callback_intf_descr RETURNING VALUE(ro_descr) TYPE REF TO cl_abap_intfdescr.
    METHODS:
      constructor IMPORTING io_repo TYPE REF TO lcl_repo
                  RAISING   zcx_abapgit_exception,
      init_listener RAISING cx_sy_create_object_error,
      is_dummy_listener RETURNING VALUE(rv_is_dummy) TYPE abap_bool,
      check_listener_impl_method IMPORTING iv_methname           TYPE abap_methname
                                 RETURNING VALUE(rv_implemented) TYPE abap_bool,
      check_listener_methimp_has_par IMPORTING iv_methname          TYPE abap_methname
                                               iv_parmname          TYPE abap_parmname
                                     RETURNING VALUE(rv_par_exists) TYPE abap_bool.
    DATA:
      mo_repository         TYPE REF TO lcl_repo,
      mo_listener           TYPE REF TO object,
      mv_callback_classname TYPE abap_classname,
      mo_listener_descr     TYPE REF TO cl_abap_classdescr.
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

    ASSERT io_repo IS BOUND.
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
    mv_callback_classname = mo_repository->get_dot_abapgit( )->get_callback_classname( ).

    IF mv_callback_classname IS NOT INITIAL.
      CREATE OBJECT mo_listener TYPE (mv_callback_classname).
    ELSE.
      CREATE OBJECT mo_listener TYPE lcl_dummy_callback_listener.
    ENDIF.

    ASSERT mo_listener IS BOUND.
    mo_listener_descr ?= cl_abap_typedescr=>describe_by_object_ref( mo_listener ).
    ASSERT mo_listener_descr IS BOUND.
  ENDMETHOD.

  METHOD lif_callback_listener~on_after_deserialize.
    CONSTANTS: lc_parmname_package     TYPE abap_parmname VALUE 'IV_PACKAGE'.
    DATA: lt_parameters TYPE abap_parmbind_tab,
          ls_parameter  TYPE abap_parmbind,
          lr_package    TYPE REF TO devclass.

    CREATE DATA: lr_package.

    IF check_listener_methimp_has_par( iv_methname = gc_methnames-on_after_deserialize
                                       iv_parmname = lc_parmname_package ).
      ls_parameter-name = lc_parmname_package.
      ls_parameter-kind = cl_abap_objectdescr=>exporting.
      lr_package->* = iv_package.
      ls_parameter-value = lr_package.
      INSERT ls_parameter INTO TABLE lt_parameters.
    ENDIF.

    dyn_call_method( io_object     = mo_listener
                     iv_methname   = gc_methnames-on_after_deserialize
                     it_parameters = lt_parameters ).
  ENDMETHOD.

  METHOD lif_callback_listener~on_before_uninstall.
    CONSTANTS: lc_parmname_package TYPE abap_parmname VALUE 'IV_PACKAGE'.
    DATA: lt_parameters TYPE abap_parmbind_tab,
          ls_parameter  TYPE abap_parmbind,
          lr_package    TYPE REF TO devclass.

    CREATE DATA: lr_package.

    IF check_listener_methimp_has_par( iv_methname = gc_methnames-on_before_uninstall
                                       iv_parmname = lc_parmname_package ).
      ls_parameter-name = lc_parmname_package.
      ls_parameter-kind = cl_abap_objectdescr=>exporting.
      lr_package->* = iv_package.
      ls_parameter-value = lr_package.
      INSERT ls_parameter INTO TABLE lt_parameters.
    ENDIF.

    dyn_call_method( io_object     = mo_listener
                     iv_methname   = gc_methnames-on_before_uninstall
                     it_parameters = lt_parameters ).
  ENDMETHOD.

  METHOD check_execution_allowed.
    IF is_dummy_listener( ) = abap_true.
      " Dummy listener is always allowed because it doesn't do anything
      rv_allowed = abap_true.
      RETURN.
    ENDIF.

    IF check_listener_impl_method( iv_methname ) = abap_false.
      " Listener does not implement the callback method (because it was added later or the listener
      " is not interested in this type of callback). -> No execution
      rv_allowed = abap_false.
      RETURN.
    ENDIF.

    " Prevent arbitrary code execution by allowing the user to take a look at the (possibly just
    " pulled) callback implementation.
    rv_allowed = lcl_popups=>popup_to_decide_callback_exec(
                   iv_methname           = iv_methname
                   iv_callback_classname = mv_callback_classname ).
  ENDMETHOD.

  METHOD is_dummy_listener.
    DATA: lo_dummy       TYPE REF TO lcl_dummy_callback_listener ##NEEDED,
          lo_class_descr TYPE REF TO cl_abap_classdescr,
          lo_ref_descr   TYPE REF TO cl_abap_refdescr.

    lo_ref_descr ?= cl_abap_typedescr=>describe_by_data( lo_dummy ).
    lo_class_descr ?= lo_ref_descr->get_referenced_type( ).
    rv_is_dummy = lo_class_descr->applies_to( mo_listener ).
  ENDMETHOD.

  METHOD check_listener_impl_method.
    rv_implemented = abap_false.

    READ TABLE mo_listener_descr->methods WITH KEY name = iv_methname
                                          TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      rv_implemented = abap_true.
    ELSE.
      READ TABLE mo_listener_descr->methods
           WITH KEY name = |{ get_callback_intf_descr( )->get_relative_name( ) }~{ iv_methname }|
           TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        rv_implemented = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD check_listener_methimp_has_par.
    DATA: lo_intf_descr TYPE REF TO cl_abap_intfdescr.
    FIELD-SYMBOLS: <ls_method>        TYPE abap_methdescr,
                   <ls_parameter>     TYPE abap_parmdescr,
                   <ls_method_def>    TYPE abap_methdescr,
                   <ls_parameter_def> TYPE abap_parmdescr.

    " Method must exist, check with check_listener_impl_method
    READ TABLE mo_listener_descr->methods WITH KEY name = iv_methname ASSIGNING <ls_method>.
    ASSERT sy-subrc = 0.

    LOOP AT <ls_method>-parameters ASSIGNING <ls_parameter> WHERE name = iv_parmname.
      " Get parameter definition
      lo_intf_descr = get_callback_intf_descr( ).
      READ TABLE lo_intf_descr->methods WITH KEY name = iv_methname ASSIGNING <ls_method_def>.
      ASSERT sy-subrc = 0.
      READ TABLE <ls_method_def>-parameters WITH KEY name = iv_parmname
                                            ASSIGNING <ls_parameter_def>.
      ASSERT sy-subrc = 0.

      " Check if parameters are of the same kind (-> at least somewhat compatible).
      " All other parameter type mismatches will raise a dynamic call exception, which seems better
      " than just hiding the error by not calling the callback at all.
      rv_par_exists = boolc( <ls_parameter>-parm_kind = <ls_parameter_def>-parm_kind ).

      EXIT.
    ENDLOOP.

    IF sy-subrc <> 0.
      " Parameter does not exist
      rv_par_exists = abap_false.
    ENDIF.
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

  METHOD get_callback_intf_descr.
    DATA: li_dummy     TYPE REF TO lif_callback_listener ##NEEDED,
          lo_ref_descr TYPE REF TO cl_abap_refdescr.
    STATICS: so_intf_descr TYPE REF TO cl_abap_intfdescr.

    IF so_intf_descr IS NOT BOUND.
      lo_ref_descr ?= cl_abap_typedescr=>describe_by_data( li_dummy ).
      so_intf_descr ?= lo_ref_descr->get_referenced_type( ).
    ENDIF.

    ASSERT so_intf_descr IS BOUND.
    ro_descr = so_intf_descr.
  ENDMETHOD.
ENDCLASS.