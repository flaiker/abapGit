CLASS zcl_abapgit_repo_content_list DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_repo TYPE REF TO zcl_abapgit_repo.

    METHODS list
      IMPORTING iv_path              TYPE string
                iv_by_folders        TYPE abap_bool
                iv_changes_only      TYPE abap_bool
      RETURNING VALUE(rt_repo_items) TYPE zif_abapgit_definitions=>tt_repo_items
      RAISING   zcx_abapgit_exception.

    METHODS list_by_transport
      IMPORTING iv_target_branch     TYPE string DEFAULT 'master'
      RETURNING VALUE(rt_repo_items) TYPE zif_abapgit_definitions=>tt_repo_items
      RAISING   zcx_abapgit_exception.

    METHODS get_log
      RETURNING VALUE(ri_log) TYPE REF TO zif_abapgit_log.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_sortkey,
                 default    TYPE i VALUE 9999,
                 parent_dir TYPE i VALUE 0,
                 dir        TYPE i VALUE 1,
                 orphan     TYPE i VALUE 2,
                 changed    TYPE i VALUE 3,
                 inactive   TYPE i VALUE 4,
               END OF c_sortkey.

    DATA: mo_repo TYPE REF TO zcl_abapgit_repo,
          mi_log  TYPE REF TO zif_abapgit_log.

    METHODS build_repo_items_local_only
      RETURNING VALUE(rt_repo_items) TYPE zif_abapgit_definitions=>tt_repo_items
      RAISING   zcx_abapgit_exception.

    METHODS build_repo_items_with_remote
      RETURNING VALUE(rt_repo_items) TYPE zif_abapgit_definitions=>tt_repo_items
      RAISING   zcx_abapgit_exception.

    METHODS build_folders
      IMPORTING iv_cur_dir    TYPE string
      CHANGING  ct_repo_items TYPE zif_abapgit_definitions=>tt_repo_items
      RAISING   zcx_abapgit_exception.

    METHODS filter_changes
      CHANGING ct_repo_items TYPE zif_abapgit_definitions=>tt_repo_items.
ENDCLASS.



CLASS zcl_abapgit_repo_content_list IMPLEMENTATION.


  METHOD build_folders.

    DATA: lv_index    TYPE i,
          lt_subitems LIKE ct_repo_items,
          ls_subitem  LIKE LINE OF ct_repo_items,
          ls_folder   LIKE LINE OF ct_repo_items.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF ct_repo_items.


    LOOP AT ct_repo_items ASSIGNING <ls_item>.
      lv_index = sy-tabix.
      CHECK <ls_item>-path <> iv_cur_dir. " files in target dir - just leave them be

      IF zcl_abapgit_path=>is_subdir( iv_path = <ls_item>-path  iv_parent = iv_cur_dir ) = abap_true.
        ls_subitem-changes = <ls_item>-changes.
        ls_subitem-path    = <ls_item>-path.
        ls_subitem-lstate  = <ls_item>-lstate.
        ls_subitem-rstate  = <ls_item>-rstate.
        APPEND ls_subitem TO lt_subitems.
      ENDIF.

      DELETE ct_repo_items INDEX lv_index.
    ENDLOOP.

    SORT lt_subitems BY path ASCENDING.

    LOOP AT lt_subitems ASSIGNING <ls_item>.
      AT NEW path.
        CLEAR ls_folder.
        ls_folder-path    = <ls_item>-path.
        ls_folder-sortkey = c_sortkey-dir. " Directory
        ls_folder-is_dir  = abap_true.
      ENDAT.

      ls_folder-changes = ls_folder-changes + <ls_item>-changes.

      zcl_abapgit_state=>reduce( EXPORTING iv_cur = <ls_item>-lstate
                                 CHANGING cv_prev = ls_folder-lstate ).
      zcl_abapgit_state=>reduce( EXPORTING iv_cur = <ls_item>-rstate
                                 CHANGING cv_prev = ls_folder-rstate ).

      AT END OF path.
        APPEND ls_folder TO ct_repo_items.
      ENDAT.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_repo_items_local_only.

    DATA: lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt,
          ls_item  TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS: <ls_repo_item> LIKE LINE OF rt_repo_items,
                   <ls_tadir>     LIKE LINE OF lt_tadir.


    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
      iv_package = mo_repo->get_package( )
      io_dot     = mo_repo->get_dot_abapgit( ) ).

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      APPEND INITIAL LINE TO rt_repo_items ASSIGNING <ls_repo_item>.
      <ls_repo_item>-obj_type = <ls_tadir>-object.
      <ls_repo_item>-obj_name = <ls_tadir>-obj_name.
      <ls_repo_item>-path     = <ls_tadir>-path.
      MOVE-CORRESPONDING <ls_repo_item> TO ls_item.
      <ls_repo_item>-inactive = boolc( zcl_abapgit_objects=>is_active( ls_item ) = abap_false ).
      IF <ls_repo_item>-inactive = abap_true.
        <ls_repo_item>-sortkey = c_sortkey-inactive.
      ELSE.
        <ls_repo_item>-sortkey  = c_sortkey-default.      " Default sort key
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_repo_items_with_remote.

    DATA:
      ls_file   TYPE zif_abapgit_definitions=>ty_repo_file,
      lt_status TYPE zif_abapgit_definitions=>ty_results_tt.

    FIELD-SYMBOLS: <ls_status>    LIKE LINE OF lt_status,
                   <ls_repo_item> LIKE LINE OF rt_repo_items.


    lt_status = mo_repo->status( mi_log ).

    LOOP AT lt_status ASSIGNING <ls_status>.
      AT NEW obj_name. "obj_type + obj_name
        APPEND INITIAL LINE TO rt_repo_items ASSIGNING <ls_repo_item>.
        <ls_repo_item>-obj_type = <ls_status>-obj_type.
        <ls_repo_item>-obj_name = <ls_status>-obj_name.
        <ls_repo_item>-inactive = <ls_status>-inactive.
        <ls_repo_item>-sortkey  = c_sortkey-default. " Default sort key
        <ls_repo_item>-changes  = 0.
        <ls_repo_item>-path     = <ls_status>-path.
      ENDAT.

      IF <ls_status>-filename IS NOT INITIAL.
        ls_file-path       = <ls_status>-path.
        ls_file-filename   = <ls_status>-filename.
        ls_file-is_changed = boolc( <ls_status>-match = abap_false ). " TODO refactor
        ls_file-rstate     = <ls_status>-rstate.
        ls_file-lstate     = <ls_status>-lstate.
        APPEND ls_file TO <ls_repo_item>-files.

        IF <ls_status>-inactive = abap_true AND
           <ls_repo_item>-sortkey > c_sortkey-changed.
          <ls_repo_item>-sortkey = c_sortkey-inactive.
        ENDIF.

        IF ls_file-is_changed = abap_true.
          <ls_repo_item>-sortkey = c_sortkey-changed. " Changed files
          <ls_repo_item>-changes = <ls_repo_item>-changes + 1.

          zcl_abapgit_state=>reduce( EXPORTING iv_cur = ls_file-lstate
                                     CHANGING cv_prev = <ls_repo_item>-lstate ).
          zcl_abapgit_state=>reduce( EXPORTING iv_cur = ls_file-rstate
                                     CHANGING cv_prev = <ls_repo_item>-rstate ).
        ENDIF.
      ENDIF.

      AT END OF obj_name. "obj_type + obj_name
        IF <ls_repo_item>-obj_type IS INITIAL.
          <ls_repo_item>-sortkey = c_sortkey-orphan. "Virtual objects
        ENDIF.
      ENDAT.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    mo_repo = io_repo.
    CREATE OBJECT mi_log TYPE zcl_abapgit_log.
  ENDMETHOD.


  METHOD filter_changes.

    DELETE ct_repo_items WHERE changes = 0.

  ENDMETHOD.


  METHOD get_log.
    ri_log = mi_log.
  ENDMETHOD.


  METHOD list.

    mi_log->clear( ).

    IF mo_repo->has_remote_source( ) = abap_true.
      rt_repo_items = build_repo_items_with_remote( ).
    ELSE.
      rt_repo_items = build_repo_items_local_only( ).
    ENDIF.

    IF iv_by_folders = abap_true.
      build_folders(
        EXPORTING iv_cur_dir    = iv_path
        CHANGING  ct_repo_items = rt_repo_items ).
    ENDIF.

    IF iv_changes_only = abap_true.
      " There are never changes for offline repositories
      filter_changes( CHANGING ct_repo_items = rt_repo_items ).
    ENDIF.

    SORT rt_repo_items BY
      sortkey ASCENDING
      obj_type ASCENDING
      obj_name ASCENDING.

  ENDMETHOD.

  METHOD list_by_transport.
    DATA: lo_repo_online     TYPE REF TO zcl_abapgit_repo_online,
          ls_item            TYPE zif_abapgit_definitions=>ty_item,
          lt_objects         TYPE zif_abapgit_cts_api=>gty_object_tab,
          lt_objects_with_tr TYPE zif_abapgit_cts_api=>gty_object_transport_tab.

    mi_log->clear( ).

    IF mo_repo->has_remote_source( ) = abap_false.
      mi_log->add_error( 'Transport overview is not supported for offline projects.' ).
      RETURN.
    ENDIF.

    lo_repo_online ?= mo_repo.

    DATA(lv_previous_branch) = lo_repo_online->get_branch_name( ).

    DATA(li_cts) = zcl_abapgit_factory=>get_cts_api( ).
    DATA(lt_files) = mo_repo->get_files_local( ii_log = mi_log ).
    LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<ls_file>).
      IF <ls_file>-item-obj_type = ls_item-obj_type AND <ls_file>-item-obj_name = ls_item-obj_name.
        CONTINUE.
      ENDIF.
      ls_item = <ls_file>-item.
      APPEND VALUE #( program_id = 'R3TR' object_type = ls_item-obj_type object_name = ls_item-obj_name ) TO lt_objects.
    ENDLOOP.
    lt_objects_with_tr = li_cts->get_current_trs_for_objs( lt_objects ).

    DATA(lv_target_branch) = zcl_abapgit_git_branch_list=>complete_heads_branch_name( iv_target_branch ).

    LOOP AT lt_objects_with_tr ASSIGNING FIELD-SYMBOL(<ls_object>)
                               WHERE transport IS NOT INITIAL
                               GROUP BY <ls_object>-transport
                               INTO DATA(lv_transport).
      APPEND VALUE zif_abapgit_definitions=>ty_repo_item(
        sortkey      = c_sortkey-default
        path         = lv_transport
        is_transport = abap_true
        transport    = lv_transport
        changes      = 0
      ) TO rt_repo_items REFERENCE INTO DATA(lr_transport).

      DATA(ls_request_header) = li_cts->get_request_header( lv_transport ).
      lr_transport->tr_owner = ls_request_header-owner.
      lr_transport->tr_description = ls_request_header-text.
      lr_transport->tr_target = ls_request_header-target_system &&
                                |{ COND #( WHEN ls_request_header-target_client IS NOT INITIAL
                                           THEN |.{ ls_request_header-target_client }| ) }|.

      " Assume branch name = transport request number for now TODO
      DATA(lv_branch_name) = zcl_abapgit_git_branch_list=>complete_heads_branch_name( to_upper( lv_transport ) ).
      LOOP AT zcl_abapgit_factory=>get_branch_overview( lo_repo_online )->get_branches( )
              TRANSPORTING NO FIELDS
              WHERE name = lv_branch_name.
        EXIT.
      ENDLOOP.

      IF sy-subrc = 0.
        " A branch exists for this transport, show uncommitted changes to this branch
        lo_repo_online->set_branch_name( lv_branch_name ).
        lr_transport->tr_branch = to_upper( lv_transport ).
      ELSE.
        " A branch does not exist for this transport, show all changes compared to target branch (~=master)
        lo_repo_online->set_branch_name( lv_target_branch ).
        lr_transport->tr_branch = space.
      ENDIF.

      " This calculates the whole status, should be filtered by objects in the transport instead TODO
      DATA(lt_status) = lo_repo_online->status( mi_log ).

      LOOP AT GROUP lv_transport ASSIGNING <ls_object>.
        APPEND VALUE zif_abapgit_definitions=>ty_repo_item(
          obj_type = <ls_object>-object_type
          obj_name = <ls_object>-object_name
          sortkey  = c_sortkey-default
*          path     = <ls_object>-
          changes  = 0
        ) TO rt_repo_items REFERENCE INTO DATA(lr_item).

        LOOP AT lt_status ASSIGNING FIELD-SYMBOL(<ls_object_status>)
                          WHERE obj_type = <ls_object>-object_type
                            AND obj_name = <ls_object>-object_name.
          APPEND VALUE #(
            path       = <ls_object_status>-path
            filename   = <ls_object_status>-filename
            is_changed = boolc( <ls_object_status>-match = abap_false )
            rstate     = <ls_object_status>-rstate
            lstate     = <ls_object_status>-lstate
          ) TO lr_item->files REFERENCE INTO DATA(lr_file).

          IF lr_item->inactive = abap_false AND <ls_object_status>-inactive = abap_true.
            lr_item->inactive = abap_true.
          ENDIF.

          IF lr_file->is_changed = abap_true.
            lr_item->changes = lr_item->changes + 1.
            lr_transport->changes = lr_transport->changes + 1.
            zcl_abapgit_state=>reduce( EXPORTING iv_cur  = <ls_object_status>-lstate
                                       CHANGING  cv_prev = lr_item->lstate ).
            zcl_abapgit_state=>reduce( EXPORTING iv_cur  = <ls_object_status>-rstate
                                       CHANGING  cv_prev = lr_item->rstate ).
            zcl_abapgit_state=>reduce( EXPORTING iv_cur  = <ls_object_status>-lstate
                                       CHANGING  cv_prev = lr_transport->lstate ).
            zcl_abapgit_state=>reduce( EXPORTING iv_cur  = <ls_object_status>-rstate
                                       CHANGING  cv_prev = lr_transport->rstate ).
          ENDIF.
        ENDLOOP.

        IF sy-subrc <> 0.
          mi_log->add_warning( |{ <ls_object>-object_type } { <ls_object>-object_name }: Could not determine status.| ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF lo_repo_online->get_branch_name( ) <> lv_previous_branch.
      lo_repo_online->set_branch_name( lv_previous_branch ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
