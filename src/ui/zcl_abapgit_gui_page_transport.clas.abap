"! GUI - Transport overview page
CLASS zcl_abapgit_gui_page_transport DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_abapgit_gui_page
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_action,
        jump_all_transports TYPE string VALUE 'jump_all_transports',
        create_branch       TYPE string VALUE 'create_branch',
        commit_transport    TYPE string VALUE 'commit_transport',
        revert_transport    TYPE string VALUE 'revert_transport',
      END OF c_action.

    METHODS:
      constructor IMPORTING io_repo TYPE REF TO zcl_abapgit_repo_online
                  RAISING   zcx_abapgit_exception,
      zif_abapgit_gui_page~on_event REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION.
  PRIVATE SECTION.
    METHODS:
      render_list RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html,
      render_actions RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html,
      create_branch_for_transport IMPORTING iv_transport             TYPE trkorr
                                  RETURNING VALUE(rv_created_branch) TYPE string
                                  RAISING   zcx_abapgit_exception
                                            zcx_abapgit_cancel,
      get_branch_for_transport IMPORTING iv_transport     TYPE trkorr
                                         iv_only_existing TYPE abap_bool DEFAULT abap_true
                               RETURNING VALUE(rv_branch) TYPE string.
    DATA:
      mo_repo            TYPE REF TO zcl_abapgit_repo_online,
      mi_cts_api         TYPE REF TO zif_abapgit_cts_api,
      mi_branch_finder   TYPE REF TO zif_abapgit_cts_branch_finder,
      mv_experimental_on TYPE abap_bool.
ENDCLASS.



CLASS zcl_abapgit_gui_page_transport IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    mo_repo   = io_repo.
    ms_control-page_title = 'TRANSPORT_REQUESTS'.
    mi_cts_api = zcl_abapgit_factory=>get_cts_api( ).

    mv_experimental_on = zcl_abapgit_persist_settings=>get_instance( )->read( )->get_experimental_features( ).

    IF mv_experimental_on = abap_true.
      mi_branch_finder = zcl_abapgit_factory=>get_cts_branch_finder( ).
    ENDIF.
  ENDMETHOD.

  METHOD render_content.
    CREATE OBJECT ro_html.

    ro_html->add( '<div class="repo">' ).
    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top( mo_repo ) ).
    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_js_error_banner( ) ).
    DATA(lv_package) = mo_repo->get_package( ).
    IF mi_cts_api->is_chrec_possible_for_package( lv_package ) = abap_false.
      ro_html->add( |<span>Warning: Change recording is not possible for package { lv_package }.</span>| ).
    ENDIF.

    ro_html->add( '<div class="transport-container">' ).

    ro_html->add( render_actions( ) ).
    ro_html->add( render_list( ) ).
    ro_html->add( '</div>' ).

    ro_html->add( '</div>' ).
  ENDMETHOD.

  METHOD zif_abapgit_gui_page~on_event.
    CASE iv_action.
      WHEN c_action-create_branch.
        DATA(lv_branch) = create_branch_for_transport( CONV #( iv_getdata ) ).
        MESSAGE |Created branch '{ lv_branch }'.| TYPE 'S'.
        ev_state = zif_abapgit_definitions=>c_event_state-re_render.
      WHEN c_action-commit_transport.

      WHEN c_action-revert_transport.
    ENDCASE.
  ENDMETHOD.

  METHOD render_list.
    TYPES: lty_line_kind TYPE c LENGTH 1,
           BEGIN OF lty_list_line,
             kind TYPE string.
        INCLUDE TYPE zif_abapgit_cts_api=>gty_request_header AS request.
    TYPES:
      branch      TYPE string,
      object_type TYPE trobjtype,
      object_name TYPE sobj_name,
      END OF lty_list_line.
    CONSTANTS: BEGIN OF lc_line_kinds,
                 transport TYPE lty_line_kind VALUE 'R',
                 task      TYPE lty_line_kind VALUE 'T',
                 item      TYPE lty_line_kind VALUE 'I',
               END OF lc_line_kinds.
    DATA: lt_list                TYPE STANDARD TABLE OF lty_list_line,
          lt_objects             TYPE zif_abapgit_cts_api=>gty_object_tab,
          lt_object_with_tr_sort TYPE STANDARD TABLE OF zif_abapgit_cts_api=>gty_object_transport,
          lv_last_transport      TYPE trkorr,
          ls_object              LIKE LINE OF lt_objects,
          lv_column_count        TYPE i.
    FIELD-SYMBOLS: <ls_line>           LIKE LINE OF lt_list,
                   <ls_file>           TYPE zif_abapgit_definitions=>ty_file_item,
                   <ls_object_with_tr> TYPE zif_abapgit_cts_api=>gty_object_transport.

    CREATE OBJECT ro_html.

    ro_html->add( '<table class="stage_tab w100">' ).
    ro_html->add( '<thead><tr><th>Transport request</th><th>Description</th><th>Owner</th><th>Last changed</th>' &&
                  '<th>Target</th>' ).
    lv_column_count = 5.
    IF mv_experimental_on = abap_true.
      ro_html->add( '<th>Branch</th><th>Actions</th>' ).
      lv_column_count = 7.
    ENDIF.
    ro_html->add( '</thead>' ).

    LOOP AT mo_repo->get_files_local( ) ASSIGNING <ls_file>.
      IF <ls_file>-item IS INITIAL.
        CONTINUE.
      ENDIF.
      ls_object-program_id = 'R3TR'.
      ls_object-object_type = <ls_file>-item-obj_type.
      ls_object-object_name = <ls_file>-item-obj_name.
      INSERT ls_object INTO TABLE lt_objects.
      CLEAR ls_object.
    ENDLOOP.

    lt_object_with_tr_sort = mi_cts_api->get_current_trs_for_objs( lt_objects ).
    SORT lt_object_with_tr_sort BY transport object_type object_name.

    LOOP AT lt_object_with_tr_sort ASSIGNING <ls_object_with_tr> WHERE transport IS NOT INITIAL.
      IF lv_last_transport <> <ls_object_with_tr>-transport.
        APPEND INITIAL LINE TO lt_list ASSIGNING <ls_line>.
        lv_last_transport = <ls_object_with_tr>-transport.
        <ls_line>-kind = lc_line_kinds-transport.
        <ls_line>-request = mi_cts_api->get_request_header( <ls_object_with_tr>-transport ).
        <ls_line>-branch = get_branch_for_transport( <ls_object_with_tr>-transport ).
      ENDIF.

      APPEND INITIAL LINE TO lt_list ASSIGNING <ls_line>.
      <ls_line>-kind = lc_line_kinds-item.
      <ls_line>-transport = <ls_object_with_tr>-transport.
      <ls_line>-object_type = <ls_object_with_tr>-object_type.
      <ls_line>-object_name = <ls_object_with_tr>-object_name.
    ENDLOOP.


    ro_html->add( '<tbody>' ).
    LOOP AT lt_list ASSIGNING <ls_line>.
      CASE <ls_line>-kind.
        WHEN lc_line_kinds-transport.
          ro_html->add( |<tr id="tr{ <ls_line>-transport }"><td>| ).
          ro_html->add_a( iv_txt = zcl_abapgit_html=>icon( iv_name = 'unfold' iv_hint = 'Expand' )
                          iv_act = |toggleTransportChildren('{ <ls_line>-transport }')|
                          iv_typ = zif_abapgit_definitions=>c_action_type-onclick ).
          ro_html->add_a( iv_txt = |{ <ls_line>-transport }|
                          iv_act = |{ zif_abapgit_definitions=>c_action-jump_transport }?{ <ls_line>-transport }| ).
          ro_html->add( |</td><td>{ <ls_line>-text }</td>| &&
                        |<td>{ <ls_line>-owner }</td><td>{ <ls_line>-changed_date DATE = USER } | &&
                        |{ <ls_line>-changed_time TIME = USER }</td><td>{ <ls_line>-target_system }| &&
                        |{ COND #( WHEN <ls_line>-target_client IS NOT INITIAL THEN '.' ) }| &&
                        |{ <ls_line>-target_client }</td>| ).
          IF mv_experimental_on = abap_true.
            ro_html->add( |<td>{ <ls_line>-branch }</td><td>| ).
            IF <ls_line>-branch IS INITIAL.
              ro_html->add_a( iv_txt   = 'Create Branch'
                              iv_act   = |{ c_action-create_branch }?{ <ls_line>-transport }| ).
            ELSE.
              ro_html->add_a( iv_txt   = 'Commit'
                              iv_act   = |{ c_action-commit_transport }?{ <ls_line>-transport }| ).
              ro_html->add_a( iv_txt   = 'Revert'
                              iv_act   = |{ c_action-revert_transport }?{ <ls_line>-transport }| ).
            ENDIF.
          ENDIF.
          ro_html->add( |</tr>| ).
        WHEN lc_line_kinds-item.
          ro_html->add( |<tr class="{ <ls_line>-transport }"><td colspan="{ lv_column_count }">| &&
                        |{ <ls_line>-object_type } | &&
                        |{ <ls_line>-object_name }</td></tr>| ).
      ENDCASE.
    ENDLOOP.
    ro_html->add( '</tbody>' ).
    ro_html->add( '</table>' ).
  ENDMETHOD.

  METHOD render_actions.
    CREATE OBJECT ro_html.

    ##TODO.

*    ro_html->add( '<table class="w100 margin-v5"><tr>' ).
*
*    ro_html->add( '<td class="indent5em">' ).
*    ro_html->add_a( iv_act = |{ c_action-jump_all_transports }|
*                    iv_id  = 'jumpAllTransportsButton'
*                    iv_txt = 'Show all transport requests for package' ).
*    ro_html->add( '</td>' ).
*
*    ro_html->add( '</tr>' ).
*    ro_html->add( '</table>' ).
  ENDMETHOD.

  METHOD create_branch_for_transport.
    DATA(lv_current_branch) = mo_repo->get_branch_name( ).
    DATA(lv_new_branch) = get_branch_for_transport( iv_transport     = iv_transport
                                                    iv_only_existing = abap_false ).
    IF lv_new_branch IS INITIAL.
      zcx_abapgit_exception=>raise( 'Could not determine branch name for transport.' ).
    ENDIF.

    DATA(lv_answer) = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar      = 'Continue?'
      iv_text_question = |A new branch '{ lv_new_branch }' will be created based on 'master'. Continue?|
    ).

    IF lv_answer <> '1'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    " Not sure how to get the SHA1 of master here for iv_from
    mo_repo->set_branch_name( zcl_abapgit_git_branch_list=>complete_heads_branch_name( 'master' ) ) ##TODO.
    mo_repo->create_branch( iv_name = zcl_abapgit_git_branch_list=>complete_heads_branch_name( lv_new_branch ) ).
*    iv_from = CONV #( zcl_abapgit_git_branch_list=>complete_heads_branch_name( 'master' ) ) )

    mo_repo->set_branch_name( lv_current_branch ).

    rv_created_branch = lv_new_branch.
  ENDMETHOD.

  METHOD get_branch_for_transport.
    rv_branch = mi_branch_finder->get_branch_for_transport(
      iv_transport = iv_transport
      iv_strategy  = zif_abapgit_cts_branch_finder=>gc_strategies-transport ).
    DATA(li_branch_overview) = zcl_abapgit_factory=>get_branch_overview( mo_repo ).
    DATA(lt_branches) = li_branch_overview->get_branches( ).

    IF iv_only_existing = abap_true.
      READ TABLE lt_branches WITH KEY
        name = zcl_abapgit_git_branch_list=>complete_heads_branch_name( rv_branch ) TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR rv_branch.
      ENDIF.
    ENDIF.

    "mo_repo->get_dot_abapgit( )->get_cts_branching_strategy( )
  ENDMETHOD.
ENDCLASS.
