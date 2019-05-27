CLASS zcl_abapgit_gui_asset_manager DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_asset_manager.

    TYPES:
      BEGIN OF ty_asset_entry.
        INCLUDE TYPE zif_abapgit_gui_asset_manager~ty_web_asset.
    TYPES: mime_name TYPE wwwdatatab-objid,
           END OF ty_asset_entry ,
           tt_asset_register TYPE STANDARD TABLE OF ty_asset_entry WITH KEY url.

    METHODS register_asset
      IMPORTING
        !iv_url       TYPE string
        !iv_type      TYPE string
        !iv_cachable  TYPE abap_bool DEFAULT abap_true
        !iv_mime_name TYPE wwwdatatab-objid OPTIONAL
        !iv_base64    TYPE string OPTIONAL
        !iv_inline    TYPE string OPTIONAL .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_asset_register TYPE tt_asset_register.

    METHODS get_mime_asset
      IMPORTING
        iv_mime_name    TYPE c
      RETURNING
        VALUE(rv_xdata) TYPE xstring
      RAISING
        zcx_abapgit_exception.

    METHODS load_asset
      IMPORTING
        is_asset_entry  TYPE ty_asset_entry
      RETURNING
        VALUE(rs_asset) TYPE zif_abapgit_gui_asset_manager~ty_web_asset
      RAISING
        zcx_abapgit_exception.

    METHODS adjust_css_for_theme
      CHANGING
        cv_content TYPE xstring.

ENDCLASS.



CLASS zcl_abapgit_gui_asset_manager IMPLEMENTATION.


  METHOD get_mime_asset.

    DATA: ls_key    TYPE wwwdatatab,
          lv_size_c TYPE wwwparams-value,
          lv_size   TYPE i,
          lt_w3mime TYPE STANDARD TABLE OF w3mime.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_mime_name.

    " Get exact file size
    CALL FUNCTION 'WWWPARAMS_READ'
      EXPORTING
        relid            = ls_key-relid
        objid            = ls_key-objid
        name             = 'filesize'
      IMPORTING
        value            = lv_size_c
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_size = lv_size_c.

    " Get binary data
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    rv_xdata = zcl_abapgit_convert=>bintab_to_xstring(
      iv_size   = lv_size
      it_bintab = lt_w3mime ).

  ENDMETHOD.


  METHOD load_asset.

    MOVE-CORRESPONDING is_asset_entry TO rs_asset.
    IF rs_asset-content IS INITIAL AND is_asset_entry-mime_name IS NOT INITIAL.
      " inline content has the priority
      rs_asset-content = get_mime_asset( is_asset_entry-mime_name ).
    ENDIF.
    IF rs_asset-content IS INITIAL.
      zcx_abapgit_exception=>raise( |failed to load GUI asset: { is_asset_entry-url }| ).
    ENDIF.

    IF rs_asset-url = 'css/common.css'.
      adjust_css_for_theme( CHANGING cv_content = rs_asset-content ).
    ENDIF.

  ENDMETHOD.


  METHOD register_asset.

    DATA ls_asset LIKE LINE OF mt_asset_register.

    SPLIT iv_type AT '/' INTO ls_asset-type ls_asset-subtype.
    ls_asset-url          = iv_url.
    ls_asset-mime_name    = iv_mime_name.
    ls_asset-is_cacheable = iv_cachable.
    IF iv_base64 IS NOT INITIAL.
      ls_asset-content = zcl_abapgit_convert=>base64_to_xstring( iv_base64 ).
    ELSEIF iv_inline IS NOT INITIAL.
      ls_asset-content = zcl_abapgit_convert=>string_to_xstring( iv_inline ).
    ENDIF.

    APPEND ls_asset TO mt_asset_register.

  ENDMETHOD.


  METHOD zif_abapgit_gui_asset_manager~get_all_assets.

    FIELD-SYMBOLS <ls_a> LIKE LINE OF mt_asset_register.

    LOOP AT mt_asset_register ASSIGNING <ls_a>.
      APPEND load_asset( <ls_a> ) TO rt_assets.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_gui_asset_manager~get_asset.

    FIELD-SYMBOLS <ls_a> LIKE LINE OF mt_asset_register.

    READ TABLE mt_asset_register WITH KEY url = iv_url ASSIGNING <ls_a>.
    IF <ls_a> IS NOT ASSIGNED.
      zcx_abapgit_exception=>raise( |Cannot find GUI asset: { iv_url }| ).
    ENDIF.
    rs_asset = load_asset( <ls_a> ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_asset_manager~get_text_asset.

    DATA ls_asset TYPE zif_abapgit_gui_asset_manager~ty_web_asset.
    ls_asset = me->zif_abapgit_gui_asset_manager~get_asset( iv_url ).

    rv_asset = zcl_abapgit_convert=>xstring_to_string_utf8( ls_asset-content ).

  ENDMETHOD.

  METHOD adjust_css_for_theme.
    CONSTANTS: lc_var_pattern TYPE string VALUE `var\((\-\-.*)\)`.
    DATA: lv_content       TYPE string,
          lt_content_tab   TYPE string_table,
          lv_variable_name TYPE string,
          lv_offset        TYPE i,
          lv_length        TYPE i,
          lv_new_value     TYPE string.
    FIELD-SYMBOLS: <lv_line> LIKE LINE OF lt_content_tab.

    lv_content = zcl_abapgit_convert=>xstring_to_string_utf8( cv_content ).
    SPLIT lv_content AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_content_tab.

    LOOP AT lt_content_tab ASSIGNING <lv_line>.
      IF <lv_line> CS 'var('.
        FIND FIRST OCCURRENCE OF REGEX lc_var_pattern
             IN <lv_line>
             SUBMATCHES lv_variable_name
             MATCH OFFSET lv_offset
             MATCH LENGTH lv_length.
        IF sy-subrc = 0 AND lv_variable_name IS NOT INITIAL.
          CASE lv_variable_name.
            WHEN '--theme-background-color'.
              lv_new_value = '#eff4f9'.
            WHEN '--theme-accent-color'.
              lv_new_value = '#fcfdfe'.
            WHEN '--theme-primary-font'.
              lv_new_value = '"72", Arial, Helvetica, sans-serif'.
            WHEN '--theme-monospace-font'.
              lv_new_value = 'Lucida Console, Consolas, Courier, monospace'.
          ENDCASE.
          IF lv_new_value IS NOT INITIAL.
            REPLACE SECTION OFFSET lv_offset LENGTH lv_length OF <lv_line> WITH lv_new_value.
            CLEAR lv_new_value.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    cv_content = zcl_abapgit_convert=>string_to_xstring_utf8(
      concat_lines_of( table = lt_content_tab sep = cl_abap_char_utilities=>cr_lf ) ).
  ENDMETHOD.
ENDCLASS.
